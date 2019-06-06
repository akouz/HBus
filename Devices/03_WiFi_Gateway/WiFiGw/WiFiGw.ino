/*
 *          *****************
 *          HBus WiFi Gateway
 *          *****************
 * Sketch   WiFiGw.ino
 * Target   Wemos D1 mini on HBus WiFi Gateway board  
 *          - optional 128x32 OLED display driven by SSD1306 controller
 *          - optional BMP280/BME280 sensor
 *          - optional MH-Z19B sensor 
 * Memory   flash 4M,  SPIFFS 1M

 * (c) 2019 Alex Kouznetsov,  https://github.com/akouz/hbus
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

//##############################################################################
// Inc
//##############################################################################

#include <coos.h>               // https://github.com/akouz/a_coos  rev 1.5
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>   // https://github.com/adafruit/Adafruit_SSD1306
#include <BMx280MI.h>           // https://bitbucket.org/christandlg/bmx280mi/src/master/
#include "HBus.h"
//#include "data\credentials.h"	

//##############################################################################
// Def
//##############################################################################

#ifndef __MY_CREDENTIALS 
const char* ssid     = "your_ssid";
const char* password = "your_password";

const char* mqtt_url = "your_mqtt_broker";
const char* mqtt_password = "mqtt_password";
#endif

const uint  mqtt_port = 1883;
const char* topic_root = "HBus";    // must be less than 60 bytes

String NodeName;  // "HBus_GW_XXXX"

// -----------------------
// measurements
// -----------------------
struct msr_struct{
    float HBvoltage;
    float temperature;
    float pressure;
    float humidity;
    float CO2;
    union{
        uint all;
        struct{
            unsigned    HBvoltage   : 1;
            unsigned    temperature : 1;
            unsigned    pressure    : 1;
            unsigned    humidity    : 1;
            unsigned    CO2         : 1;
            unsigned    time        : 1;
            unsigned    bmx280      : 1;        
        };
    }valid;        
} msr;


// -----------------------------------
// Declarations for an SSD1306 display connected to I2C (SDA, SCL pins)
// -----------------------------------
#define SCREEN_WIDTH    128 // OLED display width, in pixels
#define SCREEN_HEIGHT   32  // OLED display height, in pixels
#define OLED_RESET      -1  // Reset pin -1 if sharing Arduino reset pin
// GND to COM 
// Vcc to 3.3V
// SCL to D1 (GPIO5) - Wemos pin 14
// SDA to D2 (GPIO4) - Wemos pin 13

// -----------------------------------
// BMx280 sensor connected to I2C (SDA, SCL pins)
// -----------------------------------
#define BMx280_I2C_ADDRESS 0x76
// GND to COM 
// Vcc to 3.3V
// SCL to D1 (GPIO5) - Wemos pin 14
// SDA to D2 (GPIO4) - Wemos pin 13

// -----------------------------------
// MH-Z19B sensor PWM output connected to GPIO13 
// -----------------------------------
#define CO2_pulse   13

// -----------------------------------
// Testpoint
// -----------------------------------
#define TESTPOINT   16

//##############################################################################
// Descriptors
//##############################################################################

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
const uchar node_descr[8] = {
2,              // device type
1,              // device model
1,              // h/w rev major
0,              // h/w rev minor
0,              // boot rev major
1,              // boot rev minor
SW_REV_MAJ,     // sketch rev major
SW_REV_MIN      // sketch rev minor       
};

//##############################################################################
// Var
//##############################################################################

WiFiClient MqttBrokerClient;
PubSubClient MqttClient(MqttBrokerClient);
uint ping_cnt;    

WiFiUDP ntpUDP;
myNTPClient timeClient(ntpUDP);

Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);

//create a BMx280I2C object using the I2C interface with I2C Address 0x76
BMx280I2C bmx280(BMx280_I2C_ADDRESS);

//##############################################################################
// Func
//##############################################################################

// =====================================  
// Debug: print buf
// =====================================  
void print_buf(uchar* buf, uchar len)
{    
    if ((buf) && (len))
    {
        Serial.print(" printing buf, len=");        
        Serial.println(len);        
        for (uchar i=0; i<len; i++)
        {
            if (buf[i] < 0x10)
              Serial.print('0');
            Serial.print(buf[i], HEX);
            Serial.print(' ');
            if ((i & 7) == 7)
            {
                Serial.print(' ');
            }
            if ((i & 0x0F) == 0x0F)
            {
                Serial.println();
            }                        
        }
        Serial.println();
    }
}

// =====================================  
// Custom command used for debug
// =====================================  
hb_msg_t* custom_command(hb_msg_t* rxmsg)
{
    int cmd;
    jsonBuf.clear();            
    JsonObject& root = jsonBuf.parseObject(rxmsg->buf+8);
    if (root.success())
    {
        cmd = root["cmd"];
        Serial.print(" cmd=");
        Serial.println(cmd);
        switch(cmd)
        {
        case 1:
            HBnodes.print_nodes();
            break;
        case 2:
            HBtopics.print_topics();
            break;
        default:
            break;    
        }
    }
    return NULL;
}
// ========================================
// OLED display
// ========================================
void coos_task_display(void)
{
    static uint state = 0;
    char buf[20];
    COOS_DELAY(2000); // show initial text for extra 2 sec
    while(1)
    {
        display.clearDisplay();
        display.setCursor(0, 0);
        switch(++state)
        {
        // ---------------------------
        // h/w and s/w revisions
        // ---------------------------
        case 1:  
            display.print("hw ");
            display.print(node_descr[2]);                        
            display.print(".");
            display.println(node_descr[3]);
            COOS_DELAY(10);                        
            display.print("sw ");
            display.print(node_descr[6]);                        
            display.print(".");
            display.println(node_descr[7]);     
            break;                   
        // ---------------------------
        // nodes and topics
        // ---------------------------
        case 2:  
            display.print(HBnodes.node_list_len);
            if (HBnodes.node_list_len == 1)
            {
                display.println(" node");
            }
            else 
            {
                display.println(" nodes");
            } 
            COOS_DELAY(10);                        
            display.print(HBtopics.def_topic_cnt);      // number of default topics
            display.print("+");
            display.print(HBtopics.topic_list_len);     // number of registered topics
            display.println(" topics");
            break;            
        // ---------------------------
        // connections
        // ---------------------------
        case 3:  
            if (WiFi.status() == WL_CONNECTED)
            {
                display.println("WiFi OK");
            }
            else 
            {
                display.println("no WiFi");
            }
            if (MqttClient.connected())
            {
                display.println("MQTT OK");
            }
            else 
            {
                display.println("no MQTT");
            }            
            break;             
        // ---------------------------
        // time, hh:mm 
        // ---------------------------
        case 4:
            if (msr.valid.time)
            {
                display.println("Time:");
                sprintf(buf,"%02d:%02d", coos.hour(), coos.minute());
                display.println(buf);                
            }
            else
                state++;   
        // ---------------------------
        // bus voltage
        // ---------------------------
        case 5:  
            display.println("Supply:");
            if (msr.valid.HBvoltage)
            {
                display.print(msr.HBvoltage);
                display.println(" V");            
            }
            else
            {
                display.println("unknown");            
            }
            break;
        // ---------------------------
        // MH-Z19B - CO2 level
        // ---------------------------
        case 6:
            if (msr.valid.CO2)
            {
                display.println("CO2, ppm:");
                display.println(msr.CO2);
                break;
            }
            else
                state++;  
        // ---------------------------
        // BMx280 - temperature
        // ---------------------------
        case 7:  
            if (msr.valid.temperature)
            {
                display.println("Temp:");
                display.print(msr.temperature);
                display.println(" C");            
            }
            else
                state = 0;
            break;
        // ---------------------------
        // BMx280 - pressure
        // ---------------------------
        case 8:  
            if (msr.valid.pressure)
            {
                display.println("Pressure:");
                display.println(msr.pressure);
            }
            else
                state = 0;
            break;
        // ---------------------------
        // BME280 - humidity
        // ---------------------------
        case 9:  
            if (msr.valid.humidity)
            {
                display.println("Humidity:");
                display.print(msr.humidity);
                display.println(" %");            
            }
            else
                state = 0;
            break;
        // ---------------------------
        default:
            state = 0;
            break;
        }
        display.display();
        if (state)
        {
            COOS_DELAY(4000);  // show text for 4 sec 
        }
        else
        {
            COOS_DELAY(500);  // pause 0.5 sec 
        }
        
    }
}
// ========================================
// Measurements - CO2 by MH-Z19B
// ========================================
void coos_task_msr_CO2(void)
{
    static uint state = 0;
    static uint pwm_cnt;
    static uint tmout = 0;
    pinMode(CO2_pulse, INPUT);
    COOS_DELAY(30000);   // initial delay  to warm up sensor
    COOS_DELAY(30000);  
    while(1)
    {
        COOS_DELAY(1);
        tmout++;
        switch(state)
        {
        case 0:
            if (digitalRead(CO2_pulse) == LOW)
            {
                pwm_cnt = 0;
                state++;
            }
            else if (tmout > 2000)
            {                
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;                                    
            }
            break;
        case 1:
        case 2:
            if (digitalRead(CO2_pulse) == HIGH)
            {
                state++;
            }
            else if (tmout > 2000)
            {                
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;
                state = 0;                                    
            }
            break;
        case 3:
            if (tmout > 2000)
            {
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;                                    
                state = 0;                                    
            }
            else
            {
                if (digitalRead(CO2_pulse) == HIGH)
                {
                    pwm_cnt++;
                }
                else
                {
                    state = 0;
                    tmout = 0;
                    msr.CO2 = 2.048 * pwm_cnt;  // because was measured in 1.024 ms ticks
                    msr.valid.CO2 = 1;   
                    HBmqtt.value[5] = msr.CO2;
                    HBmqtt.valid[5].value = msr.valid.CO2;   
                    COOS_DELAY(10000);  // pause 10 sec 
                }
            }        
            break;
        default:
            state = 0;
            tmout = 0;
            break;
        }
    }
}
// ========================================
// Measurements - HBus voltage and BMx280
// ========================================
void coos_task_msr(void)
{
    static uint state = 0;    
    int tmp;
    if (msr.valid.bmx280)
    {
        bmx280.measure();   
    }   
    COOS_DELAY(1000);   // initial delay 1 sec
    while(1)
    {
        switch(++state)
        {
        // --------------------------------
        // measure HBus voltage
        // --------------------------------
        case 1:
            tmp = analogRead(A0);
            // additional attenuator 33k/4.7k, coeff  8.0213
            msr.HBvoltage = 8.0213 * (tmp * (3.2 / 1024.0));
            msr.valid.HBvoltage = 1;
            HBmqtt.value[1] = msr.HBvoltage;
            HBmqtt.valid[1].value = 1; 
            break;
        // --------------------------------
        // get BMx280 results
        // --------------------------------
        case 2:
            if (!msr.valid.bmx280)
            {
                uchar bmpID = bmx280.readID();
                if ((bmpID == 0x58) || (bmpID == 0x60))
                {
//                    Serial.println("BMx280 connected");
                    msr.valid.bmx280 = 1;
                }
                COOS_DELAY(1); 
            }        
            if (msr.valid.bmx280)
            {
                if (bmx280.hasValue())
                {
                    COOS_DELAY(1); 
                    msr.temperature = bmx280.getTemperature();      // C
                    msr.valid.temperature = 1;
                    COOS_DELAY(1); 
                    msr.pressure = bmx280.getPressure()/100;        // mbar  
                    msr.valid.pressure = 1;
                    if (bmx280.isBME280())
                    {
                        COOS_DELAY(1); 
                        msr.humidity = bmx280.getHumidity();        // %
                        msr.valid.humidity = 1;        
                    }                    
                    COOS_DELAY(1); 
                    bmx280.measure();  // start next measurement                     
                    HBmqtt.value[2] = msr.temperature;
                    HBmqtt.valid[2].value = msr.valid.temperature;  // C
                    HBmqtt.value[3] = msr.pressure;
                    HBmqtt.valid[3].value = msr.valid.pressure;     // mbar
                    HBmqtt.value[4] = msr.humidity;
                    HBmqtt.valid[4].value = msr.valid.humidity;     // relative, %
                }
                else
                {
//                    Serial.println("BMx280 disconnected");
                    msr.valid.bmx280 = 0;
                    msr.valid.temperature = 0;
                    msr.valid.pressure = 0;
                    msr.valid.humidity = 0;        
                    HBmqtt.valid[2].value = 0;
                    HBmqtt.valid[3].value = 0;
                    HBmqtt.valid[4].value = 0;
                }
            }
            break;
        default:
            state = 0;
            break;
        }        
        COOS_DELAY(1000);  // pause 1 sec
    }
}
// ========================================
// Send messages to MQTT broker at least every 10 sec to keep connection alive
// ========================================
void coos_task_mqtt_ping(void)
{
    while(1)
    {
        COOS_DELAY(1000);       // 1 sec
        if ((WiFi.status() == WL_CONNECTED) && (MqttClient.state() == 0))
        {
            MqttClient.publish("ping", NodeName.c_str()); // "ping", "HBus_GW_XXXX"
            COOS_DELAY(9000);   // 9 sec     
        }         
    }
}
// ========================================
// Broadcast own topic values to HBus  
// ========================================
// if node has permanent ID (eg if node configured) then every 10 sec 
// check next own topic and broadcast its value  
void coos_task_broadcast(void)
{
    mqtt_msg_t* msg;        // MQTT message
    static uchar idx = 0;   // topic index    
    static uchar topic_id_refresh = 250;
    COOS_DELAY(5000);       // initial pause 5 sec 
    // -------------------------------
    // loop
    // -------------------------------
    while(1)
    {
        if (++topic_id_refresh >= 200)  // after power-up and once in a while
        {
            topic_id_refresh = 0;
            // annonce own topics at HBus                        
            while (HBmqtt.init_topic_id(HBcmd.own.ID) != OK)     
            {
                COOS_DELAY(500);
            }
        } 
        COOS_DELAY(10000);  // pause 10 sec 
        if (HBcmd.own.ID < 0xF000) // if not a temporary ID
        {
            if ((HBmqtt.valid[idx].value) && (ownTopicId[idx]))  // broadcast only valid values
            {                
                msg = HBmqtt.publish_own_val(idx);      // to HBus, also prepare MQTT message
                if ((msg) && (MqttClient.connected()))
                {
                    uchar len = strlen(topic_root); // topic_root to be added to topic
                    if (len < 60) 
                    {
                        char topic[0x100];
                        strcpy(topic, topic_root);
                        topic[len++] = '/';
                        strcpy(topic+len, msg->tpc);
                        MqttClient.publish(topic, (uchar*)msg->pld, msg->pldlen); // publish to MQTT 
                        digitalWrite(TESTPOINT, HIGH);
                        blink(20);                      // flash LED for 200 ms
/*                                                 
                        Serial.print(" published topic=");
                        Serial.print(topic);
                        Serial.print(", payload=");
                        Serial.println(msg->pld);
*/                        
                    }                
                }
                
            }    
            ++idx = (idx >= MAX_TOPIC)? 0 : idx;  // next topic
        }
    }
}
// ========================================
// Reconnect to WiFi and/or to cloud    
// ========================================
void coos_task_reconnect(void)
{
    static uint i;
    while(1)
    {
        COOS_DELAY(1000);
        // -----------------------
        // re-connect to WiFi
        // -----------------------
        if (WiFi.status() != WL_CONNECTED)
        {
            WiFi.begin(ssid, password);
            for (i=0; i<20; i++)
            {
                COOS_DELAY(1000);
                if (WiFi.status() == WL_CONNECTED)
                {
                    break;  // success
                }
            }
        }
        // -----------------------
        // re-connect to cloud
        // -----------------------
        if ((WiFi.status() == WL_CONNECTED) && (!MqttClient.connected()))
        {
            for (i=0; i<20; i++) // try to (re)connect to cloud for 10 sec
            {
                // ----------------------
                // check cloud connection
                // ----------------------
                int state = MqttClient.state();
                if (state) // 0 = connected
                {
                    if (MqttClient.connect(NodeName.c_str()))
                    {
                        // Once connected, publish an announcement...
//                        Serial.println("MQTT broker (re)-connected");
                        MqttClient.publish("ping", NodeName.c_str()); // "ping", "HBus_GW_XXXX"
                        // ... and resubscribe
                        char txt[0x40];
                        uchar len = strlen(topic_root);
                        if (len < 60)
                        {
                            strcpy(txt, topic_root);
                            txt[len++] = '/'; 
                            txt[len++] = '#';  // multilevel wildcard
                            txt[len++] = 0; 
                            MqttClient.subscribe(txt); 
                        }
                        else
                        {
//                            Serial.println(" topic_root string is too long");                            
                        }
                        break;  // success
                    }
                }
                // ----------------------
                // while wating, check WiFi connection
                // ----------------------
                if (WiFi.status() == WL_CONNECTED)
                {
                    COOS_DELAY(500); // wait 0.5 sec
                }
                else 
                {
                    break;  // WiFi connection lost
                }
            } // for 10 sec
        } // if (!CloudClient.connected()
    }
}

// ========================================
// Every hour get time from NTP server    
// ========================================
void coos_task_NTP(void)
{
    mqtt_msg_t* msg;
    static uint cnt_1hr = 3600;
    static uint cnt, i;
    timeClient.begin();
    COOS_DELAY(10000);   // pause 10 sec 
    while(1)
    {
        COOS_DELAY(1000);  // 1.024 sec rate
        // --------------------------
        // approx. every hour
        // --------------------------
       if (++cnt_1hr > (3600))   
        {
            // --------------------
            // try to get NTP time, 3 attempts
            // --------------------
            for (i=0; i<3; i++)            
            {
                timeClient.sendNTPPacket();
                // ----------------
                // for 2 sec check for reply
                // ----------------
                for (cnt=0; cnt<20; cnt++)
                {
                    COOS_DELAY(100); 
                    if (timeClient.checkReply())  // if reply received
                    {
                        ulong tmp = timeClient.getEpochTime();
                        msg = HBmqtt.make_msg_time(tmp); // PUBLISH to HBus, topic="time"
                        if ((msg) && (MqttClient.connected()))
                        {
                            MqttClient.publish(msg->tpc, (uchar*)msg->pld, msg->pldlen); // publish to MQTT broker
                            blink(20);                 // flash LED for 200 ms
                        }
                        msr.valid.time = 1;                        
                        cnt_1hr = 0;    
                        break;
                    }
                }
//                Serial.println();                 
                if (cnt_1hr == 0)
                {
                    break; // success
                }
                else
                {
                    COOS_DELAY(10000); // pause 10 sec, then try again 
                }
            }
            cnt_1hr = 0;  // anyway           
        }         
    }
}
// ========================================
// When a message from MQTT broker received
// ========================================
void Mqtt_callback(char* topic, byte* payload, uint len) 
{
    digitalWrite(TESTPOINT, LOW);   // to measure MQTT round trip 
    if (strcmp(topic, topic_root))  // ignore messages to topic_root
    {
//        print_buf(payload, len);         
        // ---------------------------------
        // detect echo message by distinctive signature {....} 00 03 04
        // ---------------------------------
        if (HBmqtt.is_signature((char*)payload))
        {
//            Serial.println(" MQTT echo suppressed ");
        }
        // ---------------------------------
        // if it is original message from external source
        // ---------------------------------
        else
        {
/*                
            Serial.print(" MQTT message: topic=");
            Serial.print(topic);
            Serial.print(", content=");
            Serial.print((char*)payload);
            Serial.print(", len=");
            Serial.println(len);
*/            
            // remove braces
            if ((payload[0] == '{') && (payload[len-1] == '}'))
            {
                payload++; // remove '{'
                len -= 2;  // remove '}'
                payload[len] = 0; // z-string
            }              
            uchar tlen = strlen((const char*)topic);
            uchar rlen = strlen(topic_root);  // length 4 for "HBus"
            if (tlen > rlen)
            {
                topic += (rlen+1);   // cut off topic_root and slash, aka "HBus/"
                uint idx = HBtopics.is_topic(topic);        // find topic index in the list
                if (idx)                                    // if topic listed
                {
//                  Serial.print(", converted to HBus message, topic index=");
                    Serial.print(idx);                            
                    uint tid = HBtopics.get_topic_id(idx);  // get topic ID
                    if (tid)
                    {
//                        Serial.print(", TopicId=");
//                        Serial.print(tid);            
                        HBmqtt.make_msg_publish(tid, payload, 0);   // cast it to HBus MQTT-SN message, 
                                                                    // treat as string. add '{' and '}' 
                    }
                }
            }  
        }
//        Serial.println();    
    }
}
// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint cnt, uint sd, uint ID)
{
    const char hdr[] =  "=== HBus WiFi Gateway ==="; 
    const char txt1[] = "Power-up cnt = "; 
    const char txt2[] = ", restored seed = ";  
    const char txt3[] = ", node ID = 0x"; 
    const char txt4[] = ", node name = "; 
    Serial.println();
    Serial.printf("\n\nReason for reboot: %s\n", ESP.getResetReason().c_str());
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.println(hdr);
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.print(txt1);
    Serial.print(cnt);    
    Serial.print(txt2);
    Serial.print(sd);
    Serial.print(txt3);
    Serial.print(ID, HEX);
    Serial.print(txt4);
    Serial.println(NodeName);
}

// =============================================================================
// Setup
// =============================================================================
void setup()
{
    Serial.begin(19200);
    Serial.println();
//    Serial.println("Serial started");
//    Serial.println();
//    delay(1000);
    EEPROM.begin(0x1000); // EEPROM size 4K bytes max
    SPIFFS.begin();
    Wire.begin();
    msr.valid.all = 0;

    pinMode(LED, OUTPUT);
    pinMode(TESTPOINT, OUTPUT);
    
    pup_cnt = 0x100*EEPROM.read(EE_PUP_CNT) + EEPROM.read(EE_PUP_CNT+1);  // number of power-ups
    node_seed = 0x100*EEPROM.read(EE_SEED) + EEPROM.read(EE_SEED+1);      
    pup_cnt = (pup_cnt >= 0xFFFE) ? 1 : (pup_cnt+1);

    uchar changed = 0;
    if (pup_cnt < (node_seed | 0xD00D)) 
    {
        EEPROM.write(EE_PUP_CNT, (uchar)(pup_cnt >> 8));
        EEPROM.write(EE_PUP_CNT+1, (uchar)pup_cnt);
        changed |= 1;
    }
    randomSeed(node_seed ^ pup_cnt);    // randomize

    HBcmd.read_own_ID();    // read from EEPROM
    if ((HBcmd.own.ID == 0) || (HBcmd.own.ID >= 0xF000))
    {
        HBcmd.own.ID = 0xF000 | random(0x1000); // then randomize it        
        EEPROM.write(EE_OWN_ID, (uchar)(HBcmd.own.ID >> 8));
        EEPROM.write(EE_OWN_ID+1, (uchar)HBcmd.own.ID);
        changed |= 2;
    }
    if (changed)
    {
        EEPROM.commit();
    }
    HBcmd.set_custom_cmd(custom_command);        
    HBcmd.set_descriptor((uchar*)node_descr);   // set descriptor for REV command
    HBmqtt.read_topic_id();                     // read from EEPROM
    HBnodes.init_node_list();                   // read from EEPROM and sort
    HBtopics.init_topic_list();                 // read from EEPROM and sort 

    // WiFi    
    WiFi.mode(WIFI_STA);            // set station mode
    WiFi.begin(ssid, password);   
    NodeName = "HBus_GW_" + WiFi.macAddress().substring(12,14)+WiFi.macAddress().substring(15,17);
        
    // MQTT broker     
    MqttClient.setServer(mqtt_url, mqtt_port);
    MqttClient.setCallback(Mqtt_callback);
    
    // optional splash screen         
    print_hdr_txt(pup_cnt, node_seed, HBcmd.own.ID);    

    // OLED display
    if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) // Address 0x3C for 128x32 
    { 
        Serial.println("SSD1306 allocation failed");
    }
    display.clearDisplay();
    display.setTextSize(2); // Draw 2X-scale text
    display.setTextColor(WHITE);
    display.setCursor(0, 0);
    display.println(" HBus WiFi");
    display.println("  Gateway");
    display.display();      // Show initial text
    
    // BMx280 sensor
    if (!bmx280.begin())
	{
		Serial.println("BMx280 sensor not found");
	}
    else
    {   
        msr.valid.bmx280 = 1;
        if (bmx280.isBME280())
	       Serial.println("BME280 connected");
	    else
	   	   Serial.println("BMP280 connected");
        bmx280.resetToDefaults();
        bmx280.writeOversamplingPressure(BMx280MI::OSRS_P_x16);
	    bmx280.writeOversamplingTemperature(BMx280MI::OSRS_T_x16);
        if (bmx280.isBME280())
	       bmx280.writeOversamplingHumidity(BMx280MI::OSRS_H_x16);
    }
        
    // Wait for WiFi connection
    for (int i=0; i<20; i++)
    {
        Serial.print(".");
        delay(500);
        if (WiFi.status() == WL_CONNECTED)
        {
            break;
        } 
    }             
    if (WiFi.status() == WL_CONNECTED)
    {
        Serial.print("WiFi connected to ");
        Serial.print(ssid);
        Serial.print(", signal strength ");
        Serial.print(WiFi.RSSI());
        Serial.println(" dBm");
    }
    else
    {
        Serial.print("Unable to connected to ");
        Serial.println(ssid);
    }
    ESP.wdtEnable(1000);
    
    // register COOS tasks
    coos.register_task(coos_task_HBus_rxtx);            // HBus rx/tx task
    coos.register_task(coos_task_tick1ms);              // reqired for proper HBus operation
    coos.register_task(coos_task_msr_CO2);              // CO2 measurement               
    coos.register_task(coos_task_msr);                  // voltage and BMx280 measurements
    coos.register_task(coos_task_broadcast); 
    coos.register_task(coos_task_reconnect);            // re-connect to WiFi and to Cloud (MQTT)
    coos.register_task(coos_task_mqtt_ping);            // keep cloud connection alive
    coos.register_task(coos_task_NTP);                  // time service
    coos.register_task(coos_task_display);              // OLED display
    // init registered tasks
    coos.start();     
}

// =============================================================================
// Main loop 
// =============================================================================
void loop()
{  
    coos.run();           // Coos scheduler 
    MqttClient.loop();    // MQTT
    ESP.wdtFeed();        // watchdog
}

/* EOF */
