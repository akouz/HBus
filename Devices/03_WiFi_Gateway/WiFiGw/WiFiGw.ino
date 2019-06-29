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

#include "HBcommon.h"               
#include "HBus.h"  
#include "HBmsr.h"                  // sensors and OLED display
#include "data\credentials.h"	

//##############################################################################
// Def
//##############################################################################

#ifndef __MY_CREDENTIALS 
const char* ssid     = "your_ssid";
const char* password = "your_password";

const char* mqtt_url = "mqtt_broker_url";
const char* mqtt_username = "";
const char* mqtt_password = "";
#endif

const uint  mqtt_port = 1883;
int   mqtt_was_connected = 0;
const char* topic_root = "HBus";    // must be less than 60 bytes

String NodeName;  // "HBus_GW_XXXX"

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
const uchar node_descr[] = {
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

//##############################################################################
// Func
//##############################################################################

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
//        Serial.print(" cmd=");
//        Serial.println(cmd);
        switch(cmd)
        {
        case 1:
            HBnodes.print_nodes();
            break;
        case 2:
            HBtopics.print_topics();
            break;
        case 3:
            if (msr.valid.roundtrip)
            { 
                Serial.print(" MQTT roundtrip ");
                Serial.print(msr.MQTT_roundtrip);
                Serial.println(" ms");
            }
            else
            {
                Serial.println(" Please wait, MQTT roundtrip not measured yet...");            
            }
            break;
        default:
            break;    
        }
    }
    return NULL;
}
// ========================================
// Send messages to MQTT broker 
// ========================================
void coos_task_mqtt_ping(void)
{
    COOS_DELAY(4000);   // initial pause 4 sec 
    while(1)
    {
        COOS_DELAY(1000);       // 1 sec
        if ((WiFi.status() == WL_CONNECTED) && (MqttClient.state() == 0))
        {
            MqttClient.publish("ping", NodeName.c_str()); // "ping", "HBus_GW_XXXX"
            COOS_DELAY(4000);   // 4 sec     
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
                        msr.MQTT_sent = millis();
                        msr.flag.MQTT_sent = 1;
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
// Common for MQTT connection
// ========================================
bool mqtt_connect(void)
{
    bool res;
    if ((mqtt_username[0]) && (mqtt_password[0])) // is both strings not empty
    {
        res = MqttClient.connect(NodeName.c_str(), mqtt_username, mqtt_password);         
    }
    else  // there is no credentials
    {
        res = MqttClient.connect(NodeName.c_str());  // send only ClientID
    }
    if (res)
    {
        mqtt_was_connected = 3;
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
    }
    else if (mqtt_was_connected)
    {
        mqtt_was_connected--;
    }                             
    return res;    
}

// ========================================
// Reconnect to WiFi and/or to MQTT    
// ========================================
void coos_task_wifi_reconnect(void)
{
    static uint i;
    while(1)
    {
        COOS_DELAY(500);
        // -----------------------
        // re-connect to WiFi
        // -----------------------
        if (WiFi.status() != WL_CONNECTED)
        {
            WiFi.begin(ssid, password);
            for (i=0; i<40; i++) // for 20 sec
            {
                COOS_DELAY(500);
                if (WiFi.status() == WL_CONNECTED)
                {
                    break;  // success
                }
            }
        }
    }
}
// ========================================
// Reconnect to MQTT    
// ========================================
void coos_task_mqtt_reconnect(void)
{
    int wait = (mqtt_was_connected)? 10000 : 30000;
    COOS_DELAY(wait); 
    while(1)
    {
        if ((WiFi.status() == WL_CONNECTED) && (!MqttClient.connected()))
        {
            mqtt_connect();
            wait = (mqtt_was_connected)? 2000 : 30000;
            COOS_DELAY(wait); 
        }
        else
        {
            COOS_DELAY(500);
        }            
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
    // ----------------------------------------
    // roundtrip measurements
    // ----------------------------------------
    digitalWrite(TESTPOINT, LOW);   // to measure MQTT round trip
    if (msr.flag.MQTT_sent)
    {
        msr.flag.MQTT_sent = 0;
        msr.MQTT_roundtrip = (uint)(millis() - msr.MQTT_sent);
        msr.valid.roundtrip = 1; 
    }
    // ----------------------------------------
    // debug
    // ----------------------------------------
    payload[len] = 0;	// make payload a valid 0-terminated text string
//    Serial.print(" MQTT topic: ");  
//    Serial.print(topic);  
//    Serial.print(", payload: ");  
//    Serial.print((char*)payload);  
    // ----------------------------------------
    // ignore messages to topic_root
    // ----------------------------------------
    if (strcmp(topic, topic_root))  
    {
//        print_buf(payload, len);         
        // ---------------------------------
        // detect echo message by distinctive signature
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
            Serial.print(" MQTT topic: ");
            Serial.print(topic);
            Serial.print(", paylad=");
            Serial.print((char*)payload);
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
//                  Serial.print(idx);                            
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
    msr.flag.all = 0;

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
    Serial.print(millis());
    Serial.println(" WiFi begin");
    
    NodeName = "HBus_GW_" + WiFi.macAddress().substring(12,14)+WiFi.macAddress().substring(15,17);
        
    // MQTT broker    
    MqttClient.setServer(mqtt_url, mqtt_port);
    MqttClient.setCallback(Mqtt_callback);
    
    // optional splash screen         
    print_hdr_txt(pup_cnt, node_seed, HBcmd.own.ID);    

            
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
    Serial.print(millis());
    if (WiFi.status() == WL_CONNECTED)
    {
        Serial.print(" WiFi connected to ");
        Serial.print(ssid);
        Serial.print(", signal strength ");
        Serial.print(WiFi.RSSI());
        Serial.println(" dBm");

        // try to connect to MQTT first time
        if (mqtt_connect())         
        {
            Serial.print(millis());
            Serial.print(" Connected to MQTT broker ");
            Serial.println(mqtt_url);
        }
        else
        {
            Serial.print(millis());
            Serial.print(" Cannot connect to MQTT broker ");
            Serial.println(mqtt_url);
        }
    }
    else
    {
        Serial.print(" Unable to connected to ");
        Serial.println(ssid);
    }    
    
    // watchdog
    ESP.wdtEnable(1000);

    // register COOS tasks
    coos.register_task(coos_task_HBus_rxtx);            // HBus rx/tx task
    coos.register_task(coos_task_tick1ms);              // reqired for proper HBus operation
    coos.register_task(coos_task_broadcast); 
    coos.register_task(coos_task_wifi_reconnect);       // re-connect to WiFi 
    coos.register_task(coos_task_mqtt_reconnect);       // re-connect to MQTT
    coos.register_task(coos_task_mqtt_ping);           
    coos.register_task(coos_task_NTP);                  // time service
    coos.register_task(coos_task_msr_CO2);              // CO2 measurement               
    coos.register_task(coos_task_msr_BMx280);           // voltage and BMx280 measurements
    coos.register_task(coos_task_OLED_display);         // OLED display

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