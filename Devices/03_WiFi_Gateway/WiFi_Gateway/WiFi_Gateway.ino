/*
 * Sketch   WiFi_Gateway.ino
 * Target   Wemos D1 mini on top of HBus WiFi Gateway 
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

#include <coos.h>  // https://github.com/akouz/a_coos
#include "HBus.h"	

//##############################################################################
// Def
//##############################################################################

const char* ssid     = "your_ssid";
const char* password = "your_password";
const char* mqtt_url = "your_mqtt_broker";
const uint  mqtt_port = 1883;
const char* mqtt_password = "mqtt_password";
const char* topic_root = "HBus";    // must be less than 30 chars

String NodeName;  // HBus_GW_XXXX

//##############################################################################
// Descriptors
//##############################################################################

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
const uchar node_descr[8] = {
2,  // device type
1,  // device model
1,  // h/w rev major
0,  // h/w rev minor
0,  // boot rev major
1,  // boot rev minor
1,  // sketch rev major
0   // sketch rev minor         Rev 1.0
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
// Send messages to MQTT broker at least every 5 sec to keep connection alive
// ========================================
void coos_task_mqtt_ping(void)
{
    while(1)
    {
        COOS_DELAY(100);
        if (++ping_cnt >= 50)
        {
            ping_cnt = 0;
            if ((WiFi.status() == WL_CONNECTED) && (MqttClient.state() == 0))
            {
                MqttClient.publish(topic_root, NodeName.c_str()); // "HBus", "HBus_GW_XXXX"     
            }         
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
    static uchar ti = 0;    // topic index    
    static uchar topic_id_refresh = 250;
    COOS_DELAY(5000);                                   // initial pause 5 sec 
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
            if ((HBmqtt.flag[ti].value_valid) && (ownTopicId[ti]))  // broadcast only valid values
            {                
               //  HBmqtt.make_msg_pub(ti); // prepare MQTT message with topic value,
                                         // then it will be automatically transmitted
            }    
            ++ti = (ti >= MAX_TOPIC)? 0 : ti;  // next topic
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
                        MqttClient.publish(topic_root, NodeName.c_str()); // "HBus", "HBus_GW_XXXX"
                        // ... and resubscribe
                        char txt[0x20];
                        uchar len = strlen(topic_root);
                        if (len < 30)
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
                        HBmqtt.make_msg_time(tmp); // PUBLISH, topic="time"
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
    if (strcmp(topic, topic_root)) // ignore messages to topic_root
    {
//        Serial.print("MQTT message, topic=");
//        Serial.print(topic);
//        Serial.print(", content=");
//        Serial.print((char*)payload);
//        Serial.print(", len=");
//        Serial.print(len);
        // ---------------------------------
        // detect echo message by distinctive signature {....} 00 03 04
        // ---------------------------------
        if ((payload[0] == '{') && 
            (payload[len-1] == 4) &&
            (payload[len-2] == 3) &&
            (payload[len-3] == 0) &&
            (payload[len-4] == '}'))             
        {
//            Serial.print(", echo suppressed ");
        }
        // ---------------------------------
        // if it is original message from external source
        // ---------------------------------
        else
        {
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

// ========================================
// Setup
// ========================================
void setup()
{
    Serial.begin(19200);
    Serial.println();
//    Serial.println("Serial started");
//    Serial.println();
//    delay(1000);
    EEPROM.begin(0x1000); // EEPROM size 4K bytes max
    SPIFFS.begin();

    pinMode(LED, OUTPUT);
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
    coos.register_task(coos_task_broadcast); 
    coos.register_task(coos_task_reconnect);            // re-connect to WiFi and to Cloud (MQTT)
    coos.register_task(coos_task_mqtt_ping);            // keep cloud connection alive
    coos.register_task(coos_task_NTP);                  // time service
    // init registered tasks
    coos.start();     
    
}

// ========================================
// Main loop 
// ========================================
void loop()
{  
    coos.run();           // Coos scheduler 
    MqttClient.loop();    // MQTT
    ESP.wdtFeed();
}

/* EOF */