/*
 * File     HBcommon.h 
 * Target   Arduino

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
 
#ifndef __HB_COMMON_H
#define __HB_COMMON_H

//##############################################################################
// Inc
//##############################################################################

#include <Arduino.h>
#include <Wire.h>
#include <EEPROM.h>
#include <stdio.h>
#include <ArduinoJson.h>    // https://github.com/bblanchon/ArduinoJson  rev 5.13.4
#include <coos.h>           // https://github.com/akouz/a_coos  rev 1.5

#include <ESP8266WiFi.h>    // https://github.com/esp8266/Arduino/blob/master/doc/esp8266wifi/readme.md
#include <DNSServer.h>
#include <WiFiUdp.h>
#include <ESP8266WebServer.h>
#include <ESP8266mDNS.h>
#include <WiFiClient.h>
#include <FS.h>
#include "myNTPClient.h"
#include <stdint.h>
#include <functional>
#include <PubSubClient.h> 

//##############################################################################
// History
//##############################################################################

// rev 0.6  -   15/03/2019, refactoring, use installed library <coos.h>, use
//              descriptors for HBcmd and HBmqtt; remove local file revisions
// rev 0.7  -   16/03/2019, report value as 0 (no decimal point) if it is not valid
// rev 0.8  -   29/04/2019, added CMP_TOPIC and REGISTER
// rev 0.9  -   2/05/2019,  WiFi server: changes for ESP8266 (Wemos D1) 
// rev 0.10  -  16/05/2019, HBnodes and HBtopics implemented, default topics 
//              added, coos_task_NTP() publishes "time" topic every hour
// rev 1.0  -   20/05/2019 released to GitHub  
// rev 1.1  -   26/05/2019, added BMx280 and OLED display 128x32    
// rev 1.2  -   28/05/2019, Gateway sends its own topics to MQTT broker too
// rev 1.3  -   30/05/2019, added CO2 sensor MH-Z19B, time displayed on OLED (coos rev 1.5 used)    
// rev 1.4  -   6/06/2019, added HMmqtt.add_signature() and HBmqtt.is_signature()    
// rev 1.5  -   11/06/2019, modified (re)connection to MQTT broker    
// rev 1.6  -   21/06/2019, refactoring
// rev 1.7  -   31/07/2019, byte-stuffing on-fly while transmitting
// rev 1.8  -   5/08/2019, HBcipher added, HBus rev 0.9
// rev 1.9  -   12/08/2019, SET_ID command can change only tmp ID 

#define SW_REV_MAJ  1
#define SW_REV_MIN  9

//##############################################################################
// Def
//##############################################################################


#define LED             2    // GPIO2
// #define TAG(x)          {tag[1] = tag[0]; tag[0] = x;}

enum{

    TIME_ZONE       = 9*60 + 30,     // +9:30, Adelaide

    // HBus revision
    HB_REV_MAJ      = 0,
    HB_REV_MIN      = 10,     

    // coos
    COOS_TASKS      = 16,   // this node uses up to 16 coos tasks            

    // misc
    DF_STATUS       = 1,    // use JSON in STATUS command    
  
    // byte-stuffing
    _ESC            = 0x1B,
    _ESC_START_HB   = 2,
    _ESC_START_MQ   = 3,
    _ESC_START_HBE  = 4,
    _ESC_START_MQE  = 5,
    _ESC_END        = 7,
    _ESC_ESC        = 8,
    _ESC_2ESC       = 9,

    // chars
    CHAR_TAB        = 9,
    CHAR_LF         = 10,
    CHAR_CR         = 13,
    CHAR_SPACE      = 0x20,

    // common constants
    READY           = 1,
    NOT_READY       = 0,
  
//    OK              = 0,
    SKIP            = 2,
    ERR             = 0xEE,
    ERR_BUSY        = 0xE0,
    ERR_PARAM       = 0xE1,
    ERR_ECHO        = 0xE2,   
    ERR_UNKNOWN     = 0xE3,
    ERR_OVERFLOW    = 0xE4,
    ERR_SECURITY    = 0xE5,
    ERR_TMOUT       = 0xE6,   

    // settings
    MAX_BUF         = 0x90,


    // ------------------------------------    
    // EEPROM addresses (max EEPROM size 4K)
    // ------------------------------------    
    EE_PUP_CNT      = 2,    // count power-ups
    EE_SEED         = 4,    // random seed  
    EE_OWN_ID       = 6,    // own NodeId
    EE_TZ           = 10,   // time zone
    EE_SECURITY     = 12,   // access control settings
    EE_SECURITY_INV = 14,   // inverted access control settings 
    EE_XTEA_KEY     = 0x30, // XTEA cipher key, 16 bytes 
    EE_DESCR        = 0x40, // description c-string, up to 64 chars
    EE_TOPIC_ID     = 0x80, // own TopicIds, 2-bytes each, up to 64 topics
    
    // Topics table, up to 256 entries
    EE_NODE_LIST    = 0x0400, // list of known NodeId, 512 x 2 bytes = 1 Kbytes
    EE_TOPIC_LIST   = 0x0800, // list of topics records, 512 x 4 bytes = 2 Kbytes 
// ------------------
//     2    |   2   |         
// ---------|-------|
// TopicId  |  CRC  | 
// ------------------
//  CRC - of TopicName
// TopicNames stored by SPIFFS, each topic in a separate file, file name is
// "Txxxx", where xxxx - hex TopicId 
};

//##############################################################################
// Typedef
//##############################################################################

#ifndef __UCHAR_DEFINED__
  #define __UCHAR_DEFINED__
  typedef unsigned char uchar;
  typedef signed   char schar;
  typedef unsigned int  uint;
  typedef unsigned long ulong;
  typedef signed   long slong;
#endif


#ifndef __HB_MSG_T__
#define __HB_MSG_T__ 
typedef struct{
  uchar buf[MAX_BUF];
  uint  crc;
  uchar len;
  uchar postpone;
  union{
    uchar all;
    struct{
      unsigned gate     : 1;
      unsigned esc      : 1;
      unsigned hb       : 1;
      unsigned encrypt  : 1;      
      unsigned valid    : 1;
      unsigned busy     : 1;
    };
  };
}hb_msg_t;
#endif

#ifndef __MQTT_MSG_T__
#define __MQTT_MSG_T__ 
typedef struct{
  char tpc[MAX_BUF];    // topic
  uint tpclen;
  char pld[MAX_BUF];    // payload
  uint pldlen;  
}mqtt_msg_t;
#endif

 
//##############################################################################
// Var
//##############################################################################

extern uint pup_cnt; 
extern uint node_seed;
extern uint led_cnt;

extern const char* topic_root;  // "HBus", or "HBus1", etc

extern StaticJsonBuffer<256> jsonBuf;
extern Coos <COOS_TASKS, 1> coos;    // 1.024 ms ticks

extern PubSubClient MqttClient;

extern const uchar node_descr[];  // WiFiGw.ino

extern uint tag[];

//##############################################################################
// Func
//##############################################################################

void blink(uint dur); 

uchar print_val(uchar val, uchar i);
void print_buf(const char* name, hb_msg_t* msg);
void printbuf(uchar* buf, uchar len);

void copy_buf(uchar* src, uchar* dst, uchar len);
void rev_4_bytes(uchar* buf);

void shift_buf(uchar* buf, uchar pos, uchar len);
void crc_add_uchar(uchar b, uint* crc);
uint calc_crc(uchar* buf, uchar len);
void crc_to_msg(hb_msg_t* msg);

uchar begin_txmsg(hb_msg_t* txmsg, uchar hb);
uchar add_txmsg_uchar(hb_msg_t* txmsg, uchar c);
uchar add_txmsg_z_str(hb_msg_t* txmsg, char* str);
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_msg_t* txmsg);
uchar finish_txmsg(hb_msg_t* txmsg);

uchar sort(uint* arr, uint len);

#endif /* __HB_COMMON_H */
