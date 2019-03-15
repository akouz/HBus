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
#include <EEPROM.h>
#include <stdio.h>
#include <ArduinoJson.h>    // https://github.com/bblanchon/ArduinoJson
#include <coos.h>           // https://github.com/akouz/a_coos

//##############################################################################
// History
//##############################################################################

// rev 0.6  -   15/03/2019, refactoring, use installed library <coos.h>, use
//              descriptors for HBcmd and HBmqtt; remove local file revisions

//##############################################################################
// Def
//##############################################################################

enum{

    // HBus revision
    HB_REV_MAJ      = 0,
    HB_REV_MIN      = 6,     

    // coos
    COOS_TASKS      = 6,   // this node uses up to 6 coos tasks            

    // MQTT
    MAX_TOPIC       = 4,    // this node can handle 4 topics

    // misc
    DF_STATUS       = 1,    // use JSON in STATUS command    
  
    // byte-stuffing
    _ESC            = 0x1B,
    _ESC_START_HB   = 2,
    _ESC_START_MQ   = 3,
    _ESC_END        = 7,
    _ESC_ESC        = 8,
    _ESC_2ESC       = 9,

    // common constants
    READY           = 1,
    NOT_READY       = 0,
  
    OK              = 0,
    ERR             = 0xEE,
    ERR_BUSY        = 0xE0,
    ERR_PARAM       = 0xE1,
    ERR_ECHO        = 0xE2,   
    ERR_UNKNOWN     = 0xE3,   

    // settings
    MAX_BUF         = 0x90,

    // EEPROM addresses
    EE_PUP_CNT      = 2,    // count power-ups
    EE_SEED         = 4,    // random seed  
    EE_OWN_ID       = 6,    // ownID
    EE_DESCR        = 0x10, // description c-string address, up to 64 chars
    
};

//##############################################################################
// Typedef
//##############################################################################

#ifndef __UCHAR_DEFINED__
#define __UCHAR_DEFINED__
typedef unsigned char   uchar;
typedef signed   char   schar;
typedef unsigned short  ushort;
typedef signed   short  sshort;
typedef unsigned int    uint;
typedef signed   int    sint;
typedef unsigned long   ulong;
typedef signed   long   slong;
typedef long     long   int64;
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
      unsigned valid    : 1;
      unsigned busy     : 1;
    };
  };
}hb_msg_t;
#endif
 
//##############################################################################
// Var
//##############################################################################

extern uint pup_cnt; 
extern uint node_seed;
extern uint led_cnt;

extern StaticJsonBuffer<128> jsonBuf;
extern Coos <COOS_TASKS> coos;

//##############################################################################
// Func
//##############################################################################

void blink(uint dur); 

uchar print_val(uchar val, uchar i);
void print_buf(const char* name, hb_msg_t* msg);

void copy_buf(uchar* src, uchar* dst, uchar len);
void shift_buf(uchar* buf, uchar pos, uchar len);
void crc_add_uchar(uchar b, uint* crc);
uint calc_crc(uchar* buf, uchar len);

uchar begin_txmsg(hb_msg_t* txmsg, uchar hb);
uchar add_txmsg_uchar(hb_msg_t* txmsg, uchar c);
uchar add_txmsg_z_str(hb_msg_t* txmsg, char* str);
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_msg_t* txmsg);
uchar finish_txmsg(hb_msg_t* txmsg);

#endif /* __HB_COMMON_H */
