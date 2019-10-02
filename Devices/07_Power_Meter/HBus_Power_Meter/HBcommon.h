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

#include "HBconfig.h"
#include <Arduino.h>
#include <EEPROM.h>
#include <stdio.h>
#include <ArduinoJson.h>    // https://github.com/bblanchon/ArduinoJson   rev 5.13.4
#include <coos.h>           // https://github.com/akouz/a_coos  rev 1.5

//##############################################################################
// History
//##############################################################################

// rev 0.6  -   15/03/2019, refactoring, use installed library <coos.h>, use
//              descriptors for HBcmd and HBmqtt; remove local file revisions
// rev 0.7  -   16/03/2019, report value as 0 (no decimal point) if it is not valid
// rev 0.8  -   29/04/2019, added CMD_TOPIC and REGISTER
// rev 0.9  -   7/08/2019, HBcipher added, Tx byte-stuffing on-fly
// rev 0.10 -   12/08/2019, SET_ID command can change only tmp ID
// rev 0.11 -   13/08/2019, reject published values if time stamp mismatch
// rev 1.0  -   05/09/2019, timestamps added to message headers
// rev 1.1  -   10/09/2019, to save SRAM, hb_tx_msg_t used for tx messages
// rev 1.2  -   01/10/2019, configurable params moved into HBconfig.h,
//              PROGMEM used to minimise RAM


//##############################################################################
// Def
//##############################################################################

enum{

    // HBus revision
    HB_REV_MAJ      = 1,
    HB_REV_MIN      = 2,


    // misc
    DF_STATUS       = 1,    // use JSON in STATUS command
    TIME_TOLERANCE  = 60,  // +/- 1 min

    // byte-stuffing
    _ESC            = 0x1B,
    _ESC_START_HB   = 2,  // HBus
    _ESC_START_MQ   = 3,  // MQTT-SN
    _ESC_START_HBE  = 4,  // HBus encrypted
    _ESC_START_MQE  = 5,  // MQTT-SN encrypted
    _ESC_END        = 7,  // finish frame
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

    OK              = 0,
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
    // EEPROM addresses (max EEPROM size 1024 bytes)
    // ------------------------------------
    EE_PUP_CNT      = 2,    // count power-ups
    EE_SEED         = 4,    // random seed
    EE_OWN_ID       = 6,    // own NodeId
    EE_TZ           = 10,   // time zone
    EE_SECURITY     = 12,   // access control settings
    EE_SECURITY_INV = 14,   // inverted access control settings
    EE_XTEA_KEY     = 0x30, // XTEA cipher key, 16 bytes
    EE_DESCR        = 0x40, // description c-string, up to 64 chars
    EE_TOPIC_ID     = 0x80, // own TopicIds, 2-bytes each, up to 32 topics
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
      unsigned ts_ok    : 1;
      unsigned valid    : 1;
      unsigned busy     : 1;
    };
  };
}hb_msg_t;

typedef struct{
  uchar buf[MAX_TX_BUF];
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
      unsigned ts_ok    : 1;
      unsigned valid    : 1;
      unsigned busy     : 1;
    };
  };
}hb_tx_msg_t;
#endif


//##############################################################################
// Var
//##############################################################################

extern uint pup_cnt;
extern uint node_seed;
extern uint led_cnt;

extern StaticJsonBuffer<128> jsonBuf;
extern Coos <COOS_TASKS, 1> coos;    // 1.024 ms ticks

extern const uchar node_descr[];

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

uchar begin_txmsg(hb_tx_msg_t* txmsg, uchar hb);
uchar add_txmsg_uchar(hb_tx_msg_t* txmsg, uchar c);
uchar add_txmsg_z_str(hb_tx_msg_t* txmsg, char* str);
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_tx_msg_t* txmsg);
void add_ts(hb_tx_msg_t* txmsg);
uchar finish_txmsg(hb_tx_msg_t* txmsg);

uchar ts_valid(hb_msg_t* rxmsg);
uchar sort(uint* arr, uint len);

#endif /* __HB_COMMON_H */
