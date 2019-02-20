/*
 * File     common.h 
 * Rev      1.0 dated 20/12/2018
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
 
#ifndef __COMMON_H
#define __COMMON_H

//##############################################################################
// Inc
//##############################################################################

#include <Arduino.h>
#include <EEPROM.h>
#include <stdio.h>
#include <ArduinoJson.h>

//##############################################################################
// Def
//##############################################################################

#ifndef BUILTIN_LED
  #define BUILTIN_LED   13
#endif
  
enum{
/*
 * ============== Descriptor ==============
 */
    DEV_TYPE      = 2,  // generic HBus node
    DEV_MODEL     = 1,  // Arduino Pro Mini framework
    
/*
 * ============== Hardware ==============
 * Rev 0.1  - December 2018, development set-up
 */
    HW_REV_MAJ    = 0,
    HW_REV_MIN    = 1,  

/*
 * ============== Software ==============
 * Rev 0.3  - February 2019, development wip
 */
    SW_REV_MAJ    = 0,
    SW_REV_MIN    = 3,  

/*
 * ============== Bootloader ==============
 * Rev 0.1  - December 2018, original Arduino bootloader
 */
    BT_REV_MAJ    = 0,
    BT_REV_MIN    = 1,  
  
/*
 * ============== HBus behaviour ==============
 */
    DF_STATUS    = 1,   // STATUS reply data format: 0 = binary, 1 = MQTT
 
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

    // settings
    MAX_BUF         = 0x90,

    // EEPROM adresses
    EE_PUP_CNT      = 2,    // count power-ups
    EE_SEED         = 4,    // random seed  
    EE_OWN_ID       = 6,    // ownID
    EE_DESCR        = 0x10, // description c-string, up to 64 chars     
};


//##############################################################################
// Typedef
//##############################################################################

#ifndef __UCHAR_DEFINED__
    #define __UCHAR_DEFINED__
    typedef unsigned char uchar;
    typedef signed   char schar;
    typedef unsigned short ushort;
    typedef signed short  sshort;
    typedef unsigned int  uint;
    typedef signed   int  sint;
    typedef unsigned long ulong;
    typedef signed   long slong;
    typedef long     long int64;
#endif

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
 
//##############################################################################
// Var
//##############################################################################

extern uint pup_cnt; 
extern uint node_seed;
extern uint led_cnt;

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

#endif /* __COMMON_H */
