/*
 * Author    A.Kouznetsov
 * Rev       1.0 dated 20/12/2018
 * Target    Arduino

Redistribution and use in source and binary forms, with or without modification, are permitted.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
 
#ifndef __COMMON_H
#define __COMMON_H

//##############################################################################
// Inc
//##############################################################################

#include <Arduino.h>
#include <EEPROM.h>

//##############################################################################
// Device
//##############################################################################

enum{
/*
 * ============== Descriptor ==============
 */
  DEV_TYPE      = 2,  // HBus node
  DEV_MODEL     = 1,  // Arduino Pro Mini

/*
 * ============== Hardware ==============
 * Rev 0.1  - December 2018, development set-up, open collector
 */
  HW_REV_MAJ    = 0,
  HW_REV_MIN    = 1,  

/*
 * ============== Software ==============
 * Rev 0.1  - December 2018, development wip
 */
  SW_REV_MAJ    = 0,
  SW_REV_MIN    = 1,  

/*
 * ============== Bootloader ==============
 * Rev 0.1  - December 2018, original Arduino bootloader
 */
  BT_REV_MAJ    = 0,
  BT_REV_MIN    = 1,  
};

//##############################################################################
// Def
//##############################################################################

enum{
    _ESC            = 0x1B,
    _ESC_START_HB   = 2,
    _ESC_START_MQ   = 3,
    _ESC_END        = 7,
    _ESC_ESC        = 8,
    _ESC_2ESC       = 9,

    READY           = 1,
    NOT_READY       = 0,
  
    OK              = 0,
    ERR             = 0xEE,
    ERR_BUSY        = 0xE0,
    ERR_PARAM       = 0xE1,
    ERR_ECHO        = 0xE2,   


    MAX_BUF         = 0x90,

    // EEPROM adresses
    EE_PUP_CNT      = 2,    // count power-ups
    EE_SEED         = 4,    // random seed  
    EE_OWN_ID       = 6,    // ownID
    EE_DESCR        = 0x10, // description c-string, up to 64 chars     
};


#ifndef BUILTIN_LED
  #define BUILTIN_LED   13
#endif  

// #define DEBUG


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

struct hb_msg_struct{
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
};
typedef struct hb_msg_struct hb_msg_t;


//##############################################################################
// Func
//##############################################################################

void copy_buf(uchar* src, uchar* dst, uchar len);
void shift_buf(uchar* buf, uchar pos, uchar len);
void crc_add_uchar(uchar b, uint* crc);
uint calc_crc(uchar* buf, uchar len);

uchar begin_txmsg(hb_msg_t* txmsg, uchar hb);
uchar add_txmsg_uchar(hb_msg_t* txmsg, uchar c);
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_msg_t* txmsg);
uchar finish_txmsg(hb_msg_t* txmsg);

#endif /* __COMMON_H */
