/*
 * File     HBrxtx.h
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
 
#ifndef __HB_RXTX_H
#define __HB_RXTX_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Def
//##############################################################################

enum{  
  _PRI_LO       = 0xFF,
  _PRI_MED      = 0xFC,
  _PRI_HI       = 0xF0,
  
  TX_TMOUT      = 20,  // 200 ms
};

//##############################################################################
// Class
//##############################################################################

class Hb_rxtx{
    public:
                Hb_rxtx(void);
    uchar       err_cnt;
    union{
        uchar   all;
        struct{
            unsigned rx_busy    : 1;     
            unsigned busy       : 1;
            unsigned debug      : 1;
            unsigned no_crc     : 1;
            unsigned seed       : 1;    // when random seed is set
        };
    }flag;
    uchar       priority;
    uchar       txpos;
    uchar       txcnt;
    hb_msg_t    rxmsg;
    hb_msg_t*   rx(uchar c);
    uchar       rtr_cnt;
    uchar       start_tx(hb_tx_msg_t* buf);
    uchar       tx(uchar* pause_cnt);
       
    private:
    uchar       tx_tmout;
    hb_tx_msg_t*   txbuf;
    uchar       echobuf[3];
    uchar       echolen;
    uchar       start;  
    uint        txcrc;
    uchar       add_rx_uchar(uchar c, hb_msg_t* dest);
    uchar       check_crc(hb_msg_t* msg);
};
extern Hb_rxtx HBrxtx;

#endif /* __HB_RXTX_H */

