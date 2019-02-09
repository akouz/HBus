/*
 * Library   HBus rx/tx
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
 
#ifndef __HB_RXTX_H
#define __HB_RXTX_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "common.h"

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
// Var
//##############################################################################


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
    uchar       rx_decode(uchar* src, uchar* src_len, hb_msg_t* dest);
    uchar       tx_encode(hb_msg_t* src, hb_msg_t* dest);
    hb_msg_t*   rx(uchar c);
    uchar       rtr_cnt;
    uchar       start_tx(hb_msg_t* buf);
    uchar       tx(uchar* pause_cnt);
       
    private:
    uchar       tx_tmout;
    hb_msg_t*   txbuf;
    uchar       echobuf[3];
    uchar       echolen;
    uint        txcrc;
    uchar       add_rx_uchar(uchar c, hb_msg_t* dest);
    void        add_crc(hb_msg_t* msg);
    uchar       check_crc(hb_msg_t* msg);
};
extern Hb_rxtx HBrxtx;

#endif /* __HB_RXTX_H */

