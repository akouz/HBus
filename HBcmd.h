/*
 * Library   HBus commands
 * Author    A.Kouznetsov
 * Rev       1.0 dated 26/12/2018
 * Target    Arduino

Redistribution and use in source and binary forms, with or without modification, are permitted.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
 
#ifndef __HB_CMD_H
#define __HB_CMD_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "common.h"

//##############################################################################
// Inc                                              
//##############################################################################

enum{
  CMD_REV            = 1,
  CMD_STATUS         = 2,
  CMD_COLLECT        = 3,
  CMD_PING           = 4,
  CMD_SET_ID         = 5,
  CMD_BOOT           = 6,
  CMD_BEEP           = 7,
  CMD_RD_DESCR       = 8,
  CMD_WR_DESCR       = 9,
};

//##############################################################################
// Class
//##############################################################################

class Hb_cmd{
  public:
                Hb_cmd(void);
    union{
        uint    ID;
        uchar   id[2];
    }own;
    uint        ignore_traffic;            // in 10 ms ticks
    hb_msg_t*   process_rx_cmd(hb_msg_t* rxmsg);
    void        tick10ms(void);

  private:
    union{
        uint ID;
        uchar id[2];
    }msg;
    uchar       rply_tmout;
    uint        ignore_collect;   // in 10 ms ticks
    uint        led_cnt;          // until LED switched off, in 10 ms ticks 
    hb_msg_t    reply;       
    uchar       rply_rev(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_status(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_collect(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_ping(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_setID(hb_msg_t* rxmsg, hb_msg_t* rply); 
    uchar       rply_boot(hb_msg_t* rxmsg, hb_msg_t* rply); 
    void        alien_boot(uchar param); 
    uchar       rply_beep(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_rd_descr(hb_msg_t* rxmsg, hb_msg_t* rply);
    uchar       rply_wr_descr(hb_msg_t* rxmsg, hb_msg_t* rply);    
};
extern Hb_cmd HBcmd;

#endif /* #define __HB_CMD_H */
