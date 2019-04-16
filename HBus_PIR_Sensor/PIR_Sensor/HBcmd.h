/*
 * File     HBcmd.h
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
 
#ifndef __HB_CMD_H
#define __HB_CMD_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "HBcommon.h"

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
  CMD_CUSTOM         = 10,
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
    uint        ignore_traffic;                 // in 10 ms ticks
    void        set_descriptor(uchar* descr);   // 8-bytes long descriptor
    hb_msg_t*   process_rx_cmd(hb_msg_t* rxmsg);
    void        set_custom_cmd(void (*c_cmd)(hb_msg_t* msg, hb_msg_t* rply));
    void        tick10ms(void);

  private:
    uchar*      descriptor;           // HBus descriptor 
    union{
        uint ID;
        uchar id[2];
    }msg;
    uchar       rply_tmout;
    uint        ignore_collect;         // in 10 ms ticks
    hb_msg_t    reply;    
    uchar       rply_unknown(hb_msg_t* rxmsg, hb_msg_t* rply);   
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
    uchar       rply_custom(hb_msg_t* rxmsg, hb_msg_t* rply);
    void        (*custom_cmd)(hb_msg_t* msg, hb_msg_t* rply); // user-defined custom command    
};
extern Hb_cmd HBcmd;

#endif /* #define __HB_CMD_H */
