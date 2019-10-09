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
// Def
//##############################################################################

enum{
  CMD_REV            = 1,
  CMD_STATUS         = 2,
  CMD_COLLECT        = 3,
  CMD_PING           = 4,
  CMD_SET_ID         = 5,
  CMD_BOOT           = 6,
  CMD_BEEP           = 7,
  CMD_DESCR          = 8,
  CMD_SECURITY       = 9,
  CMD_CUSTOM         = 10,
  CMD_TOPIC          = 11,
};

//##############################################################################
// Class
//##############################################################################


class HB_cmd{
  public:
                HB_cmd(void);
    union{
        uint    ID;
        uchar   id[2];
    }own;
    union{
        uint all;
        struct{
            unsigned    rev         : 1;
            unsigned    status      : 1;
            unsigned    collect     : 1;
            unsigned    ping        : 1;
            unsigned    boot        : 1;
            unsigned    rddescr     : 1;
            unsigned    wrdescr     : 1;
            unsigned    customcmd   : 1;
            unsigned    topic       : 1;
            unsigned    rdsecurity  : 1;
            unsigned    ignore_ts   : 1;    // ignore time stamp mismatch for encrypted messages
            unsigned                : 2;    // not used
            unsigned                : 3;    // not used here, those flags are for MQTT mode
        };
    } allow;    // allowed unecrypted access
    uint        ignore_traffic;                 // in 10 ms ticks
    hb_tx_msg_t*   process_rx_cmd(hb_msg_t* rxmsg);
    void        set_custom_cmd(hb_tx_msg_t* (*c_cmd)(hb_msg_t* msg));
    void        tick10ms(void);
    void        read_own_ID(void);
    void        read_security(uchar key_valid);

  private:
    union{
        uint ID;
        uchar id[2];
    }msg;
    uchar       rply_tmout;
    uint        ignore_collect;         // in 10 ms ticks
    hb_tx_msg_t cmd_reply;
    uchar       rply_unknown(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_rev(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_status(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_collect(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_ping(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_setID(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_boot(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    void        alien_boot(hb_msg_t* rxmsg);
    uchar       rply_beep(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_descr(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_security(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_custom(hb_msg_t* rxmsg, hb_tx_msg_t* rply);
    uchar       rply_topic(hb_msg_t* rxmsg, hb_tx_msg_t* rply);    // read MQTT topic
    hb_tx_msg_t*   (*custom_cmd)(hb_msg_t* msg);   // user-defined custom command
};
extern HB_cmd HBcmd;

#endif /* #define __HB_CMD_H */
