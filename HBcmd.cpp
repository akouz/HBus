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

//##############################################################################
// Inc
//##############################################################################

#include  "common.h"
#include  "HBcmd.h"

//##############################################################################
// Var
//##############################################################################

Hb_cmd HBcmd;

//##############################################################################
// Func
//##############################################################################

// =====================================
// Constructor
// =====================================
Hb_cmd::Hb_cmd(void)
{
    ignore_collect = 0;
    ignore_traffic = 0;
    own.id[1] = EEPROM.read(EE_OWN_ID);
    own.id[0] = EEPROM.read(EE_OWN_ID+1);
    reply.len = 0;
    reply.all = 0;
}

// =====================================  
// Process input commands, form a reply if required
// =====================================  
hb_msg_t* Hb_cmd::process_rx_cmd(hb_msg_t* rxmsg)
{
    uchar cmd;
    uchar res = 0; // no reply
    if ((rxmsg) && (reply.busy == 0) && (rxmsg->hb) && (rxmsg->len >= 8))
    {
        reply.hb = 1;
        cmd = rxmsg->buf[0];
        // ----------------------------
        // reply to COLLECT command
        // ----------------------------
        if ((cmd == CMD_COLLECT) && (ignore_collect == 0))
        {
            res = rply_collect(rxmsg, &reply);
            rxmsg->busy = 0;
            return &reply;            
        }  
        // ----------------------------
        // if ID (eg address) matches 
        // ----------------------------
        else if ((rxmsg->buf[3] == own.id[1]) && (rxmsg->buf[4] == own.id[0]))
        {
            begin_txmsg(&reply, rxmsg->hb);
            reply.postpone = 0;
            switch(cmd)
            {
                case CMD_REV:       res = rply_rev(rxmsg, &reply);      break;
                case CMD_STATUS:    res = rply_status(rxmsg, &reply);   break;
                case CMD_PING:      res = rply_ping(rxmsg, &reply);     break;
                case CMD_SET_ID:    res = rply_setID(rxmsg, &reply);    break;
                case CMD_BOOT:      res = rply_boot(rxmsg, &reply);     break;
                case CMD_BEEP:      res = rply_beep(rxmsg, &reply);     break;
                case CMD_RD_DESCR:  res = rply_rd_descr(rxmsg, &reply); break;
                case CMD_WR_DESCR:  res = rply_wr_descr(rxmsg, &reply); break;
                default:  break;
            }
            if (READY == res)
            {
                finish_txmsg(&reply);
                rxmsg->busy = 0;
                rxmsg = NULL;
                return &reply;
            }
        }
        // ----------------------------
        // react to BOOT to another node 
        // ----------------------------
        else if (cmd == CMD_BOOT)   
        {
            alien_boot(rxmsg->buf[8]);  
            rxmsg->busy = 0;
        }
        // ----------------------------
        // debug
        // ----------------------------
        else
        {
            rxmsg->busy = 0;
        }
    }
    else
    {
        rxmsg->busy = 0;
    }
    return NULL; 
}

// =====================================  
// Reply revisions
// =====================================  
uchar Hb_cmd::rply_rev(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  OK);
    add_txmsg_uchar(rply, DEV_TYPE);
    add_txmsg_uchar(rply, DEV_MODEL); 
    add_txmsg_uchar(rply, HW_REV_MAJ); 
    add_txmsg_uchar(rply, HW_REV_MIN);
    add_txmsg_uchar(rply, BT_REV_MAJ);
    add_txmsg_uchar(rply, BT_REV_MIN);
    add_txmsg_uchar(rply, SW_REV_MAJ);
    add_txmsg_uchar(rply, SW_REV_MIN);
    return READY;           
}

// =====================================  
// Reply status
// =====================================  
uchar Hb_cmd::rply_status(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  OK);
    return READY;           
}

// =====================================  
// Reply COLLECT
// =====================================  
uchar Hb_cmd::rply_collect(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    uchar grp = rxmsg->buf[3];
    uchar res = 0;
    // -----------------------------
    // check group
    // -----------------------------
    switch (grp)
    {
        case 1: // all nodes
            res = READY;
            break;
        case 2: // nodes with tmp ID
            res = ((own.ID & 0xF000) == 0xF000)? READY : NOT_READY;
            break;
        case 3: // nodes with permanent ID
            res = ((own.ID & 0xF000) == 0xF000)? NOT_READY : READY;
            break;
        default:
            break;  
    }
    // -----------------------------
    // if reply required
    // -----------------------------
    uchar slots = rxmsg->buf[4];
    if (READY == res)
    {
        rply->postpone = random(slots);
        begin_txmsg(&reply, rxmsg->hb);
        copy_msg_hdr(rxmsg, 0, 3, rply);
        add_txmsg_uchar(rply, own.id[1]); 
        add_txmsg_uchar(rply, own.id[0]); 
        copy_msg_hdr(rxmsg, 5, 7, rply);
        add_txmsg_uchar(rply, OK);
        finish_txmsg(&reply);
    }
    else
    {
        rply->postpone = slots;
        rply->len = 0;
        res = READY;
    }
    return res;
}

// =====================================  
// Reply PING
// =====================================  
uchar Hb_cmd::rply_ping(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    ignore_collect = (uint)rxmsg->buf[7]*100;
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  OK);
    return READY;
}

// =====================================  
// Reply SET_ID
// =====================================  
uchar Hb_cmd::rply_setID(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    uchar res;
    if (rxmsg->len > 9)
    {
        own.id[1] = rxmsg->buf[8];
        own.id[0] = rxmsg->buf[9];
        EEPROM.write(EE_OWN_ID, own.id[1]);
        EEPROM.write(EE_OWN_ID+1, own.id[0]);
        res = OK;
    }
    else
    {
        res = ERR;
    }
    copy_msg_hdr(rxmsg, 0, 3, rply);
    add_txmsg_uchar(rply, own.id[1]); 
    add_txmsg_uchar(rply, own.id[0]);
    copy_msg_hdr(rxmsg, 5, 7, rply); 
    add_txmsg_uchar(rply,  res);
    return READY;
}

// =====================================  
// Reply BOOT
// =====================================  
uchar Hb_cmd::rply_boot(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply); 
    add_txmsg_uchar(rply,  OK);
    return READY;
}

// =====================================  
// React to alien BOOT
// =====================================  
void Hb_cmd::alien_boot(uchar param)
{
    ignore_traffic = (uint)param*100;
}

// =====================================  
// Reply BEEP
// =====================================  
uchar Hb_cmd::rply_beep(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    digitalWrite(LED_BUILTIN, HIGH);
    led_cnt = (uint)rxmsg->buf[7]*100;
    copy_msg_hdr(rxmsg, 0, 7, rply); 
    add_txmsg_uchar(rply,  OK);
    return READY; 
}

// =====================================  
// Reply RD_DESCR
// =====================================  
uchar Hb_cmd::rply_rd_descr(hb_msg_t* rxmsg, hb_msg_t* rply)
{    
    uchar len;
    copy_msg_hdr(rxmsg, 0, 7, rply); 
    add_txmsg_uchar(rply,  OK);
    len = EEPROM.read(EE_DESCR);
    len = (len < 64)? len : 0;
    add_txmsg_uchar(rply,  len);
    if (len)
    {
        for (uchar i=0; i<len; i++)
        {
            add_txmsg_uchar(rply,  EEPROM.read(EE_DESCR+1+i));
        }
    }
    return READY;
}

// =====================================  
// Reply WR_DESCR
// =====================================  
uchar Hb_cmd::rply_wr_descr(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    uchar len = rxmsg->buf[8];
    copy_msg_hdr(rxmsg, 0, 7, rply); 
    if (len < 64)
    {
        add_txmsg_uchar(rply,  OK);
        EEPROM.write(EE_DESCR, len);
        for (uchar i=0; i<len; i++)
        {
            EEPROM.write(EE_DESCR+1+i, rxmsg->buf[9+i]); 
        }
    }
    else
    {
        add_txmsg_uchar(rply,  ERR);
    }
    return READY;
}

// =====================================  
// Every 10 ms
// =====================================  
void Hb_cmd::tick10ms(void)
{
    if (ignore_traffic)
    {   
        ignore_traffic--;
    }
    if (ignore_collect)
    {
        ignore_collect--;
    }
    if (led_cnt)
    {
        led_cnt--;
        if (led_cnt == 0)
        {
            digitalWrite(LED_BUILTIN, LOW);
        }
    }
}

/* EOF */
