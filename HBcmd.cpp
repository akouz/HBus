/*
 * File     HBcmd.cpp
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

//##############################################################################
// Inc
//##############################################################################

#include  "HBcommon.h"
#include  "HBmqtt.h"
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
    descriptor = NULL;
    custom_cmd = NULL;
}

// =====================================  
// Set descriptor required for REV command
// =====================================  
void Hb_cmd::set_descriptor(uchar* descr)
{
    descriptor = descr;   // 
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
                case CMD_CUSTOM:    res = rply_custom(rxmsg, &reply);   break;
                default:            res = rply_unknown(rxmsg, &reply);  break;
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
            alien_boot(rxmsg->buf[7]);  
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
// Unknown command
// =====================================  
uchar Hb_cmd::rply_unknown(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  ERR_UNKNOWN);  
    return READY;
}

// =====================================  
// Reply revisions
// =====================================  
uchar Hb_cmd::rply_rev(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  OK);
    for (uchar i=0; i<8; i++)
    {
        if (descriptor) // if descriptor supplied
        {
            add_txmsg_uchar(rply, descriptor[i]);
        }
        else
        {
            add_txmsg_uchar(rply, 0);
        }    
    }
    add_txmsg_uchar(rply, HB_REV_MAJ);   // HBus revision specified separately
    add_txmsg_uchar(rply, HB_REV_MIN);
    return READY;           
}

// =====================================  
// Reply status
// =====================================  
uchar Hb_cmd::rply_status(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    uint tpc;
    copy_msg_hdr(rxmsg, 0, 7, rply);
    if (DF_STATUS == 1) // DF = JSON 
    {
        char buf[32];
        add_txmsg_uchar(rply,  DF_STATUS);
        // list all topics
        snprintf(buf, sizeof(buf),"{topics:[");
        add_txmsg_z_str(rply, buf);
        for (uchar i=0; i< MAX_TOPIC; i++)
        {
            tpc = HBmqtt.get_topic(i); 
            if (i < MAX_TOPIC-1)        
                snprintf(buf, sizeof(buf),"%d,", tpc);
            else
                snprintf(buf, sizeof(buf),"%d]", tpc);
            add_txmsg_z_str(rply, buf);
        }
        // list all topics values
        snprintf(buf, sizeof(buf),",values:[");
        add_txmsg_z_str(rply, buf);
        for (uchar i=0; i< MAX_TOPIC; i++)
        {
            dtostrf(HBmqtt.value[i], 4,2, buf);
            add_txmsg_z_str(rply, buf);         
            if (i < MAX_TOPIC-1)        
                add_txmsg_uchar(rply, ',');
            else
                add_txmsg_uchar(rply, ']');
        }
        add_txmsg_uchar(rply, '}');
    }
    else // DF = binary, other formats not implemented yet
    {
        add_txmsg_uchar(rply,  0); // 0 = binary data
        add_txmsg_uchar(rply, MAX_TOPIC);
        for (uchar i=0; i<MAX_TOPIC; i++)
        {
            tpc = HBmqtt.get_topic(i); 
            add_txmsg_uchar(rply, (uchar)(tpc >> 8));
            add_txmsg_uchar(rply, (uchar)tpc);
        }
    }
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
    blink((uint)rxmsg->buf[7]*100);
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
// Reply C_CMD
// =====================================  
uchar Hb_cmd::rply_custom(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    if (custom_cmd)
    {
        add_txmsg_uchar(rply,  OK);
        custom_cmd(rxmsg, rply);  // whatever defined
    }
    else
    {
        add_txmsg_uchar(rply,  ERR);    // custom command not defined
    }    
    return READY;
}

// =====================================  
// Set custom command 
// =====================================  
void  Hb_cmd::set_custom_cmd(void (*c_cmd)(hb_msg_t* msg, hb_msg_t* rply))
{
    custom_cmd = c_cmd;   // set user-defined custom command
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
