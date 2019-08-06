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
 *                                                                                                                                                                        F
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

#include  "HBmqtt.h"
#include  "HBcmd.h"
#include  "HBcipher.h"

//##############################################################################
// Var
//##############################################################################

HB_cmd HBcmd;

//##############################################################################
// Func
//##############################################################################


// =====================================
// Constructor
// =====================================
HB_cmd::HB_cmd(void)
{
    this->ignore_collect = 0;
    this->ignore_traffic = 0;
    this->cmd_reply.len = 0;    
    this->cmd_reply.all = 0;
    this->own.ID = 0;
    this->node_descr = NULL;
    this->custom_cmd = NULL;
}

// =====================================  
// Init own ID when EEPROM started
// =====================================  
void HB_cmd::read_own_ID(void)
{
    this->own.id[1] = EEPROM.read(EE_OWN_ID);
    this->own.id[0] = EEPROM.read(EE_OWN_ID+1);    
}

// =====================================  
// Read security settings from EEPROM
// =====================================  
void HB_cmd::read_security(uchar key_valid)
{
    this->allow.all = 0xFFFF;    // default: enable unecrypted access
    HBmqtt.allow.all = 0xFFFF;
    if ((key_valid) && (this->own.ID < 0x800))  // if key valid and permanent NodeID assigned
    {
        uint val = 0x100*(uint)EEPROM.read(EE_SECURITY);
        val +=(uchar)EEPROM.read(EE_SECURITY+1);   
        uint notval = 0x100*(uint)EEPROM.read(EE_SECURITY_INV);
        notval += (uchar)EEPROM.read(EE_SECURITY_INV+1);
        notval = (~notval) & 0xFFFF;    // because ESP8266 compiler extends inversion to 32 bit
        if (val == notval)              // if direct and inverted values are the same
        {
            this->allow.all = val;      // HBus mode settings
            HBmqtt.allow.all = val;     // MQTT mode settings
            //Serial.print(" allow=");
            //Serial.println(this->allow.all, HEX);
        }   
    }
}

// =====================================  
// Set descriptor required for REV command
// =====================================  
void HB_cmd::set_descriptor(uchar* descr)
{
    this->node_descr = descr;  
}

// =====================================  
// Process input commands, form a reply if required
// =====================================  
hb_msg_t* HB_cmd::process_rx_cmd(hb_msg_t* rxmsg)
{
    uchar cmd;
    uchar res = 0; // no reply
    if ((rxmsg) && (this->cmd_reply.busy == 0) && (rxmsg->hb) && (rxmsg->len >= 8))
    {
        this->cmd_reply.hb = rxmsg->hb;
        cmd = rxmsg->buf[0];
        // ----------------------------
        // reply to COLLECT command
        // ----------------------------
        if ((cmd == CMD_COLLECT) && (this->ignore_collect == 0))
        {
            res = rply_collect(rxmsg, &cmd_reply);
            rxmsg->busy = 0;
            return &cmd_reply;            
        }  
        // ----------------------------
        // if ID (eg address) matches 
        // ----------------------------
        else if ((rxmsg->buf[3] == own.id[1]) && (rxmsg->buf[4] == own.id[0]))
        {
            this->cmd_reply.hb = rxmsg->hb;
            this->cmd_reply.encrypt = rxmsg->encrypt;
//            if (rxmsg->encrypt) 
//               Serial.println(" Encrypted");
            begin_txmsg(&this->cmd_reply, rxmsg->hb);
            this->cmd_reply.postpone = 0;
            switch(cmd)
            {
                case CMD_REV:       res = rply_rev(rxmsg, &this->cmd_reply);      break;
                case CMD_STATUS:    res = rply_status(rxmsg, &this->cmd_reply);   break;
                case CMD_PING:      res = rply_ping(rxmsg, &this->cmd_reply);     break;
                case CMD_SET_ID:    res = rply_setID(rxmsg, &this->cmd_reply);    break;
                case CMD_BOOT:      res = rply_boot(rxmsg, &this->cmd_reply);     break;
                case CMD_BEEP:      res = rply_beep(rxmsg, &this->cmd_reply);     break;
                case CMD_DESCR:     res = rply_descr(rxmsg, &this->cmd_reply);    break;
                case CMD_SECURITY:  res = rply_security(rxmsg, &this->cmd_reply); break;
                case CMD_CUSTOM:    res = rply_custom(rxmsg, &this->cmd_reply);   break;
                case CMD_TOPIC:     res = rply_topic(rxmsg, &this->cmd_reply);    break;
                default:            res = rply_unknown(rxmsg, &this->cmd_reply);  break;
            }
            if (READY == res)
            {                
                if (OK == finish_txmsg(&cmd_reply))
                {
                    rxmsg->busy = 0;
                    rxmsg = NULL;
                    return &cmd_reply;
                }                
            }
        }
        // ----------------------------
        // react to BOOT to another node 
        // ----------------------------
        else if (cmd == CMD_BOOT)   
        {
            alien_boot(rxmsg);  
        }
    }
    rxmsg->busy = 0;
    return NULL; 
}

// =====================================  
// Unknown command
// =====================================  
uchar HB_cmd::rply_unknown(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    copy_msg_hdr(rxmsg, 0, 7, rply);
    add_txmsg_uchar(rply,  ERR_UNKNOWN);  
    return READY;
}

// =====================================  
// Reply revisions
// =====================================  
uchar HB_cmd::rply_rev(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.rev))
    {
        copy_msg_hdr(rxmsg, 0, 7, rply);
        add_txmsg_uchar(rply,  OK);
        for (uchar i=0; i<8; i++)
        {
            if (node_descr) // if descriptor supplied
            {
                add_txmsg_uchar(rply, node_descr[i]);
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
    return ERR_SECURITY;
}

// =====================================  
// Reply status
// =====================================  
uchar HB_cmd::rply_status(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.status))
    {
        uint tpc;
        copy_msg_hdr(rxmsg, 0, 7, rply);
        if (DF_STATUS == 1) // DF = JSON 
        {
            char buf[32];
            add_txmsg_uchar(rply,  DF_STATUS);
            // list all topics
            snprintf(buf, sizeof(buf),"{tid:[");
            add_txmsg_z_str(rply, buf);       
            buf[0] = 0;
            for (uchar i=0; i< MAX_TOPIC; i++)
            {
                if (HBmqtt.valid[i].topic_name)
                {          
                    if (buf[0])  
                    {
                        add_txmsg_uchar(rply, uchar(',')); // add comma between items
                    }                          
                    tpc = ownTopicId[i];
                    snprintf(buf, sizeof(buf),"%d", tpc);
                    add_txmsg_z_str(rply, buf);
                } 
            }                
            // list all topics values
            snprintf(buf, sizeof(buf),"], val:[");
            add_txmsg_z_str(rply, buf);
            buf[0] = 0;
            for (uchar i=0; i<MAX_TOPIC; i++)
            {
                if (HBmqtt.valid[i].topic_name)
                {            
                    if (buf[0])
                    {
                        add_txmsg_uchar(rply, uchar(',')); // add comma between items
                    }                          
                    if (HBmqtt.valid[i].value) 
                    {
                        dtostrf(HBmqtt.value[i], 4,2, buf);
                    }
                    else
                    {
                        buf[0] = '0';
                        buf[1] = 0;
                    }
                    add_txmsg_z_str(rply, buf);
                }             
            }
            add_txmsg_uchar(rply, ']');
            add_txmsg_uchar(rply, '}');
        }
        else // DF = binary, other formats not implemented yet
        {
            add_txmsg_uchar(rply,  0); // 0 = binary data
            add_txmsg_uchar(rply, MAX_TOPIC);
            for (uchar i=0; i<MAX_TOPIC; i++)
            {
                tpc = ownTopicId[i]; 
                add_txmsg_uchar(rply, (uchar)(tpc >> 8));
                add_txmsg_uchar(rply, (uchar)tpc);
            }
        }
        return READY;           
    }
    return ERR_SECURITY;                    
}

// =====================================  
// Reply COLLECT
// =====================================  
uchar HB_cmd::rply_collect(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.collect))
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
            cmd_reply.hb = rxmsg->hb;
            cmd_reply.encrypt = rxmsg->encrypt;
            begin_txmsg(&cmd_reply, rxmsg->hb);
            copy_msg_hdr(rxmsg, 0, 3, rply);
            add_txmsg_uchar(rply, own.id[1]); 
            add_txmsg_uchar(rply, own.id[0]); 
            copy_msg_hdr(rxmsg, 5, 7, rply);
            add_txmsg_uchar(rply, OK);
            finish_txmsg(&cmd_reply);
        }
        else
        {
            rply->postpone = slots;
            rply->len = 0;
            res = READY;
        }
        return res;
    }
    return ERR_SECURITY;                    
}

// =====================================  
// Reply PING
// =====================================  
uchar HB_cmd::rply_ping(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.ping))
    {
        ignore_collect = (uint)rxmsg->buf[7]*100;
        copy_msg_hdr(rxmsg, 0, 7, rply);
        add_txmsg_uchar(rply,  OK);
        return READY;
    }
    return ERR_SECURITY;                    
}

// =====================================  
// Reply SET_ID
// =====================================  
uchar HB_cmd::rply_setID(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.setID))
    {
        uchar res;
        if (rxmsg->len > 9)
        {
            own.id[1] = rxmsg->buf[8];
            own.id[0] = rxmsg->buf[9];
            EEPROM.write(EE_OWN_ID, own.id[1]);
            EEPROM.write(EE_OWN_ID+1, own.id[0]);
            EEPROM.commit();
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
    return ERR_SECURITY;                    
}

// =====================================  
// Reply BOOT
// =====================================  
uchar HB_cmd::rply_boot(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.boot))
    {
        copy_msg_hdr(rxmsg, 0, 7, rply); 
        add_txmsg_uchar(rply,  OK);
        return READY;
    }
    return ERR_SECURITY;                    
}

// =====================================  
// React to alien BOOT
// =====================================  
void HB_cmd::alien_boot(hb_msg_t* rxmsg)
{
    if ((rxmsg->encrypt) || (this->allow.boot))
    {
        uint param = (uint)rxmsg->buf[7];
        this->ignore_traffic = param*100;
    }
}

// =====================================  
// Reply BEEP
// =====================================  
uchar HB_cmd::rply_beep(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.ping))
    {
        blink((uint)rxmsg->buf[7]*100);
        copy_msg_hdr(rxmsg, 0, 7, rply); 
        add_txmsg_uchar(rply,  OK);
        return READY; 
    }
    return ERR_SECURITY;                    
}

// =====================================  
// Reply DESCR
// =====================================  
uchar HB_cmd::rply_descr(hb_msg_t* rxmsg, hb_msg_t* rply)
{    
    uchar len, rdwr;
    copy_msg_hdr(rxmsg, 0, 7, rply);
    rdwr = rxmsg->buf[7];
    // ----------------------
    // write
    // ----------------------
    if ((rdwr) && (rxmsg->len > 8)) 
    {
        if ((rxmsg->encrypt) || (this->allow.wrdescr))
        {
            len = rxmsg->buf[8];
            if (len < 64)
            {
                add_txmsg_uchar(rply,  OK);
                EEPROM.write(EE_DESCR, len);
                for (uchar i=0; i<len; i++)
                {
                    EEPROM.write(EE_DESCR+1+i, rxmsg->buf[9+i]); 
                }
                EEPROM.commit();
            }
            else
            {
                add_txmsg_uchar(rply,  ERR);
            }
            return READY;
        }
    }
    // ----------------------
    // read
    // ----------------------
    else 
    {
        if ((rxmsg->encrypt) || (this->allow.rddescr))
        {
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
    } 
    return ERR_SECURITY;                    
}

// =====================================  
// EEPROM cipher and access control settings  
// =====================================  
uchar HB_cmd::rply_security(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    uchar rdwr = rxmsg->buf[7];
    // ----------------------
    // store
    // ----------------------
    if ((rdwr) && (rxmsg->len > 9))
    {
        if ((rxmsg->encrypt) || (this->allow.wrsecurity))
        {
            copy_msg_hdr(rxmsg, 0, 7, rply);
            // store unencrypted access settings
            EEPROM.write(EE_SECURITY, rxmsg->buf[8]);
            EEPROM.write(EE_SECURITY+1, rxmsg->buf[9]);
            EEPROM.write(EE_SECURITY_INV, ~rxmsg->buf[8]);
            EEPROM.write(EE_SECURITY_INV+1, ~rxmsg->buf[9]);
            // store EEPROM key
            if (rxmsg->len > 25) // new cipher supplied
            {
                if (HBcipher.valid == 0) // stored cipher is blank 
                {
                    for (uchar i=0; i<4; i++) // 4 keys
                    {
                        for (uchar j=0; j<4; j++) // 4 bytes each
                        {
                            // reverse byte order in every key
                            EEPROM.write(EE_XTEA_KEY+ 4*i + j, rxmsg->buf[10+4*i + 3-j]); 
                        }                    
                    }
                    EEPROM.commit();
                    HBcipher.get_EE_key();  // restore new keys from EEPROM
                    add_txmsg_uchar(rply, OK);
                    return READY;
                }
                else 
                {
                    add_txmsg_uchar(rply, ERR); // cannot rewrite valid cipher
                }
            }
            else if (rxmsg->len == 10) // header + access_settings 
            {
                add_txmsg_uchar(rply, OK);
            }
            else
            {
                add_txmsg_uchar(rply, ERR_PARAM); // wrong length
            }             
            EEPROM.commit();   // write access settings aqnyway
            read_security(HBcipher.valid);
            return READY;
        }
        return ERR_SECURITY;                    
    }  
    // ----------------------
    // read
    // ----------------------
    else
    {
        if ((rxmsg->encrypt) || (this->allow.rdsecurity))
        {
            copy_msg_hdr(rxmsg, 0, 7, rply);
            add_txmsg_uchar(rply, OK);
            add_txmsg_uchar(rply, (uchar)(this->allow.all >> 8));
            add_txmsg_uchar(rply, (uchar)this->allow.all);
            return READY;
        } 
        return ERR_SECURITY;                    
    }   
    return ERR_PARAM;  
}

// =====================================  
// Reply C_CMD
// =====================================  
uchar HB_cmd::rply_custom(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.customcmd))
    {
        copy_msg_hdr(rxmsg, 0, 7, rply);
        if (custom_cmd)
        {
            add_txmsg_uchar(rply,  OK);
            rply = custom_cmd(rxmsg);        // whatever defined
        }
        else
        {
            add_txmsg_uchar(rply,  ERR);    // custom command not defined
        }    
        return READY;
    } 
    return ERR_SECURITY;                    
}


// =====================================  
// Set custom command 
// =====================================  
void HB_cmd::set_custom_cmd(hb_msg_t* (*c_cmd)(hb_msg_t* msg))
{
    custom_cmd = c_cmd;   // set user-defined custom command
}

// =====================================  
// Reply topic, eg pair TopicId + TopicName 
// =====================================  
uchar  HB_cmd::rply_topic(hb_msg_t* rxmsg, hb_msg_t* rply)
{
    if ((rxmsg->encrypt) || (this->allow.topic))
    {
        uchar ti;
        char c;
        char* tn;
        copy_msg_hdr(rxmsg, 0, 7, rply);
        ti = rxmsg->buf[7];  // topic index
        if (ti >= MAX_TOPIC)
        {
            add_txmsg_uchar(rply, ERR);
        } 
        else if (HBmqtt.valid[ti].topic_name == 0)
        {
            for (uchar i=ti+1; i<MAX_TOPIC; i++)
            {
                if (HBmqtt.valid[ti].topic_name)            
                {
                    add_txmsg_uchar(rply, i); // reply next valid index
                    return READY;
                }
            }
            add_txmsg_uchar(rply, ERR);
        } 
        else
        {
            add_txmsg_uchar(rply, OK);
            add_txmsg_uchar(rply, (uchar)(ownTopicId[ti] >> 8));
            add_txmsg_uchar(rply, (uchar)ownTopicId[ti]);
            tn = (char*)ownTopicName[ti];
            if (tn)  // if topic name defined
            {
                for (uchar i=0; i<64; i++)
                {
                    c = tn[i];
                    if (c)
                    {
                        add_txmsg_uchar(rply, (uchar)c);
                    }
                    else
                    {
                        break;
                    } 
                }
            }
        }
        return READY;
    } 
    return ERR_SECURITY;                    
}


// =====================================  
// Every 10 ms
// =====================================  
void HB_cmd::tick10ms(void)
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
