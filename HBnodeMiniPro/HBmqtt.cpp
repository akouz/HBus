/*
 * File     HBmqtt.cpp 
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

#include "HBmqtt.h"
#include "HBcmd.h"

//##############################################################################
// Var
//##############################################################################

HB_mqtt HBmqtt;

// topics
const char* ownTopicName[MAX_TOPIC] = {
topic0,  topic1,  topic2,  topic3,  topic4,  topic5,  topic6,  topic7
};

uint ownTopicId[MAX_TOPIC];

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
HB_mqtt::HB_mqtt(void)
{
    mqmsg.all = 0;
    mqmsg.len = 0;
    MsgID = 1;
    MsgID_cnt = 0;  
    MsgID_err_cnt = 0;  
    for (uchar i=0; i<MAX_TOPIC; i++)
    {   
        valid[i].all = 0;   
        ownTopicId[i] = 0;
        if ((ownTopicName[i]) && (ownTopicName[i][0]))
        {
            valid[i].topic_name = 1;
        } 
    }
}

// =============================================
// When EEPROM is ready, fetch ownTopicId
// =============================================
void HB_mqtt::read_topic_id(void)
{
    uint tid;
    uint addr;
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        if  (valid[i].topic_name)
        {
            addr = EE_TOPIC_ID + 2*i;
            tid = 0x100*(uint)EEPROM.read(addr) + EEPROM.read(addr+1);
            if (tid < 0xFFFF)
            {
                ownTopicId[i] = tid;
                valid[i].topic = 1;  
            }
        }
    }         
}

// =============================================
// Is topic name in the list?  
// =============================================
char HB_mqtt::is_own_topic_name(const char* tn)
{
    for (uchar ti=0; ti<MAX_TOPIC; ti++)
    {
        if (strcmp(tn, ownTopicName[ti]) == 0)
            return ti;         
    }
    return -1;
}


// =============================================
// Is topicID in the list?  
// =============================================
char HB_mqtt::is_own_topic_id(uint tid)
{
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        if (tid == ownTopicId[i])
        {
           return (char)i;
        }
    }
    return -1;
}

// =============================================
// Get MsgID from the bus
// =============================================
void  HB_mqtt::get_MsgID(uchar msg_id)
{    
    if ((msg_id == MsgID+1) || ((MsgID == 0xFE) && (msg_id == 1)))
    {
        MsgID = msg_id;       
    }
    else
    {
        MsgID_err_cnt++;
        if ((msg_id > MsgID) || ((msg_id < 0x10) && (MsgID > 0xF0)))
        {
            MsgID = msg_id;
        }       
    } 
}

// =============================================
// Read message and extract its values  
// =============================================
char HB_mqtt::rd_msg(hb_msg_t* msg)
{
    schar res = -2;
    uchar mt = msg->buf[0] & 0x0F;                      // MsgType is low nibble            
    uint tid = 0x100*(uint)msg->buf[3] + msg->buf[4];   // TopicId
    get_MsgID(msg->buf[5]);                             // MsgId
    // ------------------------------------
    // PUBLISH messages
    // ------------------------------------
    if ((mt == MT_PUBLISH) && ((msg->encrypt) || (allow.publish)))
    {
        MsgID_cnt = (MsgID_cnt < 0xFFFFFFFF)? MsgID_cnt+1 : 1;
        if (tid >= 0x20) // it is a user-defined topic
        {
            res =  is_own_topic_id(tid);
            if (res >= 0) // it is one of own topics
            {
                jsonBuf.clear();            
                JsonObject& root = jsonBuf.parseObject(msg->buf+8);
                if (root.success())
                {
                    value[(uchar)res] = root["val"];
                    valid[(uchar)res].value = 1;  
                    blink(10);
                }
                else
                {
    //              Serial.println(" JSON parser failed");
                }
            }
        }
        else if (tid == 1) // it is pre-defined topic "time"
        {
            jsonBuf.clear();            
            JsonObject& root = jsonBuf.parseObject(msg->buf+8);
            if (root.success())
            {
                coos.daysec = (ulong)root["daysec"];
                blink(10);
            }
            if (coos.uptime < 0x10000000) // time never been updated
            {
                coos.uptime = (ulong)root["atime"];
            } 
            else
            {
                ulong atime = (ulong)root["atime"];
                if ((atime < coos.uptime+TIME_TOLERANCE) && (atime > coos.uptime-TIME_TOLERANCE))  // +/- 10 sec
                {
                    coos.uptime = atime;  // time accepted
                }                  
            }
        }      
    }
    // ------------------------------------
    // REGISTER messages
    // ------------------------------------
    if ((mt == MT_REGISTER) && (msg->buf[8]) && ((msg->encrypt) || (allow.reg)))  
    {
        MsgID_cnt = (MsgID_cnt < 0xFFFFFFFF)? MsgID_cnt+1 : 1;
        msg->buf[msg->len] = 0;   // make 0-terminated string
        res = is_own_topic_name((const char *)msg->buf + 8);
        if (res >= 0)
        {
            if (tid > 0) // if it is an assignment    
            {
                uint addr = EE_TOPIC_ID + 2*res;
                EEPROM.write(addr, (uchar)(tid >> 8));
                EEPROM.write(addr+1, (uchar)tid);
                if (tid < 0xFFFF) // if valid tid  
                {
                    ownTopicId[(uchar)res] = tid;   //  bind ownTopicId and ownTopicName
                    valid[(uchar)res].topic = 1;  
                }
                else // if tid==0xFFFF then clear ownTopicId
                {
                    ownTopicId[(uchar)res] = 0;    
                    valid[(uchar)res].topic = 0;
                }
                blink(10);
            }
            else   // if t is a query
            {
                if (ownTopicId[(uchar)res]) // ownTopicId was assigned
                {
                    make_msg_reg((uchar)res);  // broadcast it  
                    blink(10);
                } 
            }
        }
    }    
    return res;
}

// =============================================
// Add signature to the end of message
// =============================================
void  HB_mqtt::add_signature(char* buf, uint* len)
{
    buf[(*len)++] = CHAR_SPACE;
    buf[(*len)++] = CHAR_CR;
    buf[(*len)++] = CHAR_LF;
    buf[(*len)++] = CHAR_TAB;
    buf[(*len)++] = 0;
}

// =============================================
// Check if signature prersent
// =============================================
uchar HB_mqtt::is_signature(char* buf)
{
    uint len = strlen(buf);
    if (len > 4)
    {
        if ((buf[len] == 0) && (buf[len-1] == CHAR_TAB))
        {
            if ((buf[len-2] == CHAR_LF) && (buf[len-3] == CHAR_CR))
            {
                if (buf[len-4] == CHAR_SPACE)
                    return 1; 
            }
        }  
    }
    return 0;
}

// =============================================
// Make message header  
// =============================================
void HB_mqtt::make_msg_header(uchar MsgType, uint tid)
{
    begin_txmsg(&mqmsg, 0);
    uchar msb_nibble = random(0x100) & 0xF0;
    add_txmsg_uchar(&mqmsg, (msb_nibble | MsgType)); // MsgType           
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[1]);       // NodeId
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[0]);    
    add_txmsg_uchar(&mqmsg, (uchar)(tid >> 8));     // TopicId
    add_txmsg_uchar(&mqmsg, (uchar)tid);
    MsgID = (MsgID < 0xFE)? MsgID+1 : 1;          
    add_txmsg_uchar(&mqmsg, MsgID);                 // MsgId
    add_txmsg_uchar(&mqmsg, random(0x100));         // nonce                
    add_txmsg_uchar(&mqmsg, 1);                     // DF = JSON
 }

// =============================================
// Make a pseudo-MQTT-SN message REGISTER  
// =============================================
uchar HB_mqtt::make_msg_reg(uchar ti)
{
    mqmsg.valid = 0;
    if ((ti < MAX_TOPIC) && (valid[ti].topic_name)) 
    {
        make_msg_header(MT_REGISTER, ownTopicId[ti]);
        if ((ownTopicName[ti]) && (ownTopicName[ti][0]))
        {
            add_txmsg_z_str(&mqmsg, (char*)ownTopicName[ti]);  
        }
        finish_txmsg(&mqmsg);
        mqmsg.encrypt = (allow.broadcast)? 0 : 1;   // can send unencrypted?
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return OK;
    }
    else
        return ERR;
}

// =============================================
// Make message PUBLISH  
// =============================================
// if len=0 then treat buf as a text string 
uchar HB_mqtt::make_msg_publish(uint tid, uchar* buf, uchar len)
{
    mqmsg.valid = 0;
    if ((tid) && (tid < 0xFFFF)) 
    {
        make_msg_header(MT_PUBLISH, tid);
        if (len == 0)
        {
            add_txmsg_uchar(&mqmsg, '{'); 
        }
        // --------------------------------------
        // if buffer supplied
        // --------------------------------------
        if (buf)
        {
            for (uchar i=0; i<128; i++)
            {
                // --------------------------
                // if buf is a text string
                // --------------------------
                if (len == 0)
                {
                    if (buf[i])
                    {
                        add_txmsg_uchar(&mqmsg, buf[i]);
                    }
                    else
                        break;
                }
                // --------------------------
                // if buf is a binary buf
                // --------------------------
                else
                {
                    if (i<len)
                    {
                        add_txmsg_uchar(&mqmsg, buf[i]);
                    }
                    else
                        break;
                }
            }
        }
        if (len == 0)
        {
            add_txmsg_uchar(&mqmsg, '}');            
        } 
        finish_txmsg(&mqmsg);
        mqmsg.encrypt = (allow.broadcast)? 0 : 1;   // can send unencrypted?
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return OK;
    }
    return ERR;
}

// =============================================
// PUBLISH own value  to HBus and to MQTT
// =============================================
hb_msg_t* HB_mqtt::publish_own_val(uint idx)
{
    uint len;
    uchar mbuf[0x40];
    uint tid = ownTopicId[idx]; // topic ID
    if (tid)  
    {
        make_msg_header(MT_PUBLISH, tid);
        len = sprintf(mbuf,"{val:");        
        if (valid[idx].value) 
        {
            dtostrf(value[idx], 4,2, mbuf+len);
            len = strlen(mbuf);
            mbuf[len++] = '}';
            mbuf[len] = 0;
        }
        else
        {
            len += sprintf(mbuf+len, "0}");
        }                
        add_txmsg_z_str(&mqmsg, mbuf);              // add mbuf as a z-string to HBus message
        finish_txmsg(&mqmsg);                       // finish message to HBus
        mqmsg.encrypt = (allow.broadcast)? 0 : 1;   // can send unencrypted?
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return &mqmsg; 
    }
    return NULL;
}

// =============================================
// Init ownTopicId
// =============================================
uchar HB_mqtt::init_topic_id(uint node_id)
{
    static uchar ti = 0;
    static uchar state = 0;
    if ((node_id & 0xF000) == 0xF000) // if temporary NodeID
    {
        return 1;   // do not process further, loop
    } 
    switch (state)
    {
    case 0:
//        Serial.print(" ti=");
//        Serial.print(ti);
        if (valid[ti].topic_name == 0) // if TopicName invalid
        {
//            Serial.print(", TopicName not valid");
            valid[ti].topic = 0;                    // ensure     
            valid[ti].value = 0;       
            state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish           
        } 
        else // if TopicName valid
        {
            if (ownTopicId[ti])    // if ownTopicId already valid  
            {
//                Serial.print(", already valid");
                valid[ti].topic = 1;                    // ensure            
                state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish           
            }
            else    // TopicId not valid, make request
            {
//                Serial.print(", request");
                valid[ti].topic = 0;                    
                make_msg_reg(ti); // issue REGISTER with TopicId=0
                state++;
            }
        }          
//        Serial.println();
        break;
    case 1:
        if (ownTopicId[ti] == 0)  // if other nodes did not supply TopicId     
        {
            ownTopicId[ti] =  (node_id << 5) | ti; // use NodeID to assign TopicId
            uint addr = EE_TOPIC_ID + 2*ti;
            EEPROM.write(addr, (uchar)(ownTopicId[ti] >> 8));
            EEPROM.write(addr+1, (uchar)ownTopicId[ti]);
            valid[ti].topic = 1;              
            make_msg_reg(ti);   // issue REGISTER with newly assigned ownTopicId - targeting gateways
        }
        state = (++ti >= MAX_TOPIC) ? 99 : 0;  // next topic or finish         
        break;
    default:
        ti = 0;     // be ready for another cycle
        state = 0;
        return OK;  // all topics processed, finish
        break;        
    }    
    return 2;   // continue
}

/* EOF */