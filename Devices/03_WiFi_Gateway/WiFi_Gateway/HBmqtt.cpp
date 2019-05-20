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
topic0,  topic1,  topic2,  topic3,  topic4,  topic5,  topic6,  topic7,
topic8,  topic9,  topic10, topic11, topic12, topic13, topic14, topic15,
topic16, topic17, topic18, topic19, topic20, topic21, topic22, topic23,
topic24, topic25, topic26, topic27, topic28, topic29, topic30, topic31
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
        flag[i].all = 0;   
        ownTopicId[i] = 0;
        if ((ownTopicName[i]) && (ownTopicName[i][0]))
        {
            flag[i].topic_name_valid = 1;
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
        if  (flag[i].topic_name_valid)
        {
            addr = EE_TOPIC_ID + 2*i;
            tid = 0x100*(uint)EEPROM.read(addr) + EEPROM.read(addr+1);
            if (tid < 0xFFFF)
            {
                ownTopicId[i] = tid;
                flag[i].topic_valid = 1;  
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
void  HB_mqtt::get_MsgID(uint msg_id)
{    
    if ((msg_id == MsgID+1) || ((MsgID == 0xFFFE) && (msg_id == 1)))
    {
        MsgID = msg_id;       
    }
    else
    {
        MsgID_err_cnt++;
        if ((msg_id > MsgID) || ((msg_id < 0x10) && (MsgID > 0xFFF0)))
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
    MsgID_cnt = (MsgID_cnt < 0xFFFFFFFF)? MsgID_cnt+1 : 1;
    uchar mt = msg->buf[0];                             // MsgType 
    uint tid = 0x100*(uint)msg->buf[3] + msg->buf[4];   // TopicId
    uint mid = 0x100*(uint)msg->buf[5] + msg->buf[6];   // MsgId
    get_MsgID(mid);
    // ------------------------------------
    // PUBLISH messages
    // ------------------------------------
    if (mt == MT_PUBLISH)
    {
        res =  is_own_topic_id(tid);
        if (res >= 0)
        {
            jsonBuf.clear();            
            JsonObject& root = jsonBuf.parseObject(msg->buf+8);
            if (root.success())
            {
                value[(uchar)res] = root["val"];
                flag[(uchar)res].value_valid = 1;  
                blink(10);
            }
            else
            {
//              Serial.println(" JSON parser failed");
            }
        }      
    }
    // ------------------------------------
    // REGISTER messages
    // ------------------------------------
    if ((mt == MT_REGISTER) && (msg->buf[8]))  
    {
        msg->buf[msg->len] = 0;   // make 0-terminated string
        res = is_own_topic_name((const char *)msg->buf + 8);
        if (res >= 0)
        {
            if (tid > 0) // if it is an assignment    
            {
                uint addr = EE_TOPIC_ID + 2*res;
                EEPROM.write(addr, (uchar)(tid >> 8));
                EEPROM.write(addr+1, (uchar)tid);
                EEPROM.commit();
                if (tid < 0xFFFF) // if valid tid  
                {
                    ownTopicId[(uchar)res] = tid;   //  bind ownTopicId and ownTopicName
                    flag[(uchar)res].topic_valid = 1;  
                }
                else // if tid==0xFFFF then clear ownTopicId
                {
                    ownTopicId[(uchar)res] = 0;    
                    flag[(uchar)res].topic_valid = 0;
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
// Make message header  
// =============================================
void HB_mqtt::make_msg_header(uchar MsgType, uint tid)
{
    begin_txmsg(&mqmsg, 0);
    add_txmsg_uchar(&mqmsg, MsgType);          
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[1]);
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[0]);    
    add_txmsg_uchar(&mqmsg, (uchar)(tid >> 8));
    add_txmsg_uchar(&mqmsg, (uchar)tid);
    MsgID = (MsgID < 0xFFFE)? MsgID+1 : 1;          
    add_txmsg_uchar(&mqmsg, (uchar)(MsgID >> 8));
    add_txmsg_uchar(&mqmsg, (uchar)(MsgID));                
    add_txmsg_uchar(&mqmsg, 1);    // JSON
 }

// =============================================
// Make a pseudo-MQTT-SN message REGISTER  
// =============================================
uchar HB_mqtt::make_msg_reg(uchar ti)
{
    mqmsg.valid = 0;
    if ((ti < MAX_TOPIC) && (flag[ti].topic_name_valid)) 
    {
        make_msg_header(MT_REGISTER, ownTopicId[ti]);
        if ((ownTopicName[ti]) && (ownTopicName[ti][0]))
        {
            add_txmsg_z_str(&mqmsg, (char*)ownTopicName[ti]);  
        }
        finish_txmsg(&mqmsg);
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
        if (len == 0)
        {
            add_txmsg_uchar(&mqmsg, '}');
        } 
        finish_txmsg(&mqmsg);
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return OK;
    }
    return ERR;
}

// =============================================
// Make message PUBLISH, default topic "time" 
// =============================================
uchar HB_mqtt::make_msg_time(ulong atime)
{
    if (atime > 0x10000000)
    {
        char tmpstr[0x40];
        ulong daysec = (atime + TIME_ZONE*60) % 86400L;
        uint len = sprintf(tmpstr, "atime:%lu, tz:%d, daysec:%lu", atime, TIME_ZONE, daysec);
        uint hr = daysec / 3600L;
        uint min = (daysec % 3600L) / 60;
        sprintf(tmpstr+len, ", hr:%d, min:%d", hr, min);
        return make_msg_publish(1, (uchar*)tmpstr, 0); // TopicId=1, text
    }
    return ERR;
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
        if (flag[ti].topic_name_valid == 0) // if TopicName invalid
        {
            flag[ti].topic_valid = 0;               // ensure     
            flag[ti].value_valid = 0;       
            state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish           
        }           
        if (ownTopicId[ti])    // if ownTopicId already valid  
        {
            flag[ti].topic_valid = 1;               // ensure            
            state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish           
        }
        else    // TopicId not valid, make request
        {
            flag[ti].topic_valid = 0;                    
            make_msg_reg(ti); // issue REGISTER with TopicId=0
            state++;
        }
        break;
    case 1:
        if (ownTopicId[ti] == 0)  // if other nodes did not supply TopicId     
        {
            ownTopicId[ti] =  (node_id << 5) | ti; // use NodeID to assign TopicId
            uint addr = EE_TOPIC_ID + 2*ti;
            EEPROM.write(addr, (uchar)(ownTopicId[ti] >> 8));
            EEPROM.write(addr+1, (uchar)ownTopicId[ti]);
            EEPROM.commit();
            flag[ti].topic_valid = 1;              
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