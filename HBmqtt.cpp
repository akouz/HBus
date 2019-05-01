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
// Def
//##############################################################################

enum{
    // MessageType
    MT_REGISTER     = 0x0A,
    MT_PUBLISH      = 0x0C,
    
};

//##############################################################################
// Var
//##############################################################################

HB_mqtt HBmqtt;

// topic
const char* TopicName[MAX_TOPIC] = {
topic0, topic1, topic2, topic3
};

uint TopicId[MAX_TOPIC];

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
HB_mqtt::HB_mqtt(void)
{
    uint tid;
    uint addr;
    mqmsg.all = 0;
    mqmsg.len = 0;
    MsgID = 1;
    MsgID_cnt = 0;  
    MsgID_err_cnt = 0;  
    for (uchar i=0; i<MAX_TOPIC; i++)
    {   
        flag[i].all = 0;   
        TopicId[i] = 0;
        addr = EE_TOPIC_ID + 2*i;
        tid = 0x100*(uint)EEPROM.read(addr) + EEPROM.read(addr+1);
        if (tid < 0xFFFF)
        {
            TopicId[i] = tid;
            flag[i].topic_valid = 1;   
        } 
    }
}

// =============================================
// Is topic name in the list?  
// =============================================
char HB_mqtt::is_topic_name(const char* tn)
{
    for (uchar ti=0; ti<MAX_TOPIC; ti++)
    {
        if (strcmp(tn, TopicName[ti]) == 0)
            return ti;         
    }
    return -1;
}


// =============================================
// Is topicID in the list?  
// =============================================
char HB_mqtt::is_topic_id(uint tid)
{
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        if (tid == TopicId[i])
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
    char res = -2;
    if (MsgID_cnt < 0xFFFFFFFF)
    {
        MsgID_cnt++;
    }
    uchar mt = msg->buf[0];                             // MsgType 
    uint tid = 0x100*(uint)msg->buf[3] + msg->buf[4];   // TopicId
    uint mid = 0x100*(uint)msg->buf[5] + msg->buf[6];   // MsgId
    get_MsgID(mid);
    // ------------------------------------
    // PUBLISH messages
    // ------------------------------------
    if (mt == MT_PUBLISH)
    {
        res =  is_topic_id(tid);
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
    if (mt == MT_REGISTER)
    {
        msg->buf[msg->len] = 0;   // make 0-terminated string
        res = is_topic_name((const char *)msg->buf + 8);
        if (res >= 0)
        {
            if (tid > 0) // if it is an assignment    
            {
                uint addr = EE_TOPIC_ID + 2*res;
                EEPROM.write(addr, (uchar)(tid >> 8));
                EEPROM.write(addr+1, (uchar)tid);
                if (tid < 0xFFFF) // if valid tid  
                {
                    TopicId[(uchar)res] = tid;   //  bind TopicId and TopicName
                    flag[(uchar)res].topic_valid = 1;  
                }
                else // if tid==0xFFFF then clear TopicId
                {
                    TopicId[(uchar)res] = 0;    
                    flag[(uchar)res].topic_valid = 0;
                }
                blink(10);
            }
            else   // if t is a query
            {
                if (TopicId[(uchar)res]) // own TopicId was assigned
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
void HB_mqtt::make_msg_header(uchar MsgType, uchar ti)
{
    begin_txmsg(&mqmsg, 0);
    add_txmsg_uchar(&mqmsg, MsgType);          
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[1]);
    add_txmsg_uchar(&mqmsg, HBcmd.own.id[0]);
    uint tpc = TopicId[ti];
    add_txmsg_uchar(&mqmsg, (uchar)(tpc >> 8));
    add_txmsg_uchar(&mqmsg, (uchar)tpc);
    MsgID = (MsgID < 0xFFFE)? MsgID+1 : 1;          
    add_txmsg_uchar(&mqmsg, (uchar)(MsgID >> 8));
    add_txmsg_uchar(&mqmsg, (uchar)(MsgID));                
    add_txmsg_uchar(&mqmsg, 1);    // JSON
 }

// =============================================
// Make a pseudo-MQTT-SN message PUBLISH  
// =============================================
uchar HB_mqtt::make_msg_pub(uchar ti)
{
    char buf[32];
    mqmsg.valid = 0;
    if (ti < MAX_TOPIC) 
    {
        if (TopicId[ti]) // if valid TopicId 
        {
            make_msg_header(MT_PUBLISH, ti);
            sprintf(buf,"{val:");
            add_txmsg_z_str(&mqmsg, buf);    // add buf as a z-string
            if (flag[ti].value_valid) 
            {
                dtostrf(value[ti], 4,2, buf);
            }
            else
            {
                buf[0] = '0';
                buf[1] = 0;
            }        
            add_txmsg_z_str(&mqmsg, buf);   // add buf as a z-string
            add_txmsg_uchar(&mqmsg, '}'); 
            finish_txmsg(&mqmsg);
            mqmsg.hb = 0;
            mqmsg.valid = 1;
            return OK;
        }
    }
    return ERR;
}

// =============================================
// Make a pseudo-MQTT-SN message REGISTER  
// =============================================
uchar HB_mqtt::make_msg_reg(uchar ti)
{
    mqmsg.valid = 0;
    if (ti < MAX_TOPIC) 
    {
        make_msg_header(MT_REGISTER, ti);
        if (TopicName[ti])
        {
            add_txmsg_z_str(&mqmsg, TopicName[ti]);  
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
// Init TopicId
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
        Serial.print("0: ti=");
        Serial.print(ti);
        Serial.print(", TopicId=");
        Serial.println(TopicId[ti]);
        if (TopicId[ti])    // if topic ID valid
        {
            flag[ti].topic_valid = 1;   // ensure            
            state = (++ti >= MAX_TOPIC) ? 99 : 0; // next topic or finish           
        }
        else    // Top[icId not valid, make request
        {
            flag[ti].topic_valid = 0;                    
            make_msg_reg(ti); // issue REGISTER with TopicId=0
            state++;
        }
        break;
    case 1:
        Serial.print("1: ti=");
        Serial.print(ti);
        Serial.print(", TopicId=");
        Serial.println(TopicId[ti]);
        if (TopicId[ti] == 0)  // if other nodes did not supply TopicId     
        {
            TopicId[ti] =  (node_id << 5) | ti; // use NodeID to assign TopicId
            uint addr = EE_TOPIC_ID + 2*ti;
            EEPROM.write(addr, (uchar)(TopicId[ti] >> 8));
            EEPROM.write(addr+1, (uchar)TopicId[ti]);
            flag[ti].topic_valid = 1;              
            make_msg_reg(ti);   // issue REGISTER with newly assigned TopicId - targeting gateways
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