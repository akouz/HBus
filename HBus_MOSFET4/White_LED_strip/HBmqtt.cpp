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
    descriptor = NULL;
    MsgID = 1;
    MsgID_cnt = 0;  
    MsgID_err_cnt = 0;  
    for (uchar i=0; i<MAX_TOPIC; i++)
    {   
        valid[i] = 0; // initially topic values are not valid
    }
}

// =============================================
// Set topics descriptor
// =============================================
void  HB_mqtt::set_descriptor(uint* descr)
{
    descriptor = descr;
}

// =============================================
// Get topic from descriptor
// =============================================
uint HB_mqtt::get_topic(uchar tpc_i)
{
    if (descriptor)
    {
        return descriptor[tpc_i];
    }
    else
        return 0;
}

// =============================================
// Is topic in the list of topics?  
// =============================================
char HB_mqtt::is_topic(uint tpc)
{
    if (descriptor) // if topic descriptor was set
    {
        for (uchar i=0; i<MAX_TOPIC; i++)
        {
            if (tpc == descriptor[i])
            {
                return (char)i;
            }
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
    if (MsgID_cnt < 0xFFFFFFFF)
    {
        MsgID_cnt++;
    }
    uint tpc =  0x100*msg->buf[3] + msg->buf[4];    // topic
    uint msg_id = 0x100*msg->buf[5] + msg->buf[6];  // message ID
    get_MsgID(msg_id);  
    char res =  is_topic(tpc);
    if (res >= 0)
    {
        jsonBuf.clear();            
        JsonObject& root = jsonBuf.parseObject(msg->buf+8);
        if (root.success())
        {
            value[res] = root["val"];
            valid[res] = 1; 
            blink(10);
        }
        else
        {
//            Serial.println(" JSON parser failed");
        }
    }
    return res;
}

// =============================================
// Make a pseudo-MQTT-SN message  
// =============================================
uchar HB_mqtt::make_msg(uchar topic_i)
{
    uint tpc;
    char buf[32];
    mqmsg.valid = 0;
    if (topic_i < MAX_TOPIC) 
    {
        begin_txmsg(&mqmsg, 0);
        uchar nonce = (uchar)random(0x100);
        add_txmsg_uchar(&mqmsg, nonce);        
        add_txmsg_uchar(&mqmsg, HBcmd.own.id[1]);
        add_txmsg_uchar(&mqmsg, HBcmd.own.id[0]);
        tpc = get_topic(topic_i);
        add_txmsg_uchar(&mqmsg, (uchar)(tpc >> 8));
        add_txmsg_uchar(&mqmsg, (uchar)tpc);
        MsgID = (MsgID < 0xFFFE)? MsgID+1 : 1;          
        add_txmsg_uchar(&mqmsg, (uchar)(MsgID >> 8));
        add_txmsg_uchar(&mqmsg, (uchar)(MsgID));                
        add_txmsg_uchar(&mqmsg, 1);    // JSON
        sprintf(buf,"{topic:%d,val:", tpc);
        add_txmsg_z_str(&mqmsg, buf);
        if (valid[topic_i])
        {
            dtostrf(value[topic_i], 4,2, buf);
        }
        else
        {
            buf[0] = '0';
            buf[1] = 0;
        }        
        add_txmsg_z_str(&mqmsg, buf);
        add_txmsg_uchar(&mqmsg, '}'); 
        finish_txmsg(&mqmsg);
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return OK;
    }
    else
        return ERR;
}


/* EOF */