/*
 * File     HBmqtt.cpp 
 * Rev      1.0 dated 8/1/2019
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


//##############################################################################
// Var
//##############################################################################

HB_mqtt HBmqtt;
StaticJsonBuffer<128> jsonBuf;

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
    topic[0] = TOPIC1;
    topic[1] = TOPIC2;
    topic[2] = TOPIC3;
    topic[3] = TOPIC4;
    for (uchar i=0; i<MAX_TOPIC; i++)
    {   
        valid[i] = 0; // initially topic values are not valid
    }
}

// =============================================
// Is topic in the list of topics?  
// =============================================
char HB_mqtt::is_topic(uint tpc)
{
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        if (tpc == topic[i])
        {
            return (char)i;
        }
    }
    return -1;
}
// =============================================
// Read message and extract its value  
// =============================================
char HB_mqtt::rd_msg(hb_msg_t* msg)
{
    uint tpc =  0x100*msg->buf[3] + msg->buf[4]; // topic 
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
// Make MQTT message  
// =============================================
uchar HB_mqtt::make_msg(uchar topic_i)
{
    mqmsg.valid = 0;
    if (topic_i < MAX_TOPIC) 
    {
        begin_txmsg(&mqmsg, 0);        
        add_txmsg_uchar(&mqmsg, 0);         // message length
        add_txmsg_uchar(&mqmsg, PUBLISH);
        add_txmsg_uchar(&mqmsg, 0);     // flags
        add_txmsg_uchar(&mqmsg, (uchar)(topic[topic_i] >> 8));
        add_txmsg_uchar(&mqmsg, (uchar)(topic[topic_i]));
        add_txmsg_uchar(&mqmsg, HBcmd.own.id[1]);
        add_txmsg_uchar(&mqmsg, HBcmd.own.id[0]);
        add_txmsg_uchar(&mqmsg, 1);    // JSON
        uchar mlen = 8;                              
        char buf[32];
        sprintf(buf,"{topic:%d,val:", topic[topic_i]);
        mlen += add_txmsg_z_str(&mqmsg, buf);
        dtostrf(value[topic_i], 4,2, buf);
        mlen += add_txmsg_z_str(&mqmsg, buf);
        add_txmsg_uchar(&mqmsg, '}'); 
        mlen++;
        if (mlen == _ESC) 
        {
            add_txmsg_uchar(&mqmsg, 0);
            mlen++;
        } 
        mqmsg.buf[2] = mlen; // replace 0 by actual message length
        finish_txmsg(&mqmsg);
        mqmsg.hb = 0;
        mqmsg.valid = 1;
        return OK;
    }
    else
        return ERR;
}


/* EOF */