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

#ifndef TOPIC0
    #define TOPIC0 ""
#endif
#ifndef TOPIC1
    #define TOPIC1 ""
#endif
#ifndef TOPIC2
    #define TOPIC2 ""
#endif
#ifndef TOPIC3
    #define TOPIC3 ""
#endif
#ifndef TOPIC4
    #define TOPIC4 ""
#endif
#ifndef TOPIC5
    #define TOPIC5 ""
#endif
#ifndef TOPIC6
    #define TOPIC6 ""
#endif
#ifndef TOPIC7
    #define TOPIC7 ""
#endif
#ifndef TOPIC8
    #define TOPIC8 ""
#endif
#ifndef TOPIC9
    #define TOPIC9 ""
#endif
#ifndef TOPIC10
    #define TOPIC10 ""
#endif
#ifndef TOPIC11
    #define TOPIC11 ""
#endif
#ifndef TOPIC12
    #define TOPIC12 ""
#endif
#ifndef TOPIC13
    #define TOPIC13 ""
#endif
#ifndef TOPIC14
    #define TOPIC14 ""
#endif
#ifndef TOPIC14
    #define TOPIC14 ""
#endif
#ifndef TOPIC15
    #define TOPIC15 ""
#endif

//##############################################################################
// Var
//##############################################################################

HB_mqtt HBmqtt;

const char topic0[] PROGMEM = TOPIC0;
const char topic1[] PROGMEM = TOPIC1;
const char topic2[] PROGMEM = TOPIC2;
const char topic3[] PROGMEM = TOPIC3;
const char topic4[] PROGMEM = TOPIC4;
const char topic5[] PROGMEM = TOPIC5;
const char topic6[] PROGMEM = TOPIC6;
const char topic7[] PROGMEM = TOPIC7;

const char topic8[] PROGMEM = TOPIC8;
const char topic9[] PROGMEM = TOPIC9;
const char topic10[] PROGMEM = TOPIC10;
const char topic11[] PROGMEM = TOPIC11;
const char topic12[] PROGMEM = TOPIC12;
const char topic13[] PROGMEM = TOPIC13;
const char topic14[] PROGMEM = TOPIC14;
const char topic15[] PROGMEM = TOPIC15;


// up to 16 topic names
const char* const ownTopicName[] PROGMEM = {
topic0,  topic1,  topic2,  topic3,   topic4,  topic5,  topic6,  topic7,
topic8,  topic9,  topic10, topic11,  topic12, topic13, topic14, topic15
};

uint ownTopicId[] = {
0, 0, 0, 0,  0, 0, 0, 0,
0, 0, 0, 0,  0, 0, 0, 0
};

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
HB_mqtt::HB_mqtt(void)
{
    this->mqmsg.all = 0;
    this->mqmsg.len = 0;
    this->MsgID = 1;
    this->MsgID_cnt = 0;
    this->MsgID_err_cnt = 0;
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        this->flag[i].all = 0;
        ownTopicId[i] = 0;
    }
}
// =============================================
// Copy topic from progmem
// =============================================
uchar copy_topic(uchar i, char* buf)
{
    uint len;
    if (i<MAX_TOPIC)
    {
        char* ptr = (char*) pgm_read_word(&ownTopicName[i]);
        if (ptr)
        {
            strcpy_P(buf, ptr);
            len = strlen(buf);
            if ((len) && (len < 0x40))
            {
                return len;
            }
        }
    }
    return 0;
}

// =============================================
// Set flags for valid topics
// =============================================
uchar HB_mqtt::validate_topics(void)
{
    uchar res = 0;
    char buf[0x80];
#ifdef DEBUG
    Serial.println(F("Topics:"));
#endif
    for (uchar i=0; i<MAX_TOPIC; i++)
    {
        if (copy_topic(i, buf))
        {
#ifdef DEBUG
            Serial.print(i);
            Serial.print(F(": topic_name="));
            Serial.println(buf);
#endif
            this->flag[i].topic_name = 1;
            res++;
        }
    }
    return res;
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
    if ((msg_id == this->MsgID+1) || ((this->MsgID == 0xFE) && (msg_id == 1)))
    {
        this->MsgID = msg_id;
    }
    else
    {
        (this->MsgID_err_cnt)++;
        if ((msg_id > this->MsgID) || ((msg_id < 0x10) && (this->MsgID > 0xF0)))
        {
            this->MsgID = msg_id;
        }
    }
}

// =============================================
// Read message and extract its values
// =============================================
uchar HB_mqtt::rd_msg(hb_msg_t* msg)
{
    static const char S_val[] = "val";
    static const char S_atime[] = "atime";
    static const char S_daysec[] = "daysec";
    schar res = -2;
    uchar mt = msg->buf[0] & 0x0F;                      // MsgType is low nibble
    uint tid = 0x100*(uint)msg->buf[3] + msg->buf[4];   // TopicId
    this->get_MsgID(msg->buf[5]);                       // MsgId
    // ------------------------------------
    // PUBLISH messages
    // ------------------------------------
    if ((mt == MT_PUBLISH) && ((msg->encrypt) || (allow.publish)))
    {
        if ((this->allow.ignore_ts) || (msg->ts_ok) || (!msg->encrypt))  // timestamp
        {
            this->MsgID_cnt = (this->MsgID_cnt < 0xFFFFFFFF)? this->MsgID_cnt+1 : 1;
            if (tid >= 0x20) // it is a user-defined topic
            {
                res =  this->is_own_topic_id(tid);
                if (res >= 0) // it is one of own topics
                {
                    uchar idx = (uchar)res;
                    jsonBuf.clear();
                    JsonObject& root = jsonBuf.parseObject(msg->buf+12);
                    if (root.success())
                    {
                        float val = root[S_val];
                        blink(10);
                        switch(this->flag[idx].val_type)
                        {
                        case VT_NONE:
                        case VT_FLOAT:
                            this->set_val_float(idx, val);
                            break;
                        case VT_INT:
                            if ((val < 32768) && (val > -32767))
                                this->set_val_int(idx, (int)val);
                            else
                                return ERR_OVERFLOW;
                            break;
                        case VT_UINT:
                            if ((val < 0x10000) && (val >= 0))
                                this->set_val_uint(idx, (uint)val);
                            else
                                return ERR_OVERFLOW;
                            break;
                        case VT_SLONG:
                            this->set_val_slong(idx, (slong)root[S_val]);
                            break;
                        case VT_ULONG:
                            if (res >= 0)
                                this->set_val_ulong(idx, (ulong)root[S_val]);
                            else
                                return ERR_OVERFLOW;
                            break;
                        default:
                            return ERR_TYPE;
                            break;
                        }
                    }
                }
            }
            else if (tid == 1) // it is pre-defined topic "time"
            {
                jsonBuf.clear();
                JsonObject& root = jsonBuf.parseObject(msg->buf+12);
                if (root.success())
                {
                    if (coos.uptime < 0x10000000) // time never been updated
                    {
                        coos.uptime =  (ulong)root[S_atime];
                        coos.daysec = (ulong)root[S_daysec];
                        blink(10);
                    }
                    else
                    {
                        ulong atime = (ulong)root[S_atime];
                        if ((atime < coos.uptime+TIME_TOLERANCE) && (atime > coos.uptime-TIME_TOLERANCE))  // +/- 10 sec
                        {
                            coos.uptime = atime;  // time accepted
                            coos.daysec = (ulong)root[S_daysec];
                            blink(10);
                        }
                    }
                }
            }
        } // timestamp OK
    }
    // ------------------------------------
    // REGISTER messages
    // ------------------------------------
    if ((mt == MT_REGISTER) && (msg->buf[8]) && ((msg->encrypt) || (allow.reg)))
    {
        if ((this->allow.ignore_ts) || (msg->ts_ok) || (!msg->encrypt))  // timestamp
        {
            this->MsgID_cnt = (this->MsgID_cnt < 0xFFFFFFFF)? (this->MsgID_cnt+1) : 1;
            msg->buf[msg->len] = 0;   // make 0-terminated string
            res = this->is_own_topic_name((const char *)msg->buf + 8);
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
                        flag[(uchar)res].topic = 1;
                    }
                    else // if tid==0xFFFF then clear ownTopicId
                    {
                        ownTopicId[(uchar)res] = 0;
                        flag[(uchar)res].topic = 0;
                    }
                    blink(10);
                }
                else   // if t is a query
                {
                    if (ownTopicId[(uchar)res]) // ownTopicId was assigned
                    {
                        this->make_msg_register((uchar)res);  // broadcast it
                        blink(10);
                    }
                }
            }
        }
    }
    return OK;
}

// =============================================
// Make message header
// =============================================
void HB_mqtt::make_msg_header(uchar MsgType, uint tid)
{
    begin_txmsg(&this->mqmsg, 0);
    uchar msb_nibble = random(0x100) & 0xF0;
    add_txmsg_uchar(&this->mqmsg, (msb_nibble | MsgType)); // MsgType
    add_txmsg_uchar(&this->mqmsg, HBcmd.own.id[1]);       // NodeId
    add_txmsg_uchar(&this->mqmsg, HBcmd.own.id[0]);
    add_txmsg_uchar(&this->mqmsg, (uchar)(tid >> 8));     // TopicId
    add_txmsg_uchar(&this->mqmsg, (uchar)tid);
    this->MsgID = (this->MsgID < 0xFE)? (this->MsgID+1) : 1;
    add_txmsg_uchar(&this->mqmsg, this->MsgID);           // MsgId
    add_txmsg_uchar(&this->mqmsg, random(0x100));         // nonce
    add_txmsg_uchar(&this->mqmsg, 1);                     // DF = JSON
    add_ts(&this->mqmsg);                                 // timestamp
 }

// =============================================
// Make a MQTT-SN message REGISTER
// =============================================
uchar HB_mqtt::make_msg_register(uchar ti)
{
    mqmsg.valid = 0;
    char buf[0x40];
    if ((ti < MAX_TOPIC) && (this->flag[ti].topic_name))
    {
        this->make_msg_header(MT_REGISTER, ownTopicId[ti]);
        copy_topic(ti, buf);    // topic name
        add_txmsg_z_str(&this->mqmsg, buf);
        finish_txmsg(&this->mqmsg);
        this->mqmsg.encrypt = (allow.broadcast)? 0 : 1;   // can send unencrypted?
        this->mqmsg.hb = 0;
        this->mqmsg.valid = 1;
        return OK;
    }
    else
        return ERR;
}

// =============================================
// Make a MQTT-SN message PUBLISH
// =============================================
// if len=0 then treat buf as a text string
uchar HB_mqtt::make_msg_publish(uint tid, uchar* buf, uchar len)
{
    mqmsg.valid = 0;
    if ((tid) && (tid < 0xFFFF))
    {
        this->make_msg_header(MT_PUBLISH, tid);
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

#ifdef BROADCAST_TOPIC_NAME
// =============================================
// Add topic name to the published message
// =============================================
uchar  HB_mqtt::add_tname(uchar idx, char* buf)
{
    char cmnt[] = ", topic:"; // 8 chars
    strcpy(buf, cmnt);
    uchar len = copy_topic(idx, buf + 8);
    if (len)
    {
//        Serial.print(buf);
        return len + 8;
    }
    return 0;
}
#endif

// =============================================
// Print value to buffer
// =============================================
uint HB_mqtt::print_own_val(uchar idx, char* buf)
{
    if (idx > MAX_TOPIC)
    {
        return 0;
    }
    switch(this->flag[idx].val_type)
    {
    case VT_FLOAT:
        dtostrf(this->value[idx].fl, 4,2, buf);
        for (uint i=0; i<0x20; i++)
        {
            if (buf[i] == 0)
                return i;
        }
        break;
    case VT_INT:
        return sprintf(buf,"%d",this->value[idx].si);
        break;
    case VT_UINT:
        return sprintf(buf,"%u",this->value[idx].ui);
        break;
    case VT_SLONG:
        return sprintf(buf,"%ld",this->value[idx].sl);
        break;
    case VT_ULONG:
        return sprintf(buf,"%lu",this->value[idx].ul);
        break;
    default:
        break;
    }
    buf[0] = '0';
    buf[1] = 0;
    return 1;
}

// =============================================
// Set own value
// =============================================
uchar  HB_mqtt::set_val_float(uchar idx, float val)
{
    if (this->flag[idx].val_type == VT_NONE) { this->flag[idx].val_type = VT_FLOAT; }
    if (this->flag[idx].val_type == VT_FLOAT)
    {
        this->value[idx].fl = val;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::set_val_int(uchar idx, int val)
{
    if (this->flag[idx].val_type == VT_NONE) { this->flag[idx].val_type = VT_INT; }
    if (this->flag[idx].val_type == VT_INT)
    {
        this->value[idx].si = val;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::set_val_uint(uchar idx, uint val)
{
    if (this->flag[idx].val_type == VT_NONE) { this->flag[idx].val_type = VT_UINT; }
    if (this->flag[idx].val_type == VT_UINT)
    {
        this->value[idx].ui = val;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::set_val_slong(uchar idx, slong val)
{
    if (this->flag[idx].val_type == VT_NONE) { this->flag[idx].val_type = VT_SLONG; }
    if (this->flag[idx].val_type == VT_SLONG)
    {
        this->value[idx].sl = val;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::set_val_ulong(uchar idx, ulong val)
{
    if (this->flag[idx].val_type == VT_NONE) { this->flag[idx].val_type = VT_ULONG; }
    if (this->flag[idx].val_type == VT_ULONG)
    {
        this->value[idx].ul = val;
        return OK;
    }
    return ERR_TYPE;
}
// =============================================
// Get own value
// =============================================
uchar  HB_mqtt::get_val(uchar idx, float* val)
{
    if (this->flag[idx].val_type == VT_FLOAT)
    {
        *val = this->value[idx].fl;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::get_val(uchar idx, int* val)
{
    if (this->flag[idx].val_type == VT_INT)
    {
        *val = this->value[idx].si;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::get_val(uchar idx, uint* val)
{
    if (this->flag[idx].val_type == VT_UINT)
    {
        *val = this->value[idx].ui;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::get_val(uchar idx, slong* val)
{
    if (this->flag[idx].val_type == VT_SLONG)
    {
        *val = this->value[idx].sl;
        return OK;
    }
    return ERR_TYPE;
}
uchar  HB_mqtt::get_val(uchar idx, ulong* val)
{
    if (this->flag[idx].val_type == VT_ULONG)
    {
        *val = this->value[idx].ul;
        return OK;
    }
    return ERR_TYPE;
}

// =============================================
// PUBLISH own value  to HBus and to MQTT
// =============================================
hb_tx_msg_t* HB_mqtt::publish_own_val(uint idx)
{
    if (idx < MAX_TOPIC)
    {
        uint tid = ownTopicId[idx]; // topic ID
        if (tid)
        {
            char   mbuf[0x40];
            this->make_msg_header(MT_PUBLISH, tid);
            uint len = sprintf(mbuf,"{val:");
            len += this->print_own_val(idx, mbuf+len);
    #ifdef BROADCAST_TOPIC_NAME
            len += this->add_tname(idx, mbuf+len);
    #endif
            mbuf[len++] = '}';
            mbuf[len] = 0;
            add_txmsg_z_str(&mqmsg, mbuf);              // add mbuf as a z-string to HBus message
            finish_txmsg(&mqmsg);                       // finish message to HBus
            mqmsg.encrypt = (allow.broadcast)? 0 : 1;   // can send unencrypted?
            mqmsg.hb = 0;
            mqmsg.valid = 1;
            return &mqmsg;
        }
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
        if (flag[ti].topic_name == 0) // if TopicName invalid
        {
            flag[ti].topic = 0;                    // ensure
            flag[ti].val_type = VT_NONE;
            state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish
        }
        else // if TopicName valid
        {
            if (ownTopicId[ti])    // if ownTopicId already valid
            {
                flag[ti].topic = 1;                    // ensure
                state = (++ti >= MAX_TOPIC) ? 99 : 0;   // next topic or finish
            }
            else    // TopicId not valid, make request
            {
                flag[ti].topic = 0;
                this->make_msg_register(ti); // issue REGISTER with TopicId=0
                state++;
            }
        }
        break;
    case 1:
        if (ownTopicId[ti] == 0)  // if other nodes did not supply TopicId
        {
            ownTopicId[ti] =  (node_id << 5) | ti; // use NodeID to assign TopicId
            uint addr = EE_TOPIC_ID + 2*ti;
            EEPROM.write(addr, (uchar)(ownTopicId[ti] >> 8));
            EEPROM.write(addr+1, (uchar)ownTopicId[ti]);
            flag[ti].topic = 1;
            this->make_msg_register(ti);   // issue REGISTER with newly assigned ownTopicId - targeting gateways
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
