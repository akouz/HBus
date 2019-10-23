/*
 * file     HBmqtt.h
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

#ifndef __HB_MQTT_H
#define __HB_MQTT_H

//##############################################################################
// Inc
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Def
//##############################################################################

enum{
    // MessageType
    MT_REGISTER     = 0x0A,
    MT_PUBLISH      = 0x0C,

    // value types
    VT_NONE         = 0,
    VT_FLOAT        = 1, // default
    VT_INT          = 2,
    VT_UINT         = 3,
    VT_SLONG        = 4,
    VT_ULONG        = 5,
};

//##############################################################################
// Var
//##############################################################################

extern const char* const ownTopicName[];
extern uint ownTopicId[];

//##############################################################################
// Class
//##############################################################################

union mq_flag_uni{
    uchar all;
    struct{
        unsigned    val_type    : 4;
        unsigned    topic       : 1;
        unsigned    topic_name  : 1;
        unsigned                : 2;
    };
};
union mq_value_uni{
    float   fl;
    int     si;
    uint    ui;
    slong   sl;
    ulong   ul;
};

class HB_mqtt{
    public:
        HB_mqtt(void);
        union{
            uint all;
            struct{
                unsigned                : 10;   // not used here, those flags for HBus mode
                unsigned    ignore_ts   : 1;    // ignore time stamp mismatch for encrypted messages
                unsigned                : 2;    // not used
                unsigned    publish     : 1;    // can read unencrypted PUBLISH
                unsigned    reg         : 1;    // can read unencrypted REGISTER
                unsigned    broadcast   : 1;    // broadcast unencrypted PUBLISH and REGISTER
            };
        } allow;    // allowed unecrypted access
        hb_tx_msg_t  mqmsg;
        union mq_flag_uni   flag[MAX_TOPIC];                // set of flags
        uchar       validate_topics(void);
        uchar       rd_msg(hb_msg_t* msg);
        hb_tx_msg_t*   publish_own_val(uint idx);           // make PUBLISH message for own value
        uchar       init_topic_id(uint node_id);            // after power-up call this function
                                                            // repeatedly until it returns OK
        uint        print_own_val(uchar idx, char* buf);
        uchar       set_val_float(uchar idx, float val);
        uchar       set_val_int(uchar idx, int val);
        uchar       set_val_uint(uchar idx, uint val);
        uchar       set_val_slong(uchar idx, slong val);
        uchar       set_val_ulong(uchar idx, ulong val);
        uchar       get_val(uchar idx, float* val);
        uchar       get_val(uchar idx, int* val);
        uchar       get_val(uchar idx, uint* val);
        uchar       get_val(uchar idx, slong* val);
        uchar       get_val(uchar idx, ulong* val);

    private:
        uint        MsgID;
        ulong       MsgID_cnt;                              // count all received MQTT messages
        uint        MsgID_err_cnt;
        union mq_value_uni  value[MAX_TOPIC];               // topic values
        uchar       make_msg_register(uchar ti);            // make REGISTER message
        uchar       make_msg_publish(uint tid, uchar* buf, uchar len); // make PUBLISH message
        char        is_own_topic_name(const char* tn);
        char        is_own_topic_id(uint tid);
        void        get_MsgID(uchar msg_id);
        void        make_msg_header(uchar MsgType, uint tid); // make header
#ifdef BROADCAST_TOPIC_NAME
        uchar       add_tname(uchar idx, char* buf);        // add topic name to the string
#endif
};

extern HB_mqtt HBmqtt;

uchar copy_topic(uchar i, char* buf);

#endif /* __HB_MQTT_H */
