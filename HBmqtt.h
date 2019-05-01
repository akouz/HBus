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

#define    MAX_TOPIC      4    // this node can handle 4 topics

const char topic0[] = "topic0";
const char topic1[] = "topic1";
const char topic2[] = "topic1/a1";
const char topic3[] = "topic1/a2";


//##############################################################################
// Var
//##############################################################################

extern const char* TopicName[MAX_TOPIC];    
extern uint TopicId[MAX_TOPIC];         

//##############################################################################
// Class
//##############################################################################

union mq_flag_uni{
    uchar all;
    struct{
        unsigned    value_valid     : 1;
        unsigned    topic_valid     : 1; 
    };
};

class HB_mqtt{
    public:
                HB_mqtt(void);
    hb_msg_t    mqmsg;
    float       value[MAX_TOPIC];           // topic values
    union mq_flag_uni   flag[MAX_TOPIC];    // set of flags
    char        rd_msg(hb_msg_t* msg);
    uchar       make_msg_pub(uchar ti);     // make PUBLISH message    
    uchar       make_msg_reg(uchar ti);     // make REGISTER message
    uchar       init_topic_id(uint node_id); // after power-up call this function  
                                            // repeatedly until it returns OK    
    
    private:
    uint*       descriptor;                 // list of topics 
    char        is_topic_name(const char* tn);
    char        is_topic_id(uint tid);
    uint        MsgID; 
    ulong       MsgID_cnt;                  // count all received MQTT messages 
    uint        MsgID_err_cnt;
    void        get_MsgID(uint msg_id);
    void        make_msg_header(uchar MsgType, uchar ti); // make header 
};    

extern HB_mqtt HBmqtt;



#endif /* __HB_MQTT_H */