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

#define   BROADCAST_TOPIC_NAME             // add tname="topic_name" to PUBLISH message

#define    MAX_TOPIC      8     // because of limited memory in Arduino Pro Mini    

const char topic0[] = ""; 
const char topic1[] = ""; 
const char topic2[] = ""; 
const char topic3[] = "";     
const char topic4[] = ""; 
const char topic5[] = ""; 
const char topic6[] = "";
const char topic7[] = "";

enum{
    // MessageType
    MT_REGISTER     = 0x0A,
    MT_PUBLISH      = 0x0C,    
};

//##############################################################################
// Var
//##############################################################################

extern const char* ownTopicName[MAX_TOPIC];    
extern uint ownTopicId[MAX_TOPIC];         

//##############################################################################
// Class
//##############################################################################

union mq_valid_uni{
    uint all;
    struct{
        unsigned    value       : 1;
        unsigned    topic       : 1;
        unsigned    topic_name  : 1;
    };
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
    hb_msg_t    mqmsg;
    float       value[MAX_TOPIC];                       // topic values
    union mq_valid_uni   valid[MAX_TOPIC];              // set of flags
    char        rd_msg(hb_msg_t* msg);
    uchar       make_msg_reg(uchar ti);                 // make REGISTER message
    uchar       make_msg_publish(uint tid, uchar* buf, uchar len); // make PUBLISH message    
    hb_msg_t*   publish_own_val(uint idx);             // make PUBLISH message for own value    
    uchar       make_msg_err(char* txt, uint errcode);  // make PUBLISH message topic="err"
    void        read_topic_id(void);                    // restorew TopicId from EEPROM
    uchar       init_topic_id(uint node_id);            // after power-up call this function  
                                                        // repeatedly until it returns OK
    void        add_signature(char* buf, uint* len);
    uchar       is_signature(char* buf);
    
    private:
    uint*       descriptor;                             // list of own topics 
    char        is_own_topic_name(const char* tn);
    char        is_own_topic_id(uint tid);
    uint        MsgID; 
    ulong       MsgID_cnt;                              // count all received MQTT messages 
    uint        MsgID_err_cnt;
    void        get_MsgID(uchar msg_id);
    void        make_msg_header(uchar MsgType, uint tid); // make header
    char        mbuf[0x40];
#ifdef BROADCAST_TOPIC_NAME
    uchar       add_tname(uchar idx, char* buf);       // add topic name to the string
#endif     
};    

extern HB_mqtt HBmqtt;



#endif /* __HB_MQTT_H */