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

#define    MAX_TOPIC      32    

const char topic0[] = "nodes";      // number of nodes on HBus
const char topic1[] = "hb/volt";    // HBus supply voltage, V
const char topic2[] = "gw/temp";    // Gateway temperature, C  (BMx280)
const char topic3[] = "gw/press";   // Gateaway atmosferic pressure, mbar (BMx280)    
const char topic4[] = "gw/hum";     // Gateway humidity, % (BME280)
const char topic5[] = "";
const char topic6[] = "";
const char topic7[] = "";
const char topic8[] = "";
const char topic9[] = "";
const char topic10[] = "";
const char topic11[] = "";
const char topic12[] = "";
const char topic13[] = "";
const char topic14[] = "";
const char topic15[] = "";
const char topic16[] = "";
const char topic17[] = "";
const char topic18[] = "";
const char topic19[] = "";
const char topic20[] = "";
const char topic21[] = "";
const char topic22[] = "";
const char topic23[] = "";
const char topic24[] = "";
const char topic25[] = "";
const char topic26[] = "";
const char topic27[] = "";
const char topic28[] = "";
const char topic29[] = "";
const char topic30[] = "";
const char topic31[] = "";

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
    hb_msg_t    mqmsg;
    float       value[MAX_TOPIC];                       // topic values
    union mq_valid_uni   valid[MAX_TOPIC];              // set of flags
    char        rd_msg(hb_msg_t* msg);
    uchar       make_msg_reg(uchar ti);                 // make REGISTER message
    uchar       make_msg_publish(uint tid, uchar* buf, uchar len); // make PUBLISH message    
    uchar       publish_own_val(uint idx);              // make PUBLISH message for own value    
    uchar       make_msg_time(ulong atime);             // make PUBLISH message topic="time"
    uchar       make_msg_err(char* txt, uint errcode);  // make PUBLISH message topic="err"
    void        read_topic_id(void);                    // restorew TopicId from EEPROM
    uchar       init_topic_id(uint node_id);            // after power-up call this function  
                                                        // repeatedly until it returns OK
    
    private:
    uint*       descriptor;                             // list of own topics 
    char        is_own_topic_name(const char* tn);
    char        is_own_topic_id(uint tid);
    uint        MsgID; 
    ulong       MsgID_cnt;                              // count all received MQTT messages 
    uint        MsgID_err_cnt;
    void        get_MsgID(uint msg_id);
    void        make_msg_header(uchar MsgType, uint tid); // make header
    char        mbuf[0x80]; 
};    

extern HB_mqtt HBmqtt;



#endif /* __HB_MQTT_H */