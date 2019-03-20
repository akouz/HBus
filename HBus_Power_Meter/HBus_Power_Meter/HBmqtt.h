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
// Class
//##############################################################################

class HB_mqtt{
    public:
                HB_mqtt(void);
    hb_msg_t    mqmsg;
    float       value[MAX_TOPIC];           // topic values
    uchar       valid[MAX_TOPIC];           // indicate valid value
    void        set_descriptor(uint* descr);
    uint        get_topic(uchar tpc_i);     // get topic value               
    char        rd_msg(hb_msg_t* msg);    
    uchar       make_msg(uchar topic_i);    
    
    private:
    uint*       descriptor;                 // list of topics 
    char        is_topic(uint tpc);
    uint        MsgID; 
    ulong       MsgID_cnt;                  // count all received MQTT messages 
    uint        MsgID_err_cnt;
    void        get_MsgID(uint msg_id);
    void        start_msg(hb_msg_t* msg, uint tpc); 
};    

extern HB_mqtt HBmqtt;



#endif /* __HB_MQTT_H */