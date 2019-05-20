/*
 * File     HBtopics.h
 * Target   Arduino ESP8266

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

#ifndef __HB_TOPICS_H
#define __HB_TOPICS_H

//##############################################################################
// Inc
//##############################################################################

#include  "HBcommon.h"
#include  "HBtopics.h"


//##############################################################################
// Def
//##############################################################################

#define MAX_TOPIC_LIST   0x100

// topic record
union trec_uni{
    uint32_t all;
    struct{
        uint16_t  TopicId;
        uint16_t  crc;      // TopicName hash
    };
};

//##############################################################################
// Class
//##############################################################################

class HB_topics{
    public:
                    HB_topics(void);
    void            init_topic_list(void);
    void            print_topics(void);
    void            add_TOPIC(hb_msg_t* msg);           // HBus TOPIC message
    void            add_REGISTER(hb_msg_t* msg);        // MQTT-SN REGISTER message    
    void            add_own_topic(uint tid, char* tn);  // Gateway own topic    
    uint            get_topic_id(uint idx);             // returns TopicId
    char*           get_topic(uint idx);                // returns TopicName
    uint            is_topic_id(uint tid);              // returns combined list index
    uint            is_topic(char* topic);              // returns combined list index

    private:
    uint            topic_list_len;
    union trec_uni  topic_list[MAX_TOPIC_LIST];
    char            file_name[0x10];
    char            file_data[0x100];

    void            sort_topics(void);
    char*           get_fname(uint tid);
    uchar           store_TopicName(char* tn, uint tid);
    char*           is_crc(uint crc, uint* idx);
    uint            is_def_topic(char* tn);
    uchar           add_to_list(uint tid, uint crc);
    uchar           rem_from_list(uint idx);    
};

extern HB_topics HBtopics;

#endif /* __HB_TOPICS_H */
