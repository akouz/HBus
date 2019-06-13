/*
 * File     HBtopics.cpp
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


//##############################################################################
// Inc
//##############################################################################

#include  "HBtopics.h"

//##############################################################################
// Def
//##############################################################################

#define MAX_DEF_LIST   32

//##############################################################################
// Var
//##############################################################################

HB_topics HBtopics;

// Default topics
const char def_tpc_time[] = {"time"};
const char def_tpc_tz[]  = {"timezone"};
const char def_tpc_dbg[] = {"debug"};
const char def_tpc_err[] = {"error"};
const char def_tpc_flt[] = {"fault"};

const char* def_topic_list[MAX_DEF_LIST] = {
NULL, def_tpc_time, def_tpc_tz, NULL,  NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL,      NULL, def_tpc_dbg, def_tpc_err, def_tpc_flt,
NULL, NULL, NULL, NULL,      NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL,      NULL, NULL, NULL, NULL
};

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
HB_topics::HB_topics(void)
{
    topic_list_len = 0;
    def_topic_cnt = 0;
    for (uint i=0; i<MAX_DEF_LIST; i++)
    {
        if (def_topic_list[i])
        {
            def_topic_cnt++;
        }
    }
}
// =============================================
// Read topic list from EEPROM
// =============================================
void HB_topics::init_topic_list(void)
{
    EEPROM.get(EE_TOPIC_LIST, topic_list);
    for (uint i=0; i<MAX_TOPIC_LIST; i++)
    {
        topic_list[i].TopicId = 0x100*(uint)EEPROM.read(EE_TOPIC_LIST + 4*i);
        topic_list[i].TopicId += EEPROM.read(EE_TOPIC_LIST + 4*i + 1);                 
        topic_list[i].crc = 0x100*(uint)EEPROM.read(EE_TOPIC_LIST + 4*i + 2);
        topic_list[i].crc += EEPROM.read(EE_TOPIC_LIST + 4*i + 3);                 
        if (topic_list[i].TopicId == 0)
        {
            topic_list[i].TopicId = 0xFFFF;
        }
        if (topic_list[i].TopicId == 0xFFFF)
        {
            topic_list[i].crc = 0xFFFF;
        }
    }
    sort_topics();
    for (uint i=0; i<MAX_TOPIC_LIST; i++)
    {
        EEPROM.write(EE_TOPIC_LIST + 4*i, (uchar)(topic_list[i].TopicId >> 8));    
        EEPROM.write(EE_TOPIC_LIST + 4*i + 1, (uchar)topic_list[i].TopicId);    
        EEPROM.write(EE_TOPIC_LIST + 4*i + 2, (uchar)(topic_list[i].crc >> 8));    
        EEPROM.write(EE_TOPIC_LIST + 4*i + 3, (uchar)topic_list[i].crc);    
    }
    for (uint i=0; i<MAX_TOPIC_LIST; i++)
    {
        if (topic_list[i].TopicId == 0xFFFF)
        {
            topic_list_len = i;
            break;
        }
    }
//    Serial.print("topic_list_len=");
//    Serial.println(topic_list_len);
    // SPIFFS
    if (!SPIFFS.exists("/spiffs"))
    {
        if (SPIFFS.format() == true)
        {
//            Serial.println("SPIFFS format OK");
            File f = SPIFFS.open("/spiffs", "w");
            if (f)
            {
               f.println("SPIFFS formatted");
               f.close();
            }
        }
        else
        {
//            Serial.println("SPIFFS format failed");
        }
    }
}
// =============================================
// Sort topic list by TopicId
// =============================================
void HB_topics::sort_topics(void)
{
    union trec_uni tmp;
    for (uint i=MAX_TOPIC_LIST-1; i>0; i--)
    {
        for (uint j=0; j<i; j++)
        {
            if (topic_list[j].TopicId > topic_list[j+1].TopicId)
            {
                tmp.all = topic_list[j].all;
                topic_list[j].all = topic_list[j+1].all;
                topic_list[j+1].all = tmp.all;
            }
        }
    }
    if (topic_list_len)
    {    
        if (topic_list[topic_list_len-1].TopicId == 0xFFFF) // if last topic invalid
        {
//            Serial.print(" removed #");
//            Serial.println(topic_list_len);
            topic_list_len--;
        }
    }
}
// =============================================
// Check is TopicId in the combined list
// =============================================
uint  HB_topics::is_topic_id(uint tid)
{
    if (tid)
    {
        // ----------------------------
        // check default list        
        // ----------------------------
        if (tid < MAX_DEF_LIST)
        {
            if (def_topic_list[tid]) // if topic defined
            {
                return tid; // in default list index = TopicId
            }
        }
        // ----------------------------
        // check collected list
        // ----------------------------
        else if (topic_list_len)
        {            
            if ((tid < topic_list[0].TopicId) ||
                (tid > topic_list[topic_list_len-1].TopicId))
            {
                return 0;
            }
            else
            {
                for (uint i=0; i<topic_list_len; i++)
                {
                    if (tid == topic_list[i].TopicId)
                    {
                        return i + MAX_DEF_LIST; // index in combined list
                    }
                }    
            }
        }
    }
    return 0;
}
// =============================================
// Check if CRC matches the table, return file name string
// =============================================
char*  HB_topics::is_crc(uint crc, uint* idx)
{
    if (*idx >= MAX_DEF_LIST)
    {
        while (*idx < topic_list_len+MAX_DEF_LIST)
        {
            uint i = *idx - MAX_DEF_LIST;
            if (crc == topic_list[i].crc)
            {
                if ((topic_list[i].TopicId) && 
                    (topic_list[i].TopicId < 0xFFFF))
                {             
                    return get_fname(topic_list[i].TopicId);
                }                          
            }
            (*idx)++;
        }
    }
    return NULL;
}
// =============================================
// Check is it a default topic
// =============================================
uint  HB_topics::is_def_topic(char* tn)
{
    if ((tn) && (tn[0]))
    {
        for (uint i=1; i<MAX_DEF_LIST; i++)
        {
            if (def_topic_list[i])
            {
                if (strcmp(def_topic_list[i], (const char*)tn) == 0)
                {
                    return i;
                }
            }            
        }
    }
    return 0;    
}
// =============================================
// Check is TopicName in the combined list
// =============================================
uint  HB_topics::is_topic(char* tn)
{
    if ((tn) && (tn[0]))
    {
        uint len = (uchar)strlen((const char*)tn);
        len = (len > 128)? 128 : len;
        tn[len] = 0; // ensure 0-terminated string
        uint crc = calc_crc((uchar*)tn, len);
        // ----------------------------
        // check default list        
        // ----------------------------        
        uint idx = is_def_topic(tn);
        if (idx)
        {
            return idx;   // it is default topic
        }
        // ----------------------------
        // check collected list        
        // ----------------------------
        idx = MAX_DEF_LIST;        
        while (idx < MAX_DEF_LIST+topic_list_len)
        {
            char* fn = is_crc(crc, &idx);
            if (fn == NULL)
            {
                return 0;
            }
            else if (SPIFFS.exists(fn))
            {
                File f = SPIFFS.open(fn, "r");
                uint flen = f.available();
                flen = (flen > 128)? 128 : flen;
                // use offset 0x20 to add "HBus/" (topic_root) later on
                f.read((uchar*)file_data+0x20, flen);     
                file_data[flen] = 0; // ensure
                f.close();
                if (strcmp((const char*)tn,(const char*)file_data+0x20) == 0)
                {
                    return idx; // index of combined list
                }
            }
            idx++; 
        }
    }
    return 0; // topic not in the list
}    
// =============================================
// Get TopicId
// =============================================
// idx is index in combined topic list
uint HB_topics::get_topic_id(uint idx)
{
    if (idx)
    {
        // ----------------------------
        // check default list        
        // ----------------------------        
        if (idx < MAX_DEF_LIST)
        {
            if (def_topic_list[idx]) // if default topic defined
            {
                return idx;   // in default list index = TopicId
            }
        }
        // ----------------------------
        // check collected list        
        // ----------------------------
        else 
        {
            idx -= MAX_DEF_LIST;
            if (idx < topic_list_len)
            {
                return  topic_list[idx].TopicId;
            }
        }
    }
    return 0;
}
// =============================================
// Get TopicName
// =============================================
// idx is index in combined topic list
char* HB_topics::get_topic(uint idx)
{
    if (idx)
    {
        // ----------------------------
        // check default list        
        // ----------------------------        
        if (idx < MAX_DEF_LIST)
        {
            return (char*) def_topic_list[idx];
        }
        // ----------------------------
        // check collected list        
        // ----------------------------
        else 
        {
            idx -= MAX_DEF_LIST;
            uint tid = topic_list[idx].TopicId;
            if ((tid) && (tid < 0xFFFF))
            {
                char* fn = get_fname(tid);
                if (SPIFFS.exists(fn))
                {
                    File f = SPIFFS.open(fn, "r");
                    uint flen = f.available();
                    flen = (flen > 63)? 63 : flen;
                    // use offset 0x20 to add "HBus/" (topic_root) later on
                    f.read((uchar*)file_data+0x20, flen);   
                    file_data[flen+0x20] = 0;
                    f.close();
                    return file_data+0x20;                
                }
            } 
        }
    }
    return NULL;
}
// =============================================
// Add topic descriptor to the list
// =============================================
uchar  HB_topics::add_to_list(uint tid, uint crc)
{
    uint res = is_topic_id(tid);
    if (res >= MAX_DEF_LIST)        // it is not default topic
    {
        res -= MAX_DEF_LIST; 
        if (res < topic_list_len)   // TopicId already in the list
        {
//            Serial.print(" already_on_the_list ");        
            topic_list[res].crc = crc;
            return OK;  // crc updated
        }
        else
        {
//            Serial.print(" out_of_range ");        
        }
    }
    else if (res == 0)              // TopicId is not in the list
    {
        if (topic_list_len < MAX_TOPIC_LIST)  // if can add
        {
            topic_list[topic_list_len].TopicId = tid;
            topic_list[topic_list_len].crc = crc;
            topic_list_len++;
            sort_topics();
//            Serial.print(" added ");
            return OK; // new descriptor added to the list
        }
    }    
    return ERR;
}
// =============================================
// Remove topic
// =============================================
uchar HB_topics::rem_from_list(uint idx)
{
    if ((idx >= MAX_DEF_LIST) && (idx < MAX_TOPIC_LIST+MAX_DEF_LIST))
    {
        uint i = idx - MAX_DEF_LIST;
        uint tid = topic_list[i].TopicId;
        topic_list[i].TopicId = 0xFFFF;
        topic_list[i].crc = 0xFFFF;
        if ((tid) && (tid<0xFFFF))
        {
            char* fn = get_fname(tid);
            if (SPIFFS.exists(fn))
            {
                SPIFFS.remove(fn);
                sort_topics();
                if (topic_list_len)
                {
                    topic_list_len--;
                }
                return OK;
            }                         
        }
    }
    return ERR;
}
// =============================================
// If msg is HBus TOPIC reply then add topic to the list
// =============================================
void  HB_topics::add_TOPIC(hb_msg_t* msg)
{
    if ((msg->buf[0] == 0x8B) && (msg->len>10) && (msg->buf[10])) // TOPIC reply
    {
//        Serial.print(" HB_topics::add_TOPIC ");
        uint tid = 0x100*(uint)msg->buf[8] + msg->buf[9];
        if ((tid >= MAX_DEF_LIST) && (tid < 0xFFFF)) // if TopicId valid
        {
            uchar len = msg->len-10;
            uchar* tn = msg->buf+10;
            tn[len] = 0;
//            Serial.print(" len=");
//            Serial.print(len);
            uint crc = calc_crc(tn, len); // TopicName hash
//            Serial.print(" crc=");
//            Serial.print(crc, HEX);
            uchar res = add_to_list(tid, crc);
//            Serial.print(" res=");
//            Serial.print(res);
            if (OK == res)
            {
                store_TopicName((char*)tn, tid);
            }
        }
//        Serial.println();
    }
}
// =============================================
// If msg is MQTT-SN REGISTER then add topic to the list
// =============================================
void  HB_topics::add_REGISTER(hb_msg_t* msg)
{
    if ((msg->buf[0] == 0x0A) && (msg->len>8) && (msg->buf[8])) // REGISTER
    {
//        Serial.print(" HB_topics::add_REGISTER ");
        uint tid = 0x100*(uint)msg->buf[3] + msg->buf[4];
//        Serial.print(" tid=");
//        Serial.print(tid, HEX);
        if ((tid >= MAX_DEF_LIST) && (tid < 0xFFFF)) // if TopicId valid
        {
            uchar len = msg->len-8;
            uchar* tn = msg->buf+8;
            tn[len] = 0;
            uint ti = is_topic((char*)tn);
            if (ti)
            {                
                if (OK == rem_from_list(ti))
                {
//                    Serial.print((char*)tn);
//                    Serial.print(" index=");
//                    Serial.print(ti);
//                    Serial.println(" removed");
                }  
            }            
//            Serial.print(" len=");
//            Serial.print(len);
            uint crc = calc_crc(tn, len); // TopicName hash
//            Serial.print(" crc=");
//            Serial.print(crc, HEX);
            uchar res = add_to_list(tid, crc);
//            Serial.print(" res=");
//            Serial.print(res);
            if (OK == res)
            {
                store_TopicName((char*)tn, tid);
            }
        } // if TopicId valid
//        Serial.println();
    }
}
// =============================================
// Add Gatway own topic
// =============================================
void HB_topics::add_own_topic(uint tid, char* tn)
{
    if ((tid >= MAX_DEF_LIST) && (tid < 0xFFFF) && (tn) && (tn[0]))
    {
//        Serial.print(" HB_topics::add_own_topic: tid=");
//        Serial.print(tid);
//        Serial.print(", name=");
//        Serial.print(tn);
        uchar len = (uchar)strlen((const char*)tn);
        len = (len>128)? 128 : len;
//        Serial.print(", len=");
//        Serial.print(len);
        uint crc = calc_crc((uchar *)tn, len); // TopicName hash
//        Serial.print(", crc=");
//        Serial.print(crc);
        uchar res = add_to_list(tid, crc);
        if (OK == res)
        {
            store_TopicName(tn, tid);
        }
        else
        {
//            Serial.print(" res=");
//            Serial.print(res);
        }
//        Serial.println();
    }
}
// =============================================
// Convert tid into filename
// =============================================
char* HB_topics::get_fname(uint tid)
{
    sprintf(file_name, "%cT%04X", '/', tid); // file name /Txxxx, where xxxx - TopicId in hex
    return file_name;
}
// =============================================
// Store presented TopicName (tn) in file
// =============================================
uchar HB_topics::store_TopicName(char* tn, uint tid)
{
    char* fn = get_fname(tid);
    if (SPIFFS.exists(fn))
    {
       SPIFFS.remove(fn);
    }
    File f = SPIFFS.open(fn, "w");
    f.print(tn);
    f.close();
//    Serial.print(" topic ");
//    Serial.print(tn);
//    Serial.print(" stored in file ");
//    Serial.println(fn);
    return OK;
}
// =============================================
// Print topics
// =============================================
void  HB_topics::print_topics(void)
{
    for (uint i=1; i<MAX_DEF_LIST; i++)
    {
        if (def_topic_list[i])
        {
            Serial.print(i);
            Serial.print(" - ");
            Serial.print(def_topic_list[i]);
            Serial.println();
        }
    }
    for (uint i=0; i<topic_list_len; i++)
    {
        Serial.print(topic_list[i].TopicId);
        Serial.print(" - ");
        Serial.print(get_topic(i+MAX_DEF_LIST));
        Serial.println();
    }
}

/* EOF */
