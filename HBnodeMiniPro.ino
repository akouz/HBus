/*
 * Sketch    HBnodeMiniPro.ino - HBus rev 2 implementation for Arduino Pro Mini
 * Author    A.Kouznetsov     (https://github.com/akouz)
 * Rev       0.2 dated 19/02/2019
 * Target    Arduino Pro Mini

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

#include "common.h"
#include "coos.h"
#include "HBrxtx.h" 
#include "HBcmd.h"
#include "HBmqtt.h"

//##############################################################################
// Def
//##############################################################################

#define LED             BUILTIN_LED 

//##############################################################################
// Var
//##############################################################################

uchar   rxpause;
uchar  state;

//##############################################################################
// Func
//##############################################################################

// ========================================
// Debug: print hex value
// ========================================
uchar print_val(uchar val, uchar i)
{
    if (val < 0x10)
        Serial.print("0");
    Serial.print(val, HEX);
    Serial.print(" ");
    if ((i & 7) == 7)
        Serial.print(" ");   
    if ((i & 0x1F) == 0x1F)
    {
        Serial.println();
        return 1;
    }
    else
    {   
        return 0;
    }    
}

// ========================================
// Debug: print message buffer
// ========================================
void print_buf(const char* name, hb_msg_t* msg)
{
    uchar nl = 1;
    Serial.println();
    Serial.println(name);
    if (msg->hb)
        Serial.print(" hb");
    else    
        Serial.print(" mqtt");
    if (msg->busy)
        Serial.print(" busy");
    if (msg->valid)
        Serial.print(" valid");
    if (msg->esc)
        Serial.print(" esc");
    if (msg->gate)
        Serial.print(" gate");
    Serial.println();           
    for (uchar i=0; i < msg->len; i++)
    {
        nl = print_val(msg->buf[i], i);
    }
    if (nl == 0)
        Serial.println();
}

// ========================================
// Clear receiver
// ========================================
void clr_rx(void)
{
    while (Serial.available())
    {
        coos.pause_cnt = 0;
        Serial.read();  // clear
    }
}

// ========================================
// Task 0: receieve and decode messages, send HBus reply or prompt MQTT data
// ========================================
void coos_task0(void)
{
    uchar val;
    static uchar tmout;
    static hb_msg_t* rxmsg;
    static hb_msg_t* txmsg;
    state = 0;
    COOS_DELAY(1);
    // ---------------------------
    // loop
    // ---------------------------
    while(1)
    {
        COOS_DELAY(1);
        // -----------------------------------------------
        // Process incoming HBus messages
        // -----------------------------------------------
        state = 1;
        while (Serial.available())
        {
            state = 2;
            coos.pause_cnt = 0;
            val = (uchar)Serial.read();  
            if (HBcmd.ignore_traffic == 0)
            {
                state = 3;
                // --------------------------------------
                // if message completed and crc matched
                // --------------------------------------
                rxmsg = HBrxtx.rx(val);
                if (rxmsg) 
                {
                    state = 0x10;
                    if (HBrxtx.flag.seed == 0) 
                    {   
                        HBrxtx.flag.seed = 1; 
                        node_seed = (uint)millis(); // randomise
                        if (pup_cnt < (node_seed | 0x0C27))  // EEPROM endurance 100k write cycles
                        {  
                            EEPROM.write(EE_SEED, (uchar)(node_seed >> 8));
                            EEPROM.write(EE_SEED+1, (uchar)node_seed);
                        }
                    }                    
                    // --------------------------------
                    // process HBus message    
                    // --------------------------------
                    if (rxmsg->hb)
                    {
                        state = 0x11;
                        txmsg = HBcmd.process_rx_cmd(rxmsg);
                        // --------------------------
                        // if reply required
                        // --------------------------
                        HBrxtx.rtr_cnt = 0;
                        while (txmsg)  
                        {
                            state = 0x12;
                            // -----------------
                            // postpone transmission in first run
                            // -----------------
                            COOS_DELAY(txmsg->postpone);
                            txmsg->postpone = 0;
                            clr_rx();
                            if ((HBrxtx.rtr_cnt++ > 2) || (txmsg->len < 14))
                            {
                                state = 0x13;
                                txmsg->busy = 0;
                                txmsg = NULL;                                
                            }
                            // -----------------
                            // transmit
                            // -----------------                            
                            if (txmsg)                             
                            {
                                state = 0x14;
                                // ------------                            
                                // ensure bus is not busy
                                // ------------                            
                                tmout = 0; 
                                HBrxtx.priority = 0xFF;
                                while (coos.pause_cnt < 2)
                                {
                                    state = 0x15;
                                    COOS_DELAY(1);
                                    clr_rx();           // receiver must be empty
                                    if (++tmout > 200)  // time-out 200 ms
                                    {
                                        state = 0x16;
                                        txmsg = NULL;
                                        COOS_DELAY(10);
                                        Serial.flush();
                                        break;
                                    }      
                                }
                                // ------------
                                // transmit and check echo                            
                                // ------------                            
                                if ((txmsg) && (coos.pause_cnt >= 2)) 
                                {
                                    state = 0x17;
                                    if (OK == HBrxtx.start_tx(txmsg))
                                    {
                                        uchar res = NOT_READY;
                                        while (NOT_READY == res)
                                        {
                                            COOS_DELAY(1);
                                            res = HBrxtx.tx(&coos.pause_cnt);
                                        } 
                                        if (READY == res)
                                        {
                                            state = 0x18;
                                            Serial.flush();
                                            txmsg->busy = 0;
                                            txmsg = NULL; // success
                                        }
                                        else    // echo mismatch
                                        {
                                            state = 0x19;
                                            COOS_DELAY(10);
                                            Serial.end();
                                            Serial.begin(19200);                                            
                                        }                                                                           
                                    } // if tx started
                                } // if pause on the bus                                 
                            } // if txmsg
                        } // while txmsg
                    } // if HBus message
                    // --------------------------------
                    // process received MQTT message
                    // --------------------------------
                    else
                    {
                        state = 0x20;
                        HBmqtt.rd_msg(rxmsg);
                        rxmsg->busy = 0;
                    }
                }
            }    
        } // while Serial.available
        // -----------------------------------------------
        // Broadcast MQTT-SN messages to the bus 
        // -----------------------------------------------
        if (HBmqtt.mqmsg.valid)
        {
            HBmqtt.mqmsg.busy = 1;
            txmsg = &HBmqtt.mqmsg;
            HBrxtx.rtr_cnt = 0;
            while(txmsg)
            {
                if (HBrxtx.rtr_cnt++ > 2)
                {
                    txmsg = NULL;                                
                }
                if (txmsg)
                {
                    tmout = 0; 
                    HBrxtx.priority = 0xFF;
                    while (coos.pause_cnt < 2)
                    {
                        COOS_DELAY(1);
                        clr_rx();           // receiver must be empty
                        if (++tmout > 200)  // time-out 200 ms
                        {
                            Serial.println(" Time-out");
                            txmsg->valid = 0;
                            txmsg->busy = 0;
                            txmsg = NULL;
                            COOS_DELAY(10);
                            Serial.flush();
                            break;
                        }      
                    }
                    if ((txmsg) && (coos.pause_cnt >= 2)) 
                    {
                        if (OK == HBrxtx.start_tx(txmsg))
                        {
                            uchar res = NOT_READY;
                            while (NOT_READY == res)
                            {
                                COOS_DELAY(1);
                                res = HBrxtx.tx(&coos.pause_cnt);
                            } 
                            if (READY == res)
                            {
                                Serial.flush();
                                txmsg->busy = 0;
                                txmsg->valid = 0;
                                txmsg = NULL; // success
                            }
                            else    // echo mismatch
                            {
                                COOS_DELAY(10);
                                Serial.end();
                                Serial.begin(19200);                                            
                            }                                                                           
                        } // if tx started
                    }     
                }
            }  // while txmsg
            HBmqtt.mqmsg.busy = 0;   // ensure
            HBmqtt.mqmsg.valid = 0;         
        } // if mqmsg.valid
    }
}

// ========================================
// Task 1: tick 10 ms
// ========================================
void coos_task1(void)
{
    static uint cnt = 0;
    while(1)
    {
        COOS_DELAY(10);
        HBcmd.tick10ms();
    }
}

// ========================================
// Task 2: if node has permanent ID (eg if node configured)   
// ========================================
// then every 10 sec prepare MQTT message with next topic value 
void coos_task2(void)
{
    static uchar topic_i = 0;
    while(1)
    {
        COOS_DELAY(10000);  // pause 10 sec
        if ((HBcmd.own.ID & 0xF000) != 0xF000) // if not temporary ID
        {
            HBmqtt.make_msg(topic_i++);    // prepare MQTT message with topic value
            topic_i = (topic_i >= MAX_TOPIC)? 0 : topic_i;  // next topic
        }
    }
}

// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint cnt, uint sd, uint ID)
{
    const char hdr[] =  " HBnode Mini Pro "; 
    const char txt1[] = "Power-up cnt = "; 
    const char txt2[] = ", restored seed = ";  
    const char txt3[] = ", node ID = 0x"; 
    Serial.println();
    for (uchar i=0; i<23; i++)  {  Serial.print('=');  }
    Serial.println();
    for (uchar i=0; i<3; i++)  {  Serial.print('=');  }
    Serial.print(hdr);
    for (uchar i=0; i<3; i++)  {  Serial.print('=');  }
    Serial.println();
    for (uchar i=0; i<23; i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.print(txt1);
    Serial.print(cnt);    
    Serial.print(txt2);
    Serial.print(sd);
    Serial.print(txt3);
    Serial.println(ID, HEX);
}

// ========================================
// Setup
// ========================================
void setup()
{
    Serial.begin(19200);
    pinMode(LED, OUTPUT);
    pup_cnt = 0x100*EEPROM.read(EE_PUP_CNT) + EEPROM.read(EE_PUP_CNT+1);   
    node_seed = 0x100*EEPROM.read(EE_SEED) + EEPROM.read(EE_SEED+1);    
    pup_cnt = (pup_cnt >= 0xFFFE) ? 1 : (pup_cnt+1);
    if (pup_cnt < (node_seed | 0x8A53))  // EEPROM endurance 100k write cycles
    {
        EEPROM.write(EE_PUP_CNT, (uchar)(pup_cnt >> 8));
        EEPROM.write(EE_PUP_CNT+1, (uchar)pup_cnt);
    }
    randomSeed(node_seed ^ pup_cnt);    
    if ((HBcmd.own.ID == 0) || (HBcmd.own.ID >= 0xF000))
    {
        HBcmd.own.ID = 0xF000 | random(0x1000);        
        EEPROM.write(EE_OWN_ID, HBcmd.own.id[1]);
        EEPROM.write(EE_OWN_ID+1, HBcmd.own.id[0]);
    }
    print_hdr_txt(pup_cnt, node_seed, HBcmd.own.ID);
    coos.register_task(coos_task0);    
    coos.register_task(coos_task1);
    coos.register_task(coos_task2);
    coos.start();                     // init registered tasks
}

// ========================================
// Main loop 
// ========================================
void loop()
{  
    coos.run();  // Coos scheduler 
}

/* EOF */