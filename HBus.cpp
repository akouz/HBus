/*
 * File     HBus.cpp
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

#include "HBus.h"


//##############################################################################
// Var
//##############################################################################

uchar hb_pause_cnt;

//##############################################################################
// Func
//##############################################################################

// ========================================
// Clear receiver
// ========================================
void clr_rx(void)
{
    while (Serial.available())
    {
        hb_pause_cnt = 0;
        Serial.read();  // clear
    }
}

// ========================================
// HBus task: receieve and decode messages, send HBus reply or prompt MQTT data
// ========================================
void coos_task_HBus_rxtx(void)
{
    uchar val;
    static uchar tmout;
    static hb_msg_t* rxmsg;
    static hb_msg_t* txmsg;
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
        while (Serial.available())
        {
            hb_pause_cnt = 0;
            val = (uchar)Serial.read();  
            if (HBcmd.ignore_traffic == 0)
            {
                // --------------------------------------
                // if message completed and crc matched
                // --------------------------------------
                rxmsg = HBrxtx.rx(val);
                if (rxmsg) 
                {
                    if (HBrxtx.flag.seed == 0) 
                    {   
                        HBrxtx.flag.seed = 1; 
                        node_seed = (uint)millis(); // randomise
                        if (pup_cnt < (node_seed | 0xF00D))  // EEPROM endurance 100k write cycles
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
                        txmsg = HBcmd.process_rx_cmd(rxmsg);
                        // --------------------------
                        // if reply required
                        // --------------------------
                        HBrxtx.rtr_cnt = 0;
                        while (txmsg)  
                        {
                            // -----------------
                            // postpone transmission in first run
                            // -----------------
                            COOS_DELAY(txmsg->postpone);
                            txmsg->postpone = 0;
                            clr_rx();
                            if ((HBrxtx.rtr_cnt++ > 2) || (txmsg->len < 14))
                            {
                                txmsg->busy = 0;
                                txmsg = NULL;                                
                            }
                            // -----------------
                            // transmit
                            // -----------------                            
                            if (txmsg)                             
                            {
                                // ------------                            
                                // ensure bus is not busy
                                // ------------                            
                                tmout = 0; 
                                HBrxtx.priority = 0xFF;
                                while (hb_pause_cnt < 2)
                                {
                                    COOS_DELAY(1);
                                    clr_rx();           // receiver must be empty
                                    if (++tmout > 200)  // time-out 200 ms
                                    {
                                        txmsg = NULL;
                                        COOS_DELAY(10);
                                        Serial.flush();
                                        break;
                                    }      
                                }
                                // ------------
                                // transmit and check echo                            
                                // ------------                            
                                if ((txmsg) && (hb_pause_cnt >= 2)) 
                                {
                                    if (OK == HBrxtx.start_tx(txmsg))
                                    {
                                        uchar res = NOT_READY;
                                        while (NOT_READY == res)
                                        {
                                            COOS_DELAY(1);
                                            res = HBrxtx.tx(&hb_pause_cnt);
                                        } 
                                        if (READY == res)
                                        {
                                            Serial.flush();
                                            txmsg->busy = 0;
                                            txmsg = NULL; // success
                                        }
                                        else    // echo mismatch
                                        {
                                            Serial.end();
                                            Serial.begin(19200);                                            
                                            COOS_DELAY(10);
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
                    while (hb_pause_cnt < 2)
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
                    if ((txmsg) && (hb_pause_cnt >= 2)) 
                    {
                        if (OK == HBrxtx.start_tx(txmsg))
                        {
                            uchar res = NOT_READY;
                            while (NOT_READY == res)
                            {
                                COOS_DELAY(1);
                                res = HBrxtx.tx(&hb_pause_cnt);
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
// Task: tick 1 ms
// ========================================
void coos_task_tick1ms(void)
{
    static uchar cnt;
    while(1)
    {
        COOS_DELAY(1);
        hb_pause_cnt = (hb_pause_cnt < 200)? hb_pause_cnt+1 : hb_pause_cnt;
        if (++cnt >= 10)
        {
            cnt = 0;
            HBcmd.tick10ms();
        }
    }
}

/* EOF */
