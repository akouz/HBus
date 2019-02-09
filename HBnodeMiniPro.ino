/*
 * Sketch    HBnodeMiniPro.ino - HBus rev 2 implementation for Arduino Pro Mini
 * Author    A.Kouznetsov
 * Rev       1.0 dated 20/12/2018
 * Target    Arduino Pro Mini

Redistribution and use in source and binary forms, with or without modification, are permitted.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//##############################################################################
// Inc
//##############################################################################

#include "common.h"
#include "coos.h"
#include "HBrxtx.h"
#include "HBcmd.h"

//##############################################################################
// Def
//##############################################################################

#define MAX_TMP_BUF     0x40        // corresponds to about 32 ms of traffic
#define LED             BUILTIN_LED 

//##############################################################################
// Var
//##############################################################################


//uchar   rxbuf[MAX_TMP_BUF];    // tmp buffer to store UART data
//uchar   rxbuf_len;
uchar   rxpause;
//hb_msg_t rx_msg;                 // decoded message
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
// Debug: micro monitor
// ========================================
/*
uchar mon(uchar c)
{
    uchar i;
    switch (c)
    {
    // ---------------------------
    // print rxbuf
    // ---------------------------
    case 'r':   
//        Serial.println();
//        Serial.println(" == rxbuf ==");
//        for (i=0; i<rxbuf_len; i++)
//        {
//            print_val(rxbuf[i], i);
//        }
//        Serial.println();
        return 1;
        break;        
    // ---------------------------
    // print rx_msg
    // ---------------------------
    case 'R':  
//        print_buf(" == rxmsg ==", &rx_msg); 
        return 1;
        break;
    default: 
        break;
    }
    return 0;
}
*/
// ========================================
// Debug: convert hex input into binary
// ========================================
/*
int h2b(uchar c)
{
    static uchar res;
    static uchar ready = 0;
    res = (ready)? res : 0;
    if (mon(c))
        return -2;
    if ((c >= '0') && (c <= '9'))
    {
        res = (res << 4) | (c - '0');
        ready++;
    }
    else if ((c >= 'A') && (c <= 'F'))
    {
        res = (res << 4) | (c - 'A' + 0x0A);
        ready++;
    }
    else if ((c >= 'a') && (c <= 'f'))
    {
        res = (res << 4) | (c - 'a' + 0x0A);
        ready++;
    }
    else if (ready) // for single-digit numbers
    {
        ready = 2;
    }
    if (ready > 1)
    {
        ready = 0;
        return (int)res;
    }
    else
    {
        return -1;
    }
}
*/
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
// Task 0: receieve and decode data, send reply 
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
                    state = 4;
                    //Serial.print(" rxmsg ");
                    // --------------------------------
                    // process HBus message    
                    // --------------------------------
                    if (rxmsg->hb)
                    {
                        state = 5;
                        txmsg = HBcmd.process_rx_cmd(rxmsg);
                        // --------------------------
                        // if reply required
                        // --------------------------
                        HBrxtx.rtr_cnt = 0;
                        while (txmsg)  
                        {
                            state = 6;
                            //Serial.print(" hb_reply ");
                            // -----------------
                            // postpone transmission in first run
                            // -----------------
                            COOS_DELAY(txmsg->postpone);
                            txmsg->postpone = 0;
                            clr_rx();
                            if ((HBrxtx.rtr_cnt++ > 2) || (txmsg->len < 14))
                            {
                                state = 6;
                                /*
                                Serial.print(" ERR: rtr_cnt=");
                                Serial.print(HBrxtx.rtr_cnt);
                                Serial.print(", txmsg.len=");
                                Serial.print(txmsg->len);
                                Serial.println(", txmsg_released");
                                */
                                txmsg->busy = 0;
                                txmsg = NULL;                                
                            }
                            // -----------------
                            // transmit
                            // -----------------                            
                            if (txmsg)                             
                            {
                                state = 7;
                                // ------------                            
                                // ensure bus is not busy
                                // ------------                            
                                tmout = 0; 
                                HBrxtx.priority = 0xFF;
                                while (coos.pause_cnt < 2)
                                {
                                    state = 8;
                                    COOS_DELAY(1);
                                    clr_rx();           // receiver must be empty
                                    if (++tmout > 200)  // time-out 200 ms
                                    {
                                        state = 9;
                                        // Serial.println(" ERR: tx_aborted");
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
                                  /*  Serial.print(" tx ");
                                    COOS_DELAY(2);
                                    Serial.flush();
                                    while (Serial.available())
                                    {
                                        Serial.read();
                                    } */
                                    state = 10;
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
                                            state = 11;
                                            Serial.flush();
                                            /*
                                            Serial.print(" txmsg len=");
                                            Serial.print(txmsg->len);
                                            Serial.print(", txpos=");
                                            Serial.print(HBrxtx.txpos);
                                            Serial.print(", txcnt=");
                                            Serial.print(HBrxtx.txcnt);
                                            Serial.println(" done");
                                            */
                                            txmsg->busy = 0;
                                            txmsg = NULL; // success
                                        }
                                        else    // echo mismatch
                                        {
                                            state = 12;
                                            // Serial.println(" echo_err");
                                            COOS_DELAY(10);
                                            Serial.end();
                                            Serial.begin(19200);                                            
                                        }                                                                           
                                    }
                                }                                 
                            }
                        } // while txmsg
                    }
                    // --------------------------------
                    // process MQTT message
                    // --------------------------------
                    else
                    {
                        state = 12;
                    }
                }
            }    
        }
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
        COOS_DELAY(5);
        HBcmd.tick10ms();
        COOS_DELAY(5);
        if (++cnt >= 1000) // every 10 sec
        {
            cnt = 0;
            // Serial.print(" node state=");
            // Serial.println(state);
            // print_buf("== HBrxtx.rxmsg ==", &HBrxtx.rxmsg);
        }            
    }
}
// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint pup_cnt, uint seed, uint ID)
{
    const char hdr[] = {' ','H','B','n','o','d','e',' ','M','i','n','i',' ','P','r','o',' ',0};
    const char txt1[] = {'P','o','w','e','r','-','u','p',' ','c','n','t',' ','=',' ',0};
    const char txt2[] = {',',' ','s','e','e','d',' ','=',' ',0}; 
    const char txt3[] = {',',' ','o','w','n',' ','I','D',' ','=',' ','0','x',0};
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
    Serial.print(pup_cnt);    
    Serial.print(txt2);
    Serial.print(seed);
    Serial.print(txt3);
    Serial.println(ID, HEX);
}

// ========================================
// Setup
// ========================================
void setup()
{
    uint pup_cnt, seed;
    Serial.begin(19200);
    pinMode(LED, OUTPUT);
    pup_cnt = 0x100*EEPROM.read(EE_PUP_CNT) + EEPROM.read(EE_PUP_CNT+1);   
    seed = 0x100*EEPROM.read(EE_SEED) + EEPROM.read(EE_SEED+1);    
    pup_cnt = (pup_cnt >= 0xFFFE) ? 1 : (pup_cnt+1);
    EEPROM.write(EE_PUP_CNT, (uchar)(pup_cnt >> 8));
    EEPROM.write(EE_PUP_CNT+1, (uchar)pup_cnt);
    randomSeed(seed ^ pup_cnt);    
    if ((HBcmd.own.ID == 0) || (HBcmd.own.ID >= 0xF000))
    {
        HBcmd.own.ID = 0xF000 | random(0x1000);
        EEPROM.write(EE_OWN_ID, HBcmd.own.id[1]);
        EEPROM.write(EE_OWN_ID+1, HBcmd.own.id[0]);
    }
    print_hdr_txt(pup_cnt, seed, HBcmd.own.ID);
/*        
    Serial.println();
    Serial.println("=======================");
    Serial.println("=== HBnode Mini Pro ===");
    Serial.println("=======================");
    Serial.print("Power-up cnt = ");
    Serial.print(pup_cnt);    
    Serial.print(", seed = ");
    Serial.print(seed);    
    Serial.print(", own ID = 0x");
    Serial.println(HBcmd.own.ID, HEX);
*/    
    // HBrxtx.flag.debug = 1;
    // HBrxtx.flag.no_crc = 1;
    coos.register_task(coos_task0);    
    coos.register_task(coos_task1);
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
