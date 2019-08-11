/*
 * Sketch    HBnodeMiniPro.ino - HBus rev 2 implementation for Arduino Pro Mini
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

#include <avr/wdt.h>
#include <coos.h>  // https://github.com/akouz/a_coos
#include "HBus.h"	
#include "HBcipher.h"  

//##############################################################################
// Def
//##############################################################################


//##############################################################################
// Descriptors
//##############################################################################

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
const uchar node_descr[8] = {
2,  // device type
1,  // device model
0,  // h/w rev major
1,  // h/w rev minor
0,  // boot rev major
1,  // boot rev minor, 0.1 is a native Arduino bootloader
0,  // sketch rev major
9   // sketch rev minor
};


//##############################################################################
// Func
//##############################################################################

// ========================================
// Broadcast topic values    
// ========================================
// if node has permanent ID (eg if node configured) then every 10 sec 
// check next topic and broadcast its value if value is valid  
void coos_task_broadcast(void)
{
    static uchar idx = 0;    // topic index
    static uchar topic_id_refresh = 250;
    COOS_DELAY(5000);                                   // initial pause 5 sec 
    // -------------------------------
    // loop
    // -------------------------------
    while(1)
    {
        if (++topic_id_refresh >= 200)  // after power-up and once in a while
        {
            topic_id_refresh = 0;
            // annonce own topics at HBus                        
            while (HBmqtt.init_topic_id(HBcmd.own.ID) != OK)     
            {
                COOS_DELAY(500);
            }
        } 
        COOS_DELAY(10000);  // pause 10 sec 
        if (HBcmd.own.ID < 0xF000) // if not a temporary ID
        {
            if ((HBmqtt.valid[idx].value) && (ownTopicId[idx]))  // broadcast only valid values
            {
                HBmqtt.publish_own_val(idx);      // to HBus, also prepare MQTT message
            }                
        }
    }
}

// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint cnt, uint sd, uint ID)
{
    const char hdr[] =  "=== HBnode Mini Pro ==="; 
    const char txt1[] = "Power-up cnt = "; 
    const char txt2[] = ", restored seed = ";  
    const char txt3[] = ", node ID = 0x"; 
    Serial.println();
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.println(hdr);
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
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
    pup_cnt = 0x100*EEPROM.read(EE_PUP_CNT) + EEPROM.read(EE_PUP_CNT+1);  // number of power-ups 
    node_seed = 0x100*EEPROM.read(EE_SEED) + EEPROM.read(EE_SEED+1);      
    pup_cnt = (pup_cnt >= 0xFFFE) ? 1 : (pup_cnt+1);
    if (pup_cnt < (node_seed | 0xDEAD)) // EEPROM endurance 100k write cycles
    {
        EEPROM.write(EE_PUP_CNT, (uchar)(pup_cnt >> 8));
        EEPROM.write(EE_PUP_CNT+1, (uchar)pup_cnt);
    }
    randomSeed(node_seed ^ pup_cnt);    // randomize
    // if own ID is temporary    
    if ((HBcmd.own.ID == 0) || (HBcmd.own.ID >= 0xF000))
    {
        HBcmd.own.ID = 0xF000 | random(0x1000); // then randomize it        
        EEPROM.write(EE_OWN_ID, HBcmd.own.id[1]);
        EEPROM.write(EE_OWN_ID+1, HBcmd.own.id[0]);
    }
    HBcmd.set_descriptor((uchar*)node_descr);           // set descriptor for REV command

    // cipher and security
    HBcipher.get_EE_key();
    HBcmd.read_security(HBcipher.valid);
    
    print_hdr_txt(pup_cnt, node_seed, HBcmd.own.ID);    // optional splash screen for debug
    wdt_enable(WDTO_120MS);                             // watchdog time-out 120 ms
    // register COOS tasks
    coos.register_task(coos_task_HBus_rxtx);            // HBus rx/tx task
    coos.register_task(coos_task_tick1ms);              // reqired for proper HBus operation               
    coos.register_task(coos_task_broadcast);            // as a sample...
    // init registered tasks
    coos.start();                     
}

// ========================================
// Main loop 
// ========================================
void loop()
{  
    coos.run();  // Coos scheduler 
    wdt_reset(); // service watchdog, it supposed to happen every ms, or few ms in a worst case
}

/* EOF */