/*
 * Sketch    PIR_sensor.ino 
 * Target    HBus PIR Sensor board 

 * (c) 2019 Alex Kouznetsov,  https://github.com/akouz/hbus/HBus_PIR_Sensor
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

//##############################################################################
// Def
//##############################################################################

//#define DEBUG
//#define DEBUG_PRINT
//#define DEBUG_PWM


#ifndef BUILTIN_LED
  #define BUILTIN_LED   13
#endif

#define LED             BUILTIN_LED

enum{
    LDR     = 0, // A0 - photoresistor

    CH1     = 7,
    CH2     = 6,
    CH3     = 5,
    
    PIR1    = 4,    // microwave movement sensor RCWL-0516 at D4
    PIR2    = 3,
    
    RGB     = 11,   // output to RGB strip
    
    // toipic indexes
    PHOTO_I     = 0, 
    PIR_I       = 1,
};
 

//##############################################################################
// Descriptors
//##############################################################################

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
const uchar node_descr[8] = {
5,  // device type
2,  // device model   - PIR sensor
1,  // h/w rev major
0,  // h/w rev minor
0,  // boot rev major
1,  // boot rev minor
1,  // sketch rev major
0   // sketch rev minor
};

// -----------------------------------
// MQTT topics
// -----------------------------------
//  MAX_TOPIC defined in HBcommon.h, there are 4 topics in this demo
const uint topic_descr[MAX_TOPIC] = {
1201,   // photo sensor
1301,   // movement detector 
};


//##############################################################################
// Var
//##############################################################################

uint  PIR_cnt = 0;
uint  DIM_cnt = 0;
uchar bright = 0;
uchar PWM[3];

//##############################################################################
// Func
//##############################################################################

// ========================================
// Measure ambient light level every second  
// ========================================
void coos_task_photo(void)
{
    static uchar cnt = 0;
    static uint sum = 0;
    static uint lvl = 1600;     
    float val;
    bright = 1;
    HBmqtt.value[PHOTO_I] = 1.5;
    while(1)
    {
        sum += (1024 - analogRead(LDR));
        if (++cnt >= 8) // after 8 sec
        {
            lvl = (sum/cnt  + lvl) / 2; 
            val = lvl / 600.0;             
            HBmqtt.value[PHOTO_I] = val;
            HBmqtt.valid[PHOTO_I] = 1;
            if ((!bright) && (val >= 1.0))
            {
                bright = 1;
                HBmqtt.make_msg(PHOTO_I);   // send value promptly  
            } 
            else if ((bright) && (val <= 0.9))
            {    
                bright = 0;
                HBmqtt.make_msg(PHOTO_I);   // send value promptly  
            }
            cnt = 0;
            sum = 0;
        }
        COOS_DELAY(1000);
    }
}
// ========================================
// Two PIR or RF radar movement detectors  
// ========================================
// re-triggering for 2 min
void coos_task_PIR(void)
{
    static uchar edge1 = 0;
    static uchar edge2 = 0;
    static uchar deadtime = 0;
    while(1)
    {
        COOS_DELAY(100);   // every 100 ms
        deadtime = (deadtime)? (deadtime - 1) : 0;
        // -----------------------------------
        // detect rising edge at any PIR sensor output
        // -----------------------------------        
        edge1 <<= 1;        
        edge1 = (digitalRead(PIR1))? (edge1| 1) : edge1;
        edge2 <<= 1;        
        edge2 = (digitalRead(PIR2))? (edge2| 1) : edge2;
        if (((((edge1 & 0x3F) == 3) || (edge2 & 0x3F) == 3)) && (deadtime == 0))
        {
            PIR_cnt = 600;          // movement detected, (re)triggered for 1 min
            HBmqtt.value[PIR_I] = PIR_cnt/10.0;        
            HBmqtt.valid[PIR_I] = 1; 
            digitalWrite(LED, HIGH);
            HBmqtt.make_msg(PIR_I);   // send value promptly
            if (bright == 0) // when dark
            {
                DIM_cnt = 2*PIR_cnt;
                PWM[0] = 8;    // brightness 100%
            }
        }
        else 
        {
            digitalWrite(LED, LOW);
            if (PIR_cnt)
            {
                PIR_cnt--;
                if (PIR_cnt == 0)
                {
                    PWM[0] = 4;               // brightness 50%  
                    HBmqtt.value[PIR_I] = 0;
                    HBmqtt.make_msg(PIR_I);   // send value promptly
                    deadtime = 5;             // 0.5 sec, to prevent false triggering 
                }                
            }    
            HBmqtt.value[PIR_I] = PIR_cnt/10.0; // se
            if (DIM_cnt)
            {
                DIM_cnt--;        
                if (DIM_cnt == 0)
                {
                    PWM[0] = 0;
                    deadtime = 5;            // 0.5 sec, to prevent false triggering 
                }    
            }
            else
            {
                PWM[0] = 0;     // ensure
            }
        }
    }
}
// ========================================
// Write to MOSFET channel
// ========================================
uchar ch_no(uchar ch_index)
{
    switch(ch_index)
    {
    case 0: 
        return CH1; break;
    case 1: 
        return CH2; break;
    default: 
        return CH3; break;
    }
}
// ========================================
// PWM for MOSFET outputs   
// ========================================
void coos_task_PWM(void)
{
    uchar pwm, i;
    static uchar cnt = 0;
    while(1)
    {
        for (i=0; i<3; i++)
        {
            if (PWM[i] == 0)        // if MOSFET is always OFF
            {
                digitalWrite(ch_no(i), LOW);
            }
            else if (PWM[i] >= 7)  // if MOSFET is always ON
            {
                digitalWrite(ch_no(i), HIGH);
            }
            else  // pulsed output, 125 Hz
            {
                pwm =  7 - ((2*i + cnt) & 7);    // spread pulses
                if (pwm == 0)
                {
                    digitalWrite(ch_no(i), LOW);   // finish pulse 
                }
                else if (PWM[i] == pwm)
                {
                    digitalWrite(ch_no(i), HIGH);   // start pulse
                }                 
            }                            
        }
        COOS_DELAY(1);      // 1 ms
        cnt = (cnt+1) & 7;  // period 8 ms
    }
}
// ========================================
// Broadcast topic values    
// ========================================
// check next topic and broadcast its value if value is valid  
void coos_task_broadcast(void)
{
    static uint i;
    static uchar topic_i = 0;
    while(1)
    {
#ifdef DEBUG
        COOS_DELAY(10000);      // 10 sec
#else
        for (i=0; i<600; i++)    // 600 sec = 10 min  
        {
            COOS_DELAY(1000);   // 1 sec
        }        
#endif        
#ifdef DEBUG_PWM
        Serial.print(" PWM=");
        for (uchar i=0; i<3; i++)
        {
            Serial.print(PWM[i]);
            Serial.print(" ");
        }
#endif        
        if (HBcmd.own.ID < 0xF000) // if not a temporary ID
        {
            if (HBmqtt.valid[topic_i])   // broadcast valid values
            {
                HBmqtt.make_msg(topic_i); // prepare MQTT message with topic value,
                                          // then it will be automatically transmitted
            }    
            ++topic_i = (topic_i >= MAX_TOPIC)? 0 : topic_i;  // next topic
        }
    }
}

// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint ID)
{
    const char hdr[]  = "=== HBus PIR Sensor ==="; 
    const char txt1[] = "h/w rev = "; 
    const char txt2[] = ", sketch rev = ";  
    const char txt3[] = ", node ID = 0x"; 
    Serial.println();
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.println(hdr);
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print('=');  }
    Serial.println();
    Serial.print(txt1);
    Serial.print(node_descr[2]);
    Serial.print('.');    
    Serial.print(node_descr[3]);
    Serial.print(txt2);
    Serial.print(node_descr[6]);
    Serial.print('.');    
    Serial.print(node_descr[7]);
    Serial.print(txt3);
    Serial.println(ID, HEX);
}

// ========================================
// Setup ports
// ========================================
void setup_ports(void)
{
    pinMode(LED, OUTPUT);  
    pinMode(CH1, OUTPUT);  
    pinMode(CH2, OUTPUT);  
    pinMode(CH3, OUTPUT);  
    pinMode(RGB, OUTPUT);  
    // set spare pins as ouptuts
     pinMode(8,OUTPUT);  
     pinMode(9,OUTPUT);  
     pinMode(10,OUTPUT);  
     pinMode(12,OUTPUT);  
     pinMode(A3,OUTPUT);  
     pinMode(A2,OUTPUT);  
     pinMode(A1,OUTPUT);  
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
    HBmqtt.set_descriptor((uint*)topic_descr);          // set MQTT topics
       
    setup_ports();
    for (uchar i=0; i<3; i++)
    {
        PWM[i] = 0;
    }
    
    print_hdr_txt(HBcmd.own.ID);    // optional splash screen for debug
    wdt_enable(WDTO_120MS);                             // watchdog time-out 120 ms

    // register COOS tasks
    coos.register_task(coos_task_HBus_rxtx);            // HBus rx/tx task
    coos.register_task(coos_task_tick1ms);              // reqired for proper HBus operation               
    coos.register_task(coos_task_photo);                // photoresistor processing
    coos.register_task(coos_task_PIR);                  // movement detector
    coos.register_task(coos_task_PWM);                  // control MOSFETs
    coos.register_task(coos_task_broadcast);    

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