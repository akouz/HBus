/*
 * Sketch    White_LED_strip.ino 
 * Target    HBus MOSFET4 board 

 * (c) 2019 Alex Kouznetsov,  https://github.com/akouz/hbus/HBus_MOSFET4
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

    VCHK    = 4, // D4 - voltage check on MOSFET side
    LED1    = 7, 
    LED2    = 8,
    CH1     = 9,
    CH2     = 10,
    CH3     = 11,
    CH4     = 12,
    
    PIR     = 2,  // microwave movement sensor RCWL-0516 at D2
    
    // toipic indexes
    PHOTO_I     = 0, 
    PIR_I       = 1,
    LED_I       = 2,
    VFAIL_I     = 3, 
};
 

//##############################################################################
// Descriptors
//##############################################################################

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
const uchar node_descr[8] = {
2,  // device type
2,  // device model   - white LED strip controller
0,  // h/w rev major
1,  // h/w rev minor
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
1200,   // photo sensor
1300,   // movement detector 
2000,   // LED strip is an outside light
500,    // report MOSFET power supply failure
};


//##############################################################################
// Var
//##############################################################################

uint  PIR_cnt = 0;
uchar bright = 0;
uint  LED_timer = 0;
uchar PWM[4];

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
    static uint lvl = 0;
    float val;
    while(1)
    {
        sum += analogRead(LDR);
        if (++cnt >= 8) // after 8 sec
        {
            lvl = (sum/cnt  + lvl) / 2; 
            val = lvl / 800.0;            // const 800.0 tuned to my LDR and my personal sense of darkness 
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
// PIR or RF radar movement detector  
// ========================================
// re-triggering for 60 sec
void coos_task_PIR(void)
{
    static uchar edge = 0xFF;
    static uchar vfail;
    vfail = (digitalRead(VCHK))? 0xFF : 0;    
    HBmqtt.valid[VFAIL_I] = 1;
    while(1)
    {
        COOS_DELAY(100);   // every 100 ms
        // -----------------------------------
        // detect falling edge at PIR sensor output
        // -----------------------------------        
        edge <<= 1;        
        edge = (digitalRead(PIR))? (edge| 1) : edge;
        if ((edge & 0x0F) == 3)
        {
#ifdef DEBUG        
            PIR_cnt = 100;    // 10 sec
#else
            PIR_cnt = 600;    // movement detected, (re)triggered for 1 min
#endif            
            HBmqtt.value[PIR_I] = (float)PIR_cnt;        
            HBmqtt.valid[PIR_I] = 1; 
            digitalWrite(LED, HIGH);
            HBmqtt.make_msg(PIR_I);   // send value promptly
            if (bright == 0) // when dark
            {
                if (LED_timer)
                {
                    PWM[0] = 8; // full brightness 
                }
                else
                {
                    PWM[0] = 6;  
                }
            }
        }
        else if (PIR_cnt)
        {
            PIR_cnt--;
            HBmqtt.value[PIR_I] = (float)PIR_cnt;        
            if (PIR_cnt == 0)
            {
                digitalWrite(LED, LOW);
                HBmqtt.make_msg(PIR_I);   // send value promptly
            }
        }
        // -----------------------------------
        // also check MOSFET voltage and report changes
        // -----------------------------------
        vfail <<= 1;
        vfail = (digitalRead(VCHK))? (vfail | 1) : vfail;  
        if ((vfail & 7) == 0)
        {
            if (HBmqtt.value[VFAIL_I])
            {
                HBmqtt.value[VFAIL_I] = 0;
                HBmqtt.make_msg(VFAIL_I); // report voltage restored
            }
        }
        if ((vfail & 7) == 7) // if no MOSFET voltage
        {
            if (HBmqtt.value[VFAIL_I] == 0)
            {
                HBmqtt.value[VFAIL_I] = 1.0;
                HBmqtt.make_msg(VFAIL_I);  // report failure
            }
        }
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
        for (i=0; i<4; i++)
        {
            if (PWM[i] == 0)        // if MOSFET is always OFF
            {
                digitalWrite(CH1+i, HIGH);
            }
            else if (PWM[i] >= 7)  // if MOSFET is always ON
            {
                digitalWrite(CH1+i, LOW);
            }
            else  // pulsed output, 125 Hz
            {
                pwm =  7 - ((2*i + cnt) & 7);    // spread pulses
                if (pwm == 0)
                {
                    digitalWrite(CH1+i, HIGH);   // finish pulse 
                }
                else if (PWM[i] == pwm)
                {
                    digitalWrite(CH1+i, LOW);   // start pulse
                }                 
            }                            
        }
        COOS_DELAY(1);      // 1 ms
        cnt = (cnt+1) & 7;  // period 8 ms
        if (cnt == 0)
        {
            PWM[0] = (uchar)(8*HBmqtt.value[LED_I]);            
        }
    }
}
// ========================================
// Control LED strip at CH1   
// ========================================
// if it is dark then switch LED strip for half brightness for 4 hours
// if movement detected set full brightness  
void coos_task_LED_strip(void)
{
    static uchar LED_on = 0;
    uchar prompt;
    while(1)
    {
        prompt = 0;
        if ((bright) && (HBmqtt.value[PHOTO_I] > 1.1) && (LED_timer == 0))
        {
#ifdef DEBUG        
            LED_timer = 60; // 1 min
#else            
            LED_timer = 3600*4; // 4 hours
#endif
#ifdef DEBUG_PRINT            
            Serial.print("LED timer charged");
#endif
            HBmqtt.value[LED_I] = 0;  
            if (LED_on)
            {
                LED_on = 0;
                prompt = 1;
            }
        }
        if ((bright == 0) && (LED_timer) && (LED_on == 0))
        {
            HBmqtt.valid[LED_I] = 1;
            LED_on = 1;
            prompt = 1;
#ifdef DEBUG_PRINT        
            Serial.print("LED on");
#endif            
        }
        if (LED_on)
        {
            HBmqtt.value[LED_I] = (PIR_cnt)? 1.0 : 0.5;                 
        }
        else 
        {   
            if (bright)
            { 
                HBmqtt.value[LED_I] = 0;     // do not switch LED strip ON
            }
            else
            {
                HBmqtt.value[LED_I] = (PIR_cnt)? 0.75 : 0;
            }                 
        }
        if ((LED_timer) && (LED_on))
        {
            if (--LED_timer == 0)
            {
                LED_on = 0;  
                if (PIR_cnt == 0)
                {
                    HBmqtt.value[LED_I] = 0;
                }             
                prompt = 1;
#ifdef DEBUG_PRINT        
                Serial.print("LED timer off");
#endif                
            }
        }
        HBmqtt.value[LED_I] = PWM[0] / 8.0;
        if (prompt)
        {
            HBmqtt.make_msg(LED_I);
        }
        COOS_DELAY(1000);
    }
}
// ========================================
// Broadcast topic values    
// ========================================
// check next topic and broadcast its value if value is valid  
void coos_task_broadcast(void)
{
    static uchar i;
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
        for (uchar i=0; i<4; i++)
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
    const char hdr[]  = "=== HBus White LED Strip ==="; 
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
    pinMode(LED1, OUTPUT);  
    pinMode(LED2, OUTPUT);
    digitalWrite(CH1, HIGH);
    pinMode(CH1, OUTPUT);  
    digitalWrite(CH2, HIGH);
    pinMode(CH2, OUTPUT);  
    digitalWrite(CH3, HIGH);
    pinMode(CH3, OUTPUT);  
    digitalWrite(CH4, HIGH);
    pinMode(CH4, OUTPUT);
    // set spare pins as ouptuts
     pinMode(3,OUTPUT);  
     pinMode(5,OUTPUT);  
     pinMode(6,OUTPUT);  
     pinMode(3,OUTPUT);  
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
    for (uchar i=0; i<4; i++)
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
    coos.register_task(coos_task_LED_strip);            // control LED strip at CH1
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