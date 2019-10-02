// ===========================================================================
// Project   HBus Power Meter
// Author    Alex Kouznetsov
// Rev       1.0 dated 2/10/2019
// Target    Arduino Pro Mini on top of https://github.com/akouz/HBus/tree/master/HBus_Power_Meter board
// File      HBus_Power_Meter.ino
// ===========================================================================

/*
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

/*
 * Power meter uses two current transformers (CT) rated for 100A, ratio 2000:1.
 * One transformer measures total household current, another measures solar
 * station current. Measured power broadcasted to HBus. 1m long RGB LED strip
 * used to indicate household power balance in the range from -2kW to +2kW.
 *
 * Mains voltage assumed to be 240Vac (Australia).
 */

//##############################################################################
// Inc
//##############################################################################

#include <avr/wdt.h>
#include <FastLED.h>    // https://github.com/FastLED/FastLED
#include "HBcommon.h"
#include "HBus.h"
#include "Pwr.h"

//##############################################################################
// Def
//##############################################################################

// LED strip definitions
#define NUM_LEDS          20            // 1m 12 Vdc LED strip
#define MID_LED           (NUM_LEDS/2)  // the LED in the middle
#define BRIGHTNESS        70
#define LED_TYPE          WS2811
#define COLOR_ORDER       BRG
#define RED               0xFF0000
#define GREEN             0x00FF00
#define BLUE              0x0000FF
#define DIM               0x303030
#define GLOW              0x080808

// Pins
#define solar_in      0     // A0
#define main_in       1     // A1
#define midpt_in      2     // A2
#define zcross_pin    2     // D2
#define stby_pin      4     // D4, used as stanby in rev 1.0
#define LED_red       5     // D5
#define LED_blue      6     // D6
#define RGB_pin       11    // D11 - SPI output - LED strip control

// debug
#define     DEBUG_PRINT

enum{
    VOLT_I          = 0,        // mains voltage, present/absent
    PWR_I           = 1,        // grid power and solar power
    ENERGY_I        = 2,        // grid energy and solar energy
};

//##############################################################################
// Var
//##############################################################################

CRGB leds[NUM_LEDS];

//##############################################################################
// Func
//##############################################################################

// ========================================
// Print header text
// ========================================
void print_hdr_txt(uint cnt, uint sd, uint ID)
{
    const char hdr[] PROGMEM =  "=== HBus Power Meter ===";
    const char txt1[] PROGMEM = "Power-up cnt = ";
    const char txt2[] PROGMEM = ", restored seed = ";
    const char txt3[] PROGMEM = ", node ID = 0x";
    Serial.println();
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print(F("="));  }
    Serial.println();
    Serial.println(hdr);
    for (uchar i=0; i<strlen(hdr); i++)  {  Serial.print(F("="));  }
    Serial.println();
    Serial.print(txt1);
    Serial.print(cnt);
    Serial.print(txt2);
    Serial.print(sd);
    Serial.print(txt3);
    Serial.println(ID, HEX);
}
// =================================
// Measuring task - every 1.024 ms
// =================================
void coos_task_measure(void)
{
  //Serial.println("coos_task_measure");
  while(1)
  {
    COOS_DELAY(1);  // every 1.024 ms
    digitalWrite(LED_blue, HIGH);
    pwr.msr[0] = analogRead(main_in);         // ADC takes 110 us
    pwr.msr[1] = analogRead(solar_in);
    pwr.msr[2] = analogRead(midpt_in);
     // CT 2000:1; 1A -> 0.5mA;
     // iron: 0.3V @ 100 Ohm -> 0.003 A -> 6A -> 1440 W
     //  reading is ~1440.0 ave; thus 1 ave = 1 W rms
    pwr.icurr[0] = (slong)(pwr.msr[0] - pwr.msr[2]);
    pwr.icurr[1] = (slong)(pwr.msr[1] - pwr.msr[2]);
    if (pwr.scnt < MAX_PTS)
    {
      for (uchar i=0; i<MAX_SNS; i++)
      {
        pwr.ipwr[i] = pwr.icurr[i] * pwr.vlt[pwr.scnt];   // instant power
        if (pwr.gate)
        {
          pwr.sum[i] += (float)pwr.ipwr[i];               // accumulate power over mains period
        }
      }
    }
    pwr.scnt = (pwr.scnt < 4*MAX_PTS)? pwr.scnt+1 : pwr.scnt;
    if (pwr.scnt == MAX_PTS)          // at the end of mains period
    {
      digitalWrite(LED_blue, LOW);
      COOS_DELAY(0);
      if (pwr.gate)
      {
        digitalWrite(LED_blue, HIGH);
        pwr.calc(MAX_PTS);              // calculate
        digitalWrite(LED_blue, LOW);
      }
    }
    else if (pwr.scnt == 3*MAX_PTS)     // no zero-crossing pulses
    {
      if (!pwr.mains_lost)
      {
        pwr.rep_mains = 1;          // report mains lost
      }
      pwr.mains_lost = 1;
      digitalWrite(LED, HIGH);
#ifdef DEBUG_PRINT
      Serial.println(F("--> ERROR: Mains lost"));
#endif
    }
    if (pwr.scnt == 10)
    {
      digitalWrite(LED_red, LOW);
      if (pwr.mains_lost)
      {
        pwr.clr();                  // clear all flags too
        pwr.rep_mains = 1;          // report mains restored
        pwr.show_pwr = 1;
        digitalWrite(LED, HIGH);
#ifdef DEBUG_PRINT
        Serial.println(F("--> Mains OK"));
#endif
      }
    }
    digitalWrite(LED_blue, LOW);
  }
}
// =================================
// Detect changes
// =================================
void coos_task_detect_change(void)
{
  pwr.show_pwr = 1;
  while(1)
  {
    COOS_DELAY(250);
    digitalWrite(LED, LOW);
    COOS_DELAY(250);
    digitalWrite(LED, LOW);
    for (uchar i=0; i<MAX_SNS; i++)
    {
      if ((pwr.ave[i] > pwr.displ[i] + 10) || (pwr.ave[i] < pwr.displ[i] - 10))
      {
        pwr.show_pwr = 1;   // refresh LED strip
        pwr.rep_pwr = 1;    // report to HBus
        digitalWrite(LED, HIGH);
        pwr.displ[i] = pwr.ave[i];
      }
    }
  }
}

// ================================
// Power to LED strip
// ================================
void pwr_to_led_strip(int total, int solar)
{
   int pwr;
   ulong val;
   // -------------------------
   // export to power grid shown in blue
   // locally consumed solar power shown in green
   // -------------------------
   pwr = 0;
   for (int i=MID_LED-1; i>=0; i--)
   {
     val = 0;
     pwr += 100;    // steps 100W
     if (-total > pwr)
        val |= BLUE & DIM;
     if (solar > pwr)
        val |= GREEN & DIM;
     pwr += 100;    // steps 100W
     if (-total > pwr)
        val |= BLUE;
     if (solar > pwr)
        val |= GREEN;
     if ((i==MID_LED-1) && (solar>10))
     {
        if (total > 0)
          val |= GREEN & GLOW;
        else
          val |= BLUE & GLOW;
     }
     leds[i] = val;
   }
   // -------------------------
   // import from power grid shown in red
   // -------------------------
   pwr = 0;
   for (int i=MID_LED; i<NUM_LEDS; i++)
   {
     val = 0;
     pwr += 100;    // steps 100W
     if (total > pwr)
       val |= RED & DIM;
     pwr += 100;    // steps 100W
     if (total > pwr)
       val |= RED;
     if ((i==MID_LED) && (total>10))
       val |= RED & GLOW;
     leds[i] = val;
   }
}

// ========================================
// Indicate/report
// ========================================
void coos_task_indicate(void)
{
    static uint cnt = 0;
    //Serial.println("coos_task_indicate");
    while(1)
    {
      cnt++;
      COOS_DELAY(10);
      if ((pwr.rep_mains) || (cnt == 2000))
      {
        pwr.rep_mains = 0;
        HBmqtt.publish_own_val(VOLT_I); // prepare MQTT message, , topic "mains voltage"
        COOS_DELAY(20);
#ifdef DEBUG_PRINT
        Serial.println(F(" rep_mains"));
#endif
      }
      else if ((pwr.rep_pwr) || (cnt >= 4000))
      {
        pwr.rep_pwr = 0;
        cnt = 0;
        HBmqtt.publish_own_val(PWR_I); // prepare MQTT message, topic "grid and solar power"
        COOS_DELAY(20);
#ifdef DEBUG_PRINT
        Serial.println(F(" rep_pwr"));
#endif
      }
      else if (pwr.show_pwr)
      {
        pwr.show_pwr = 0;
        pwr_to_led_strip(pwr.displ[GRID_PWR_I], pwr.displ[SOLAR_PWR_I]);
        COOS_DELAY(1);
        FastLED.show();
#ifdef DEBUG_PRINT
        Serial.print(pwr.displ[GRID_PWR_I]);
        Serial.print(F(", "));
        Serial.print(pwr.displ[SOLAR_PWR_I]);
        Serial.println(F(" show_pwr"));
#endif
      }
    }
}

// ========================================
// Once per broadcast accumulated energy, total+solar
// ========================================
void coos_task_kwhr(void)
{
    static uchar cnt = 0;
    while(1)
    {
        COOS_DELAY(10000);   // 10 sec
        if (++cnt >= 60)     // 10 min
        {
            cnt = 0;
            HBmqtt.publish_own_val(ENERGY_I);
        }
    }
}

// =================================
// Zero-crossing ISR
// =================================
// z-cross pulse occurs when positive half cycle ends
void zcross_ISR(void)
{
  TCNT0 = 90;         // timer 0 interrupt in approx 0.5 ms
  pwr.scnt = 0;       // reset sample counter
  pwr.gate = 1;       // open the gate
  digitalWrite(LED_red, HIGH);
}

// ========================================
// Setup
// ========================================
void setup()
{
    Serial.begin(19200);
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
    print_hdr_txt(pup_cnt, node_seed, HBcmd.own.ID);    // optional splash screen for debug

    attachInterrupt(digitalPinToInterrupt(zcross_pin), zcross_ISR, FALLING);

    pinMode(LED, OUTPUT);
    pinMode(LED_red, OUTPUT);
    pinMode(LED_blue, OUTPUT);
    pinMode(stby_pin, OUTPUT);

    digitalWrite(LED, LOW);
    digitalWrite(LED_red, LOW);
    digitalWrite(LED_blue, LOW);
    digitalWrite(stby_pin, LOW);   // normal mode

    // set spare pins as outputs
    pinMode(3,OUTPUT);
    pinMode(7,OUTPUT);
    pinMode(8,OUTPUT);
    pinMode(9,OUTPUT);
    pinMode(13,OUTPUT);
    pinMode(12,OUTPUT);
    pinMode(10,OUTPUT);
    pinMode(A3,OUTPUT);
    pinMode(A4,OUTPUT);
    pinMode(A5,OUTPUT);
    pinMode(A6,OUTPUT);
    pinMode(A7,OUTPUT);


    // register COOS tasks
    coos.register_task(coos_task_HBus_rxtx);            // HBus rx/tx task
    coos.register_task(coos_task_tick1ms);              // reqired for proper HBus operation
    coos.register_task(coos_task_measure);              // measure power
    coos.register_task(coos_task_detect_change);
    coos.register_task(coos_task_indicate);
    coos.register_task(coos_task_kwhr);                 // once in 10 min

    // init registered tasks
    coos.start();
    wdt_enable(WDTO_120MS);                             // start watchdog with time-out of 120 ms
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
