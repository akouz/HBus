// ===========================================================================
// Project   Sole Power Meter, part of HBus project https://github.com/akouz/HBus 
// Author    Alex Kouznetsov
// Rev       1.0 dated 17/03/2019
// Target    Arduino Pro Mini
// File      Sole_power_meter.ino
// License   MIT, https://github.com/akouz/HBus/blob/master/LICENSE
// ===========================================================================

/*
 * Power meter uses two current transformers (CT). One measures
 * total household current, another measures solar station current.
 * 1m long RGB LED strip used to indicate household power balance
 * in the range from -2kW to +2kW. 
 * 
 * Sketch written for CT rated to 100A, ratio 2000:1. With CT load 
 * resistor of 100 Ohm it can measure current up to 50 A peak.
 * Mains voltage assumed to be 240Vac (Australia).
 */


//##########################################################
// Inc
//##########################################################

#include <FastLED.h>    // https://github.com/FastLED/FastLED
#include  <coos.h>      // https://github.com/akouz/a_coos
#include <avr/wdt.h>   

//##########################################################
// Def
//##########################################################

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

// Power measurement
#define MAX_SNS       2     // two inputs: total CT and solar CT
#define MAX_MSR       3     // measured values: total, solar and midpoint
#define MAX_PTS       19    // 19 measuremets in 20 ms, 1.024 ms interval
#define AVE_PTS       200   // average for 200 mains periods, 0.4 sec

// Pins
#define solar_in      0     // A0
#define main_in       1     // A1
#define midpt_in      2     // A2
#define zcross_pin    2     // D2
#define LED_red       5     // D5
#define LED_blue      6     // D6
#define RGB_pin       11    // D11 - SPI output - LED strip control
#define LED           13

//##########################################################
// Var
//##########################################################

class Pwr{
  public:
            Pwr(void);          // constructor
    void    clr(void);
    uchar   scnt;               // sample No after zero-crossing
    uint    mcnt;               // mains peroids counter
    union{
      uchar all_flags;
      struct{
        unsigned  mains_lost    : 1;
        unsigned  shown         : 1; 
        unsigned  to_show       : 1;
        unsigned  gate          : 1;
      };
    };
    int     msr[MAX_MSR];       // measurement results
    slong   vlt[MAX_PTS];       // instant voltage, estimated  
    slong   icurr[MAX_SNS];     // instant current
    float   ipwr[MAX_SNS];      // instant power 
    float   sum[MAX_SNS];       // power accumulated during mains period
    float   ave_sum[MAX_SNS];   // power accumulated during several mains period
    int     ave[MAX_SNS];       // average power 
    int     displ[MAX_SNS];     // displayed power
    void    calc(uchar pts);
  private:
};
Pwr pwr;

CRGB leds[NUM_LEDS];

Coos <4, 1> coos;   // create coos for 4 tasks, tick 1.024 ms

//##########################################################
// Func
//##########################################################

// =================================
// Constructor
// =================================
Pwr::Pwr(void)
{
  float angle, x;
  float offs = 0.246;
  x = 1.024 * PI / 10;                // 1.024 ms steps
  for (uchar i=0; i<MAX_PTS; i++)
  {
    angle = x*i + offs;
    vlt[i] = (int)(338 * sin(angle)); // 240 Vac -> 338 V peak
  }
  clr();
}
// =================================
// Clear
// =================================
void Pwr::clr(void)
{
  for (uchar i=0; i<MAX_SNS; i++)
  {
    sum[i] = 0;
    ave_sum[i] = 0;
    ave[i] = 0;
  }
  scnt = 0;
  mcnt = 0;
  all_flags = 0;  // clear all flags
}
// =================================
// Store and calculate
// =================================
void Pwr::calc(uchar pts)
{ 
  uchar i;   
  for (i=0; i<MAX_SNS; i++)
  {
    ipwr[i] = sum[i] / pts ;
    ave_sum[i] += ipwr[i];
    sum[i] = 0;    
  }  
  if (++mcnt >= AVE_PTS) 
  {
    mcnt = 0;
    for (i=0; i<MAX_SNS; i++)
    {
      ave[i] = (int)(ave_sum[i] / (AVE_PTS * 10));  // W
      ave_sum[i] = 0;
    }
  }  
}
// =================================
// Measuring task - every 1.024 ms
// =================================
void measure(void)
{
  uchar i;
  COOS_DELAY(0);
  while(1)
  {
    COOS_DELAY(1);  // every ms
    digitalWrite(LED_blue, HIGH);
    pwr.msr[0] = analogRead(main_in);         // ADC takes 110 us
    pwr.msr[1] = analogRead(solar_in);
    pwr.msr[2] = analogRead(midpt_in);    
    /*
     * CT 2000:1; 1A -> 0.5mA;  
     * iron: 0.3V @ 100 Ohm -> 0.003 A -> 6A -> 1440 W
     * reading is ~1440.0 ave; thus 1 ave = 1 W rms
     */
    pwr.icurr[0] = (slong)(pwr.msr[0] - pwr.msr[2]);
    pwr.icurr[1] = (slong)(pwr.msr[1] - pwr.msr[2]);
    if (pwr.scnt < MAX_PTS)
    {
      for (i=0; i<MAX_SNS; i++)
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
      Serial.println("--> ERROR: Mains lost");
      pwr.mains_lost = 1;
    }
    if (pwr.scnt == 10)
    {
      digitalWrite(LED_red, LOW); 
      if (pwr.mains_lost)
      {
        pwr.clr();
        Serial.println("--> Mains OK");
      }
    }
    digitalWrite(LED_blue, LOW);
  }
}
// ================================
// All LEDs black
// ================================
void leds_black(void)
{
    for(uchar i=0; i<NUM_LEDS; i++) 
    {
      leds[i] = CRGB::Black;
    }
} 
// ================================
// Half leds red
// ================================
void leds_red(void)
{
    for(uchar i=0; i<(NUM_LEDS/2); i++) 
    {
      leds[i] = CRGB::Red;
    }
} 
// ================================
// Half leds blue
// ================================
void leds_blue(void)
{
    for(uchar i=(NUM_LEDS/2); i<NUM_LEDS; i++) 
    {
      leds[i] = CRGB::Blue;
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
// =================================
// Display task
// =================================
void indicate(void)
{ 
  uchar i;
  COOS_DELAY(0);
  while(1)
  {    
    COOS_DELAY(500);
    pwr.to_show = 0;
    for (i=0; i<MAX_SNS; i++)
    {
      if (pwr.ave[i] != pwr.displ[i])      
      {
        pwr.to_show = 1;
        pwr.displ[i] = pwr.ave[i];
      }
    }
    if (((pwr.to_show) || (pwr.shown == 0)) && (pwr.mains_lost == 0)) 
    {
      Serial.print(" Total ");
      COOS_DELAY(1);
      Serial.print(pwr.displ[0]);
      COOS_DELAY(1);
      Serial.print(" W, solar ");
      COOS_DELAY(1);
      Serial.print(pwr.displ[1]);
      COOS_DELAY(1);
      Serial.println(" W ");
      COOS_DELAY(1);
      pwr_to_led_strip(pwr.displ[0], pwr.displ[1]);
      COOS_DELAY(1);
      FastLED.show();
      pwr.shown = 1;
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
// =================================
// Setup
// =================================
void setup() 
{  
  Serial.begin(19200);
  Serial.println("==================");
  Serial.println(" Sole Power Meter ");
  Serial.println("===================");
  Serial.print(millis());
  
  attachInterrupt(digitalPinToInterrupt(zcross_pin), zcross_ISR, FALLING); 
  
  pinMode(LED_red, OUTPUT); 
  pinMode(LED_blue, OUTPUT); 
  digitalWrite(LED_red, LOW); 
  digitalWrite(LED_blue, LOW); 
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
  
  digitalWrite(LED, HIGH);
  
  FastLED.addLeds<LED_TYPE, RGB_pin, COLOR_ORDER>(leds, NUM_LEDS).setCorrection( TypicalLEDStrip );
  FastLED.setBrightness(BRIGHTNESS);  

  leds_red();
  leds_blue();
  FastLED.show();

  ulong t = millis();

  while (millis() < t + 1000 )
  {
     delay(100);
  }

  leds_black();
  FastLED.show();
  digitalWrite(LED, LOW);
  Serial.print(" -- ");
  Serial.println(millis());
  
  wdt_enable(WDTO_120MS); // watchdog time-out 120 ms  
  coos.register_task(measure);
  coos.register_task(indicate);
  coos.start();
}
// =================================
// Loop
// =================================
void loop() 
{
  coos.run();
  wdt_reset();
}

/* EOF */
