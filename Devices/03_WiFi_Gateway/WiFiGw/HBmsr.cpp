/*
 * File     HBmsr.cpp
 * Target   HBus Gateway
 * Descr    Sensors and OLED display

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

#include "HBmsr.h"
#include "HBus.h"
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>   // https://github.com/adafruit/Adafruit_SSD1306
#include <BMx280MI.h>           // https://bitbucket.org/christandlg/bmx280mi/src/master/

//##############################################################################
// Def
//##############################################################################

// -----------------------------------
// Declarations for an SSD1306 display connected to I2C (SDA, SCL pins)
// -----------------------------------
#define SCREEN_WIDTH    128 // OLED display width, in pixels
#define SCREEN_HEIGHT   32  // OLED display height, in pixels
#define OLED_RESET      -1  // Reset pin -1 if sharing Arduino reset pin
// GND to COM 
// Vcc to 3.3V
// SCL to D1 (GPIO5) - Wemos pin 14
// SDA to D2 (GPIO4) - Wemos pin 13

// -----------------------------------
// BMx280 sensor connected to I2C (SDA, SCL pins)
// -----------------------------------
#define BMx280_I2C_ADDRESS 0x76
// GND to COM 
// Vcc to 3.3V
// SCL to D1 (GPIO5) - Wemos pin 14
// SDA to D2 (GPIO4) - Wemos pin 13

// -----------------------------------
// MH-Z19B sensor PWM output connected to GPIO13 
// -----------------------------------
#define CO2_pulse   13

//##############################################################################
// Var
//##############################################################################

struct msr_struct msr;

Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);
bool oled_exists;

//create a BMx280I2C object using the I2C interface with I2C Address 0x76
BMx280I2C bmx280(BMx280_I2C_ADDRESS);


//##############################################################################
// Func
//##############################################################################

// ========================================
// Measurements - CO2 by MH-Z19B
// ========================================
void coos_task_msr_CO2(void)
{
    static uint state = 0;
    static uint pwm_cnt;
    static uint tmout = 0;
    pinMode(CO2_pulse, INPUT);
    COOS_DELAY(30000);   // initial delay  to warm up sensor
    COOS_DELAY(30000);  
    while(1)
    {
        COOS_DELAY(1);
        tmout++;
        switch(state)
        {
        case 0:
            if (digitalRead(CO2_pulse) == LOW)
            {
                pwm_cnt = 0;
                state++;
            }
            else if (tmout > 2000)
            {                
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;                                    
            }
            break;
        case 1:
        case 2:
            if (digitalRead(CO2_pulse) == HIGH)
            {
                state++;
            }
            else if (tmout > 2000)
            {                
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;
                state = 0;                                    
            }
            break;
        case 3:
            if (tmout > 2000)
            {
                msr.valid.CO2 = 0;  // sensor disconnected
                tmout = 0;                                    
                state = 0;                                    
            }
            else
            {
                if (digitalRead(CO2_pulse) == HIGH)
                {
                    pwm_cnt++;
                }
                else
                {
                    state = 0;
                    tmout = 0;
                    msr.CO2 = 2.048 * pwm_cnt;  // because was measured in 1.024 ms ticks
                    msr.valid.CO2 = 1;   
                    HBmqtt.value[5] = msr.CO2;
                    HBmqtt.valid[5].value = msr.valid.CO2;   
                    COOS_DELAY(10000);  // pause 10 sec 
                }
            }        
            break;
        default:
            state = 0;
            tmout = 0;
            break;
        }
    }
}
// ========================================
// Measurements - HBus voltage and BMx280
// ========================================
void coos_task_msr_BMx280(void)
{
    static uint state = 0;    
    int tmp;
    // --------------------------------
    // init BMx280 sensor
    // --------------------------------
    if (!bmx280.begin())
	{
		Serial.println("BMx280 sensor not found");
	}
    else
    {   
        msr.valid.bmx280 = 1;
        if (bmx280.isBME280())
	       Serial.println("BME280 connected");
	    else
	   	   Serial.println("BMP280 connected");
        bmx280.resetToDefaults();
        bmx280.writeOversamplingPressure(BMx280MI::OSRS_P_x16);
	    bmx280.writeOversamplingTemperature(BMx280MI::OSRS_T_x16);
        if (bmx280.isBME280())
	       bmx280.writeOversamplingHumidity(BMx280MI::OSRS_H_x16);
    }
    COOS_DELAY(1);
    if (msr.valid.bmx280)
    {
        bmx280.measure();   
    }   
    COOS_DELAY(1000);   // initial delay 1 sec
    // --------------------------------
    // loop
    // --------------------------------
    while(1)
    {
        switch(++state)
        {
        // --------------------------------
        // measure HBus voltage
        // --------------------------------
        case 1:
            tmp = analogRead(A0);
            // additional attenuator 33k/4.7k, coeff  8.0213
            msr.HBvoltage = 8.0213 * (tmp * (3.2 / 1024.0));
            msr.valid.HBvoltage = 1;
            HBmqtt.value[1] = msr.HBvoltage;
            HBmqtt.valid[1].value = 1; 
            break;
        // --------------------------------
        // get BMx280 results
        // --------------------------------
        case 2:
            if (!msr.valid.bmx280)
            {
                uchar bmpID = bmx280.readID();
                if ((bmpID == 0x58) || (bmpID == 0x60))
                {
//                    Serial.println("BMx280 connected");
                    msr.valid.bmx280 = 1;
                }
                COOS_DELAY(1); 
            }        
            if (msr.valid.bmx280)
            {
                if (bmx280.hasValue())
                {
                    COOS_DELAY(1); 
                    msr.temperature = bmx280.getTemperature();      // C
                    msr.valid.temperature = 1;
                    COOS_DELAY(1); 
                    msr.pressure = bmx280.getPressure()/100;        // mbar  
                    msr.valid.pressure = 1;
                    if (bmx280.isBME280())
                    {
                        COOS_DELAY(1); 
                        msr.humidity = bmx280.getHumidity();        // %
                        msr.valid.humidity = 1;        
                    }                    
                    COOS_DELAY(1); 
                    bmx280.measure();  // start next measurement                     
                    HBmqtt.value[2] = msr.temperature;
                    HBmqtt.valid[2].value = msr.valid.temperature;  // C
                    HBmqtt.value[3] = msr.pressure;
                    HBmqtt.valid[3].value = msr.valid.pressure;     // mbar
                    HBmqtt.value[4] = msr.humidity;
                    HBmqtt.valid[4].value = msr.valid.humidity;     // relative, %
                }
                else
                {
//                    Serial.println("BMx280 disconnected");
                    msr.valid.bmx280 = 0;
                    msr.valid.temperature = 0;
                    msr.valid.pressure = 0;
                    msr.valid.humidity = 0;        
                    HBmqtt.valid[2].value = 0;
                    HBmqtt.valid[3].value = 0;
                    HBmqtt.valid[4].value = 0;
                }
            }
            break;
        default:
            state = 0;
            break;
        }        
        COOS_DELAY(1000);  // pause 1 sec
    }
}

// ========================================
// OLED display
// ========================================
void coos_task_OLED_display(void)
{
    static uint state = 0;
    char buf[20];
    // ----------------------------------
    // init OLED display
    // ----------------------------------
    if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) // Address 0x3C for 128x32 
    { 
        oled_exists = false;
        Serial.println("SSD1306 allocation failed");
        COOS_DELAY(COOS_STOP);  // switch off this task
    }
    else
    {
        display.clearDisplay();
        display.setTextSize(2); // Draw 2X-scale text
        display.setTextColor(WHITE);
        display.setCursor(0, 0);
        display.println(" HBus WiFi");
        display.println("  Gateway");
        display.display();      // Show initial text
    }
    COOS_DELAY(2000); // show initial text for extra 2 sec
    // ----------------------------------
    // loop
    // ----------------------------------
    while(1)
    {
        display.clearDisplay();
        display.setCursor(0, 0);
        switch(++state)
        {
        // ---------------------------
        // h/w and s/w revisions
        // ---------------------------
        case 1:  
            display.print("hw ");
            display.print(node_descr[2]);                        
            display.print(".");
            display.println(node_descr[3]);
            COOS_DELAY(10);                        
            display.print("sw ");
            display.print(node_descr[6]);                        
            display.print(".");
            display.println(node_descr[7]);     
            break;                   
        // ---------------------------
        // nodes and topics
        // ---------------------------
        case 2:  
            display.print(HBnodes.node_list_len);
            if (HBnodes.node_list_len == 1)
            {
                display.println(" node");
            }
            else 
            {
                display.println(" nodes");
            } 
            COOS_DELAY(10);                        
            display.print(HBtopics.def_topic_cnt);      // number of default topics
            display.print("+");
            display.print(HBtopics.topic_list_len);     // number of registered topics
            display.println(" topics");
            break;            
        // ---------------------------
        // connections
        // ---------------------------
        case 3:  
            if (WiFi.status() == WL_CONNECTED)
            {
                display.println("WiFi OK");
//                Serial.print("WiFi OK");
            }
            else 
            {
                display.println("no WiFi");
//                Serial.print("no WiFi");
            }
            if (MqttClient.connected())
            {
                display.println("MQTT OK");
//                Serial.println(", MQTT OK");
            }
            else 
            {
                display.println("no MQTT");
//                Serial.println(", no MQTT");
            }            
            break;             
        // ---------------------------
        // time, hh:mm 
        // ---------------------------
        case 4:
            if (msr.valid.time)
            {
                display.println("Time:");
                sprintf(buf,"%02d:%02d", coos.hour(), coos.minute());
                display.println(buf);                
            }
            else
                state++;   
        // ---------------------------
        // bus voltage
        // ---------------------------
        case 5:  
            display.println("Supply:");
            if (msr.valid.HBvoltage)
            {
                display.print(msr.HBvoltage);
                display.println(" V");            
            }
            else
            {
                display.println("unknown");            
            }
            break;
        // ---------------------------
        // MH-Z19B - CO2 level
        // ---------------------------
        case 6:
            if (msr.valid.CO2)
            {
                display.println("CO2, ppm:");
                display.println(msr.CO2);
                break;
            }
            else
                state++;  
        // ---------------------------
        // BMx280 - temperature
        // ---------------------------
        case 7:  
            if (msr.valid.temperature)
            {
                display.println("Temp:");
                display.print(msr.temperature);
                display.println(" C");            
            }
            else
                state = 0;
            break;
        // ---------------------------
        // BMx280 - pressure
        // ---------------------------
        case 8:  
            if (msr.valid.pressure)
            {
                display.println("Pressure:");
                display.println(msr.pressure);
            }
            else
                state = 0;
            break;
        // ---------------------------
        // BME280 - humidity
        // ---------------------------
        case 9:  
            if (msr.valid.humidity)
            {
                display.println("Humidity:");
                display.print(msr.humidity);
                display.println(" %");            
            }
            else
                state = 0;
            break;
        // ---------------------------
        default:
            state = 0;
            break;
        }
        display.display();
        if (state)
        {
            COOS_DELAY(4000);  // show text for 4 sec 
        }
        else
        {
            COOS_DELAY(500);  // pause 0.5 sec 
        }
    }
}

/* EOF */