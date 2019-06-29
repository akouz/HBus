/*
 * file     HBmsr.h 
 * Target   HBus Gateway

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

#ifndef __HB_MSR_H
#define __HB_MSR_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Var
//##############################################################################

struct msr_struct{
    float HBvoltage;
    float temperature;
    float pressure;
    float humidity;
    float CO2;
    uint  MQTT_roundtrip;   // ms
    ulong MQTT_sent;        // millis()
    union{
        uint all;
        struct{
            unsigned    HBvoltage   : 1;
            unsigned    temperature : 1;
            unsigned    pressure    : 1;
            unsigned    humidity    : 1;
            unsigned    CO2         : 1;
            unsigned    time        : 1;
            unsigned    bmx280      : 1;
            unsigned    roundtrip   : 1;        
        };
    }valid;
    union{
        uint all;
        struct{
            unsigned    MQTT_sent   : 1;
        };
    }flag;                
};
extern struct msr_struct msr;

//##############################################################################
// Func
//##############################################################################

void coos_task_msr_CO2(void);
void coos_task_msr_BMx280(void);
void coos_task_OLED_display(void);


#endif /* __HB_MSR_H */
