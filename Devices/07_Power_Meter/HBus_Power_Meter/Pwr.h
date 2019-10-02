/*
 * File     Pwr.h
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
 #ifndef __PWR_H
 #define __PWR_H

 //##############################################################################
// Inc
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Def
//##############################################################################

// Power measurement
enum{
    MAX_SNS         = 2,     // two inputs: total CT and solar CT
    MAX_MSR         = 3,     // measured values: total, solar and midpoint
    MAX_PTS         = 19,    // 19 measuremets in 20 ms, 1.024 ms interval
    AVE_PTS         = 200,   // average for 200 mains periods, 0.4 sec

    GRID_PWR_I      = 0,
    SOLAR_PWR_I     = 1,
};

//##############################################################################
// Var
//##############################################################################

class Pwr{
  public:
            Pwr(void);          // constructor
    void    clr(void);
    uchar   scnt;               // sample No after zero-crossing
    uint    mcnt;               // mains peroids counter
    union{
      uchar all_flags;
      struct{
        unsigned  mains_lost        : 1;    // set when zero-crossing could not detect mains
        unsigned  rep_mains         : 1;    // report mains lost/restored to HBus
        unsigned  rep_pwr           : 1;    // report power to HBus
        unsigned  show_pwr          : 1;    // power readings to LED strip
        unsigned  gate              : 1;
      };
    };
    int     msr[MAX_MSR];       // measurement results
    slong   vlt[MAX_PTS];       // instant voltage, estimated
    slong   icurr[MAX_SNS];     // instant current
    float   ipwr[MAX_SNS];      // instant power
    float   sum[MAX_SNS];       // power accumulated during mains period
    float   ave_sum[MAX_SNS];   // power accumulated during several mains period
    float   kwhr[MAX_SNS];      // total energy (kW*hr) for each sensor
    int     ave[MAX_SNS];       // average power
    int     displ[MAX_SNS];     // displayed power
    void    calc(uchar pts);
  private:
};

extern Pwr pwr;


 #endif /* __PWR_H */
