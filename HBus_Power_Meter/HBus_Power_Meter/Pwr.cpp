/*
 * File     Pwr.cpp
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

#include  "Pwr.h"


//##############################################################################
// Var
//##############################################################################

Pwr pwr; 

//##############################################################################
// Func
//##############################################################################

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

/* EOF */
