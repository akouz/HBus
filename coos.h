/*
 * Library   COOS - COoperative Operating System
 * Author    A.Kouznetsov
 * Rev       1.2 dated 14/07/2018
 * Target    Arduino

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
 
#ifndef __COOS_H
#define __COOS_H

//##############################################################################
// Inc
//##############################################################################

#include <Arduino.h>
#include <setjmp.h>
#include "common.h"

//##############################################################################
// Type                                                   
//##############################################################################

#ifndef __UCHAR_DEFINED__
  #define __UCHAR_DEFINED__
  typedef unsigned char uchar;
  typedef signed   char schar;
  typedef unsigned int  uint;
  typedef unsigned long ulong;
  typedef signed   long slong;
#endif

//##############################################################################
// Def                                              
//##############################################################################

#define COOS_MAX_TASKS       4     // max number of tasks, set as many as required (up to 255)
#define COOS_STOP            -2    // set this value to stop task
#define COOS_DELAY(__delay)  if (!setjmp(coos.task_context[coos.task_no])) {longjmp(coos.main_context, __delay+1);} else {}

//##############################################################################
// Class                                              
//##############################################################################

class Coos{
  public:
                Coos(void);                           // constructor
    void        register_task(void (*tsk)(void));     // user tasks must be registered first
    void        start(void);                          // init scheduler once
    void        run(void);                            // COOS task switcher
    jmp_buf     main_context;                         // context of scheduler
    jmp_buf     task_context[COOS_MAX_TASKS];         // list of task contexts
    uchar       task_no;                              // current task No
    int         task_delay[COOS_MAX_TASKS];           // task delay in msec, task stopped if value is negative
    uchar       pause_cnt;                            // count pause from previous rx symbol
    uint        msec;                                 // ms of current sec
    ulong       uptime;                               // sec since start
    
  private:
    void        (*tsk_p[COOS_MAX_TASKS])(void);       // list of registered tasks
    void        update_time(void);
    uint        ms;
    uchar       task_cnt;                             // counts registered coos tasks
};
extern Coos coos;

#endif /* __COOS_H */
