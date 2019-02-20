/*
 * File     coos.cpp
 * Rev      1.2 dated 14/07/2018
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

#include  "coos.h"

//##############################################################################
// Var  
//##############################################################################

Coos coos;

//##############################################################################
// Func
//##############################################################################

// =================================
// Constructor
// =================================
Coos::Coos(void)
{
  uint i;
  uptime = 0;
  msec = 0;
  ms = 0;
  task_cnt = 0;
  for (i=0; i<COOS_MAX_TASKS; i++)
  {
    tsk_p[i] = NULL;                   // task is not registered
    task_delay[i] = COOS_STOP;         // all unregistered tasks stopped
  }
}
// =================================
// Register a task 
// =================================
void Coos::register_task(void (*tsk)(void))
{
  if (task_cnt < COOS_MAX_TASKS)
  {
    tsk_p[task_cnt++] = tsk;
  }
}
// =================================
// Update time
// =================================
// supposed to happen more often than every millisecond,
// but can be late if a task takes long time to execute
void Coos::update_time(void)
{
  uint millisec = (uint)millis(); 
  while (ms != millisec) // catch up time
  {      
    ms++;  
    pause_cnt = (pause_cnt < 200)? pause_cnt+1 : pause_cnt;
    for (int i=0; i<task_cnt; i++)
    {
      if (task_delay[i] > 0)  // positive delays 
      {
        task_delay[i]--;
      }
    }
    if (++msec >= 1000) // if 1 sec passed
    {
      uptime++;         // count seconds since start
      msec = 0;
    }
  }  
}
// =================================
// Start scheduler - init registered tasks
// =================================
void Coos::start(void)
{
  int   res;
  void (*tsk)(void);
  update_time(); 
  for (task_no=0; task_no<task_cnt; task_no++)
  {
    if (tsk_p[task_no] != NULL)            // if task was registered
    {
      res = setjmp(main_context);
      if (res == 0)
      {
        tsk = tsk_p[task_no];
        tsk();                             // invoke task
      }
      else
      {
        task_delay[task_no] = --res;      // task returns the required delay
      }
    }  
    update_time(); 
  }
  task_no = 0;
}
// =================================
// Run scheduler on regular basis
// =================================
void Coos::run(void)
{
  int   res;
  int tmp;
  if (task_delay[task_no] == 0)
  {
    res = setjmp(main_context);           // set return point and get delay value from the task 
    if (res == 0)                         // after setting return point
    {
      longjmp(task_context[task_no], 1);  // invoke task 
    }
    else                                  // after returning from task 
    {
      tmp = --res;
      task_delay[task_no] = tmp;        // set task delay (negative delay - task stopped) 
    }
  }
  task_cnt = (task_cnt > COOS_MAX_TASKS)? COOS_MAX_TASKS : task_cnt;
  if (++task_no > task_cnt)               // next task
  {
    task_no = 0;                          // round-robin
  }
  update_time(); 
}

/* EOF */