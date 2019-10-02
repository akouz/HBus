/*
 * File     HBus.h
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
 #ifndef __HBUS_H
 #define __HBUS_H

//##############################################################################
// Inc
//##############################################################################

#include "HBcommon.h"
#include "HBrxtx.h" 
#include "HBcmd.h"
#include "HBmqtt.h"


//##############################################################################
// Def
//##############################################################################

enum{
//    MAX_HASH_LEN        = 8,
//    MAX_HASH_LIST       = 0x10,
};

//##############################################################################
// Func
//##############################################################################

void hbus_msg_to_mqtt(hb_msg_t* msg);
void coos_task_HBus_rxtx(void);
void coos_task_tick1ms(void);


#endif /* __HBUS_H */
