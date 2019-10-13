/*
 * File     HBcommon.h
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

#ifndef __HB_CONFIG_H
#define __HB_CONFIG_H

//##############################################################################
// Def
//##############################################################################

#ifndef BUILTIN_LED
  #define BUILTIN_LED   13
#endif

#define LED             BUILTIN_LED

// -----------------------------------
// Device descriptor for REV command
// -----------------------------------
// must be 8 bytes long
#define HB_DEV_TYPE         2  // device type
#define HB_DEV_MODEL        1  // device model
#define HB_HW_REV_MAJ       0  // h/w rev major
#define HB_HW_REV_MIN       1  // h/w rev minor
#define HB_BOOT_REV_MAJ     0  // boot rev major
#define HB_BOOT_REV_MIN     1  // boot rev minor, 0.1 is a native Arduino bootloader
#define HB_SKETCH_REV_MAJ   1  // sketch rev major
#define HB_SKETCH_REV_MIN   0  // sketch rev minor


#define TIME_ZONE      (10*60)  // +10 hr, Australian East Coast time zone

#define COOS_TASKS      4       // number of COOS tasks, keep it low to save RAM

#define MAX_TX_BUF      0x50    // to save RAM, Tx buffer length might be shorter than MAX_BUF = 0x90

#define MAX_TOPIC       4       // should be not greater than 16

#define BROADCAST_TOPIC_NAME    // add tname="topic_name" to PUBLISH message
// #define DEBUG                   // enable debug prints

// define topic names TOPIC0 ... TOPIC15
#define TOPIC0  "test1"
#define TOPIC1  "test2"
#define TOPIC2  "test3"
#define TOPIC3  "test4"

#endif /* __HB_CONFIG_H */
