/*
 * Author    A.Kouznetsov
 * Rev       1.0 dated 8/1/2019
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

//##############################################################################
// Inc
//##############################################################################

#include  "common.h"


//##############################################################################
// Var
//##############################################################################

uint pup_cnt; 
uint node_seed;
uint led_cnt;     // until LED switched off, in 10 ms ticks

//##############################################################################
// Func
//##############################################################################

// =============================================
// Blink LED
// =============================================
void blink(uint dur) // 10ms ticks
{
    led_cnt = dur;
    digitalWrite(LED_BUILTIN, HIGH);
} 
// =============================================
// Copy buffer
// =============================================
void copy_buf(uchar* src, uchar* dst, uchar len)
{    
    if (dst)
    {
        for (uchar i=0; i<len; i++)
        {
            dst[i] = (src)? src[i] : 0; // if src does not exist then fill dst by 0                     
        }
    }
}
// =============================================
// Shift buffer down
// =============================================
void shift_buf(uchar* buf, uchar pos, uchar len)
{
    if ((buf) && (pos) && (len))
    {
        for (uchar i=0; i<len; i++)
        {
            buf[i] = buf[i+pos];
        }
    }
}
// =============================================
// Add uchar value to crc
// =============================================
void crc_add_uchar(uchar b, uint* crc)
{
    *crc ^= (b << 8);
    for (uchar j=0; j<8; j++)
    {
        *crc = (*crc & 0x8000)? ((*crc << 1) ^ 0x1021) : (*crc << 1);
    }
}

// =============================================
 // Calculate CRC
// =============================================
/*
 * Name  : CRC-16 CCITT
 * Poly  : 0x1021    x^16 + x^12 + x^5 + 1
 * Init  : 0xFFFF
 * Revert: false
 * XorOut: 0x0000
 * Check : 0x3B0A ("123456789" hex)
 */
uint calc_crc(uchar* buf, uchar len)
{
    uint crc = 0xFFFF;
    if (buf)
    {
        for (uchar i=0; i<len; i++)
        {
            crc_add_uchar(buf[i], &crc);
        }
    }
    return crc;
}
// =============================================
// Reset Tx buffer and start a new message
// =============================================
uchar begin_txmsg(hb_msg_t* txmsg, uchar hb)
{
    if (txmsg->busy)
    {
        return NOT_READY;
    }
    else
    {
        txmsg->hb = (hb)? 1 : 0;  
        txmsg->buf[0] = _ESC;
        txmsg->buf[1] = (hb)? _ESC_START_HB : _ESC_START_MQ;   // HBus/MQTT
        txmsg->crc = 0xFFFF;    // init crc
        txmsg->len = 2;
        return READY;   
    }    
}
// =============================================
// Add char to Tx message
// =============================================
uchar add_txmsg_uchar(hb_msg_t* txmsg, uchar c)
{
    if ((txmsg->len >= MAX_BUF) || (txmsg->busy))
    {
        return ERR;
    }
    crc_add_uchar(c, &txmsg->crc);   // calculate crc
    if (c == _ESC)
    {
        if (txmsg->len >= 4) // it could be _ESC_ESC sequence
        {
            if  ((txmsg->buf[txmsg->len-2] == _ESC) && 
                 (txmsg->buf[txmsg->len-1] == _ESC_ESC))
            {
                txmsg->buf[txmsg->len-1] = _ESC_2ESC;  // replace 
                return OK;
            }
        }
        txmsg->buf[txmsg->len++] = _ESC;
        txmsg->buf[txmsg->len++] = _ESC_ESC;
    }
    else
    {
        txmsg->buf[txmsg->len++] = c;
    }
    return OK;
}
// =============================================
// Add 0-terminated string to Tx message
// =============================================
uchar add_txmsg_z_str(hb_msg_t* txmsg, char* str)
{
    uchar res = 0;    
    if (txmsg->busy == 0)
    {
        while (*str)    
        {
            if (txmsg->len < MAX_BUF)
            {
                add_txmsg_uchar(txmsg, (uchar)*str);
                str++;
                res++;    
            }
            else
            {
                break;
            }
        }
    }
    return res;
}
// =============================================
// Copy message header
// =============================================
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_msg_t* txmsg)
{
    for (uchar i=first; i<last; i++)
    {
        if (i == 0)
        {
            add_txmsg_uchar(txmsg, src->buf[0] | 0x80); // set "reply" flag  
        }
        else
        {
            add_txmsg_uchar(txmsg, src->buf[i]);
        }
    }
}

// =============================================
// Finish Tx message
// =============================================
uchar finish_txmsg(hb_msg_t* txmsg)
{
    uchar crcL = (uchar)txmsg->crc;          // crc lsb
    add_txmsg_uchar(txmsg, (uchar)(txmsg->crc >> 8));  // crc msb
    add_txmsg_uchar(txmsg, crcL);
    if (txmsg->len <= MAX_BUF-2)
    {
        txmsg->buf[txmsg->len++] = _ESC;
        txmsg->buf[txmsg->len++] = _ESC_END;
        txmsg->busy = 1;
        return OK;
    }
    return ERR;
}
                           
/* EOF */