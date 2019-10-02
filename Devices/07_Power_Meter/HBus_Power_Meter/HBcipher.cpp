/*
 * File     HBcipher.cpp
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

#include  "HBcipher.h"
#include  "cipher.h"    // cipher codes

//##############################################################################
// Def
//##############################################################################

#define DELTA     0x9E3779B9    // XTEA constant

//##############################################################################
// Var
//##############################################################################

HB_cipher HBcipher;

// flash key to be unique for the project
const ulong flashkey[4]  = { 
    KEY1,
    KEY2,
    KEY3,
    KEY4,
};

// 32-bit LFSR
static union __attribute__((aligned(4))) lfsr_reg
{
  ulong ulo;
  uchar uch[4];
} lfsr;

uint gfetch;  // 16-bit LFSR

//##############################################################################
// Func
//##############################################################################

// ===================================================
// Constructor
// ===================================================
HB_cipher::HB_cipher(void)
{
    this->valid = 0;
}

// ===================================================
// Read EEPROM key
// ===================================================
void HB_cipher::get_EE_key(void)
{
    uchar all0 = 0;
    uchar allFF = 0;
    for (uchar i=0; i<16; i++)
    {
        this->key.uch[i] = EEPROM.read(EE_XTEA_KEY+i);          // read key from EEPROM
        (this->key.uch[i] == 0) ? all0++ : all0 = 0;
        (this->key.uch[i] == 0xFF) ? allFF++ : allFF = 0;        
    }
    this->valid = ((all0 == 16) || (allFF == 16)) ?  0 : 1; 
    encrypt8(13, this->key.uch, (ulong*)flashkey);    // encrypt first half of EEPROM key using flash key
    encrypt8(17, this->key.uch+8, (ulong*)flashkey);  // encrypt second half
}

// ===================================================
// LFSR gamma
// ===================================================
uchar HB_cipher::gamma(void)
{
    if ((gfetch & 0x0410) == 0x0400)
    {
        lfsr.ulo = (lfsr.ulo & 1) ? (((lfsr.ulo ^ LFSR1) >> 1) | 0x80000000) : (lfsr.ulo >> 1);
    }
    else
    {
        lfsr.ulo = (lfsr.ulo & 1) ? (((lfsr.ulo ^ LFSR2) >> 1) | 0x80000000) : (lfsr.ulo >> 1);
    }
    gfetch = (gfetch & 1) ? ((gfetch << 1) ^ LFSR16) : ((gfetch << 1) | 1);     // rotate with inversion
    switch (gfetch & 7)      
    {
        case 0: return (lfsr.uch[2] >> 3) | (~lfsr.uch[0] & 0xE0); break;
        case 1: return (lfsr.uch[1] ^ lfsr.uch[3]); break;
        case 2: return (~lfsr.uch[0]) ^ (lfsr.uch[1] + 0x15); break;
        case 3: return (lfsr.uch[1] + lfsr.uch[2] + 0x40); break;
        case 4: return (lfsr.uch[0] << 4) | (lfsr.uch[3] >> 4); break;
        case 5: return lfsr.uch[3] + (~lfsr.uch[2] ^ 0xA6); break;
        case 6: return (~lfsr.uch[0]) ^ (lfsr.uch[2] + 0x81); break;
        default: return lfsr.uch[1] + (~lfsr.uch[3]) + 5; break;          
    }   
}

// ===================================================
// Set initial LFSR state
// ===================================================
void  HB_cipher::set_lfsr(ulong* val)
{
    lfsr.ulo = (val[0] ^ val[1]) | 0x40;   // LFSR register must be not equal to 0, set an arbitrary value
    gfetch = (uint)((val[0] >> 5) | 0x20) & 0xFFFF;   // gamma fetch schedule  
}

// ===================================================
// XTEA encrypter
// ===================================================
// encrypts 64 bits (8 bytes) using key kp
void HB_cipher::encrypt8(uchar rounds, uchar* buf, ulong* kp)
{
    uchar i;
    ulong sum = 0;
    c_union8_t* v = (c_union8_t*)buf;
    for (i=0; i<rounds; i++)
    {
        *(v->ulo) += (((*(v->ulo + 1) << 4) ^ (*(v->ulo + 1) >> 5)) + *(v->ulo + 1)) ^ (sum + *(kp+(sum & 3)));
        sum = sum + DELTA;
        *(v->ulo + 1) += (((*(v->ulo) << 4) ^ (*(v->ulo) >> 5)) + *(v->ulo)) ^ (sum + *(kp + ((sum>>11) & 3)));
        if (i == 2)             
        {
            set_lfsr(v->ulo);   // use partially encrypted buffer as LFSR seed
        }    
    }
}

// ===================================================
// XTEA decrypter
// ===================================================
// decrypt 64 bits (8 bytes) using key kp
void HB_cipher::decrypt8(uchar rounds, uchar* buf, ulong* kp)
{
    uchar i;
    ulong sum = DELTA * rounds;
    c_union8_t* v = (c_union8_t*)buf;
    for (i=0; i<rounds; i++)
    {
        *(v->ulo + 1) -= (((*(v->ulo) << 4) ^ (*(v->ulo) >> 5)) + *(v->ulo)) ^ (sum + *(kp +((sum>>11) & 3)));
        sum = sum - DELTA;
        *(v->ulo) -= (((*(v->ulo + 1) << 4) ^ (*(v->ulo + 1) >> 5)) + *(v->ulo + 1)) ^ (sum + *(kp + (sum & 3)));
        if (i == (rounds - 4))  
        {
            set_lfsr(v->ulo);   // use partially decrypted buffer as LFSR seed
        }    
    }
}

// ===================================================
// Encrypt buffer
// ===================================================
void HB_cipher::encrypt(uchar* buf, uint len)
{
    encrypt8(ROUNDS, buf, this->key.ulo);  // first 8 bytes encrypted by XTEA
    for (uint i=8; i<len; i++)
    {
        buf[i] ^= gamma();    // the rest encrypted by LFSR
    }
}

// ===================================================
// Decrypt buffer
// ===================================================
void HB_cipher::decrypt(uchar* buf, uint len)
{
    decrypt8(ROUNDS, buf, this->key.ulo);
    for (uint i=8; i<len; i++)
    {
        buf[i] ^= gamma();
    }
}

/* EOF */