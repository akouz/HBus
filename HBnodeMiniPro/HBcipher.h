/*
 * File     HBcipher.h
 * 
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
 * 
 * -----------------------------------------------------------------------------
 *                  --- Notes ---
 *                  -------------
 * 
 * This is a combined XTEA+LFSR cipher. First 8 bytes of buffer 
 * encypted/decripted by XTEA block cipher with 128-bit key. The rest of the
 * buffer encrypted/decrypted by 32-bit LFSR used as a stream cipher.
 * 
 * XTEA key combines two keys:
 * -- fixed key called flashkey stored in mucrocontroller's program memory
 * -- user assigned key stored in EEPROM
 * At start-up cipher reads EEPROM key and encrypts it using flaskey. The 
 * resulting 128-bit key used as XTEA key. 
 * 
 * Number of XTEA encryption rounds is arbitrary, but it should not be less 
 * than 6,  see https://www.tayloredge.com/reference/Mathematics/VRAndem.pdf
 * "A CRYPTANALYSIS OF THE TINY ENCRYPTION ALGORITHM by VIKRAM REDDY ANDEM"
 * TUSCALOOSA, ALABAMA, 2003 
 * -----------------------------------------------------------------------------
*/
 
#ifndef __HB_CIPHER_H
#define __HB_CIPHER_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Def
//##############################################################################

typedef union __attribute__((aligned(4))) {
    ulong ulo[2];
    uchar uch[8];    // uch[0] is lsb byte of ulo[0]
} c_union8_t;

typedef union __attribute__((aligned(4))) {
    ulong  ulo[4];
    uchar  uch[16];
} c_union16_t;


//##############################################################################
// Class
//##############################################################################

class HB_cipher{

    public:
        HB_cipher(void);
        uchar   valid;
        void    get_EE_key(void);
        void    encrypt(uchar* buf, uint len);
        void    decrypt(uchar* buf, uint len);
        c_union16_t    key;

    private:
        uchar   gamma(void);
        void    set_lfsr(ulong* val);
        void    encrypt8(uchar rounds, uchar* buf, ulong* kp);
        void    decrypt8(uchar rounds, uchar* buf, ulong* kp);
};
extern HB_cipher HBcipher;

#endif /* __HB_CIPHER_H */