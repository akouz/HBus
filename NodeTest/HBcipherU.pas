// =====================================================================
// HBus cipher, combined XTEA + LFSR
// =====================================================================
{
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
}

unit HBcipherU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, IniFiles;

const
  DELTA     = $9E3779B9;    // XTEA constant

type
  byte_p = ^byte;
  longword_p = ^longword;

// =====================================
{ THbCipher }
// =====================================
THbCipher = class(TStringList)
private
  v : array [0..1] of longword;
  sum : longword;
  procedure FLongwordToBuf(val : longword; buf : byte_p);
  function FStrBufToLongword(s : string) : longword;
  function FLongwordToStrBuf(val : longword) : string;
  procedure FSetLFSR(val : longword_p);
  function FEncrypt8(s : string; kp : longword_p; rnds : byte) : string;
  function FDecrypt8(s : string; kp : longword_p; rnds : byte) : string;
  function FGamma : byte;
public
  Key   : array [0..3] of longword;
  EENotes : string;
  EEKey  : array [0..3] of longword;
  Notes : string;
  FlashKey : array [0..3] of longword;
  Rounds : byte;
  LFSR : longword;
  LFSR1 : longword;
  LFSR2 : longword;
  gfetch : word;
  LFSR16 : longword;
  procedure Calc_Key;
  function encrypt(s : string) : string;
  function decrypt(s : string) : string;
  procedure ReadFlashCip(fn : string);
  procedure ReadEECip(fn : string);
  procedure SaveFlashCip(fn : string);
  procedure SaveEECip(fn : string);
  constructor Create;
  destructor Destroy; override;
end;

//##############################################################################
implementation
//##############################################################################

{ THbCipher }

// =====================================
// Convert longword to buffer
// =====================================
procedure THbCipher.FLongwordToBuf(val : longword; buf : byte_p);
begin
  (buf+3)^ := byte(val shr 24);
  (buf+2)^ := byte(val shr 16);
  (buf+1)^ := byte(val shr 8);
  (buf)^   := byte(val);
end;

// =====================================
// Convert string buffer to longword
// =====================================
// string buff is little endian
function THbCipher.FStrBufToLongword(s : string) : longword;
begin
  result := 0;
  if length(s) > 0 then begin
    result := ord(s[1]);
    if length(s) > 1 then begin
      result := result + $100*ord(s[2]);
      if length(s) > 2 then begin
        result := result + $10000*ord(s[3]);
        if length(s) > 3 then begin
          result := result + $1000000*ord(s[4]);
        end;
      end;
    end;
  end;
end;

// =====================================
// Convert longword to buffer
// =====================================
// string buff is little endian
function THbCipher.FLongwordToStrBuf(val : longword) : string;
begin
  result := '' + char(val) + char(val shr 8);
  result := result + char(val shr 16) + char(val shr 24);
end;

// =====================================
// XTEA encryption - block of 8 bytes
// =====================================
function THbCipher.FEncrypt8(s : string; kp : longword_p; rnds : byte) : string;
var i : byte;
begin
  v[0] := FStrBufToLongword(copy(s,1,4));
  v[1] := FStrBufToLongword(copy(s,5,4));
  sum := 0;
  for i:=1 to rnds do begin
    v[0] := v[0] + ((((v[1] shl 4) xor (v[1] shr 5)) + v[1]) xor (sum + (kp + (sum and 3))^));
    sum := sum + DELTA;
    v[1] := v[1] + ((((v[0] shl 4) xor (v[0] shr 5)) + v[0]) xor (sum + (kp + ((sum shr 11) and 3))^));
    if (i = 3) then
       FSetLFSR(v);   // use partially encrypted buffer as LFSR seed
  end;
  result := FLongwordToStrBuf(v[0]) + FLongwordToStrBuf(v[1]);
end;
// =====================================
// XTEA decryption - block of 8 bytes
// =====================================
function THbCipher.FDecrypt8(s : string; kp : longword_p; rnds : byte) : string;
var i : byte;
begin
  v[0] := FStrBufToLongword(copy(s,1,4));
  v[1] := FStrBufToLongword(copy(s,5,4));
  sum := DELTA*rnds;
  for i:=1 to rnds do begin
    v[1] := v[1] - ((((v[0] shl 4) xor (v[0] shr 5)) + v[0]) xor (sum + (kp + ((sum shr 11) and 3))^));
    sum := sum - DELTA;
    v[0] := v[0] - ((((v[1] shl 4) xor (v[1] shr 5)) + v[1]) xor (sum + (kp + (sum and 3))^));
    if (i = rnds-3) then
       FSetLFSR(v);   // use partially decrypted buffer as LFSR seed
  end;
  result := FLongwordToStrBuf(v[0]) + FLongwordToStrBuf(v[1]);
end;

// =====================================
// Calculate working key using flash key and EEPROM key
// =====================================
procedure THbCipher.Calc_Key;
var i : integer;
    s : string;
begin
  s := FLongwordToStrBuf(EEkey[0]) + FLongwordToStrBuf(EEkey[1]);
  s := FEncrypt8(s, FlashKey, 13);
  Key[0] := FStrBufToLongword(copy(s,1,4));
  Key[1] := FStrBufToLongword(copy(s,5,4));
  s := FLongwordToStrBuf(EEkey[2]) + FLongwordToStrBuf(EEkey[3]);
  s := FEncrypt8(s, FlashKey, 17);
  Key[2] := FStrBufToLongword(copy(s,1,4));
  Key[3] := FStrBufToLongword(copy(s,5,4));
end;

// =====================================
// Set initial LFSR state
// =====================================
procedure THbCipher.FSetLFSR(val : longword_p);
begin
  LFSR := (val[0] xor val[1]) or $40;
  gfetch := word(val[0] shr 5) or $20;
end;

// =====================================
// Get stream cipher gamma
// =====================================
function THbCipher.FGamma : byte;
var xval : longword;
    buf : array [0..3] of byte;
begin
  if ((gfetch and $0410) = $0400) then
    xval := LFSR1
  else
    xval := LFSR2;
  if (LFSR and 1) = 1 then
    LFSR := ((LFSR xor xval) shr 1) or $80000000
  else
    LFSR := LFSR shr 1;
  if  (gfetch and 1) = 1 then
    gfetch := (gfetch shl 1) xor LFSR16
  else
    gfetch := (gfetch shl 1) or 1;  // rotate with inversion
  FLongwordToBuf(LFSR, buf);
  case (gfetch and 7) of
    0: result := (buf[2] shr 3) or ((not buf[0]) and $E0);
    1: result := buf[1] xor buf[3];
    2: result := (not buf[0]) xor (buf[1] + $15);
    3: result := buf[1] + buf[2] + $40;
    4: result := (buf[0] shl 4) or (buf[3] shr 4);
    5: result := buf[3] + ((not buf[2]) xor $A6);
    6: result := (not buf[0]) xor (buf[2] + $81);
    else
      result := buf[1] + (not buf[3]) + 5;
  end;
end;

// ===================================================
// Encrypt buffer
// ===================================================
function THbCipher.encrypt(s : string) : string;
var i : integer;
    kp : longword_p;
    b : byte;
begin
  kp := @Key[0];
  result := FEncrypt8(s, kp, Rounds); // first 8 bytes encrypted by XTEA
  for i:=9 to length(s) do begin
    b := ord(s[i]);
    b := b xor FGamma; // the rest encrypted by LFSR
    result := result + char(b);
  end;
end;

// ===================================================
// Decrypt buffer
// ===================================================
function THbCipher.decrypt(s : string) : string;
var i : integer;
    kp : longword_p;
    b : byte;
begin
  kp := @Key[0];
  result := FDecrypt8(s, kp, Rounds); // first 8 bytes decrypted by XTEA
  for i:=9 to length(s) do begin
    b := ord(s[i]);
    b := b xor FGamma; // the rest encrypted by LFSR
    result := result + char(b);
  end;
end;

// =====================================
// Read flash cipher
// =====================================
procedure THbCipher.ReadFlashCip(fn : string);
var ini : TIniFile;
begin
  ini := TIniFile.Create(fn);
  Notes := ini.ReadString('Cipher', 'Notes', 'Default cipher');
  FlashKey[0] := ini.ReadInteger('Cipher', 'Key1', $60F3C66D);
  FlashKey[1] := ini.ReadInteger('Cipher', 'Key2', $5DF53900);
  FlashKey[2] := ini.ReadInteger('Cipher', 'Key3', $4F533EB6);
  FlashKey[3] := ini.ReadInteger('Cipher', 'Key4', $E42B2A61);
  LFSR1 := ini.ReadInteger('Cipher', 'LFSR1', $1EDC6F41);
  LFSR2 := ini.ReadInteger('Cipher', 'LFSR2', $04C11DB7);
  LFSR16 := ini.ReadInteger('Cipher', 'LFSR16', $755B);
  Rounds := ini.ReadInteger('Cipher', 'Rounds', 6);
  ini.Free;
end;

// =====================================
// Read EEPROM cipher
// =====================================
procedure THbCipher.ReadEECip(fn : string);
var ini : TIniFile;
begin
  ini := TIniFile.Create(fn);
  EENotes := ini.ReadString('Cipher', 'EENotes', 'Default EEPROM key');
  EEKey[0] := ini.ReadInteger('Cipher', 'EEKey1', $4c25dc00);
  EEKey[1] := ini.ReadInteger('Cipher', 'EEKey2', $bcb2e7dc);
  EEKey[2] := ini.ReadInteger('Cipher', 'EEKey3', $89eb06ab);
  EEKey[3] := ini.ReadInteger('Cipher', 'EEKey4', $15227cb7);
  ini.Free;
end;

// =====================================
// Save flash cipher
// =====================================
procedure THbCipher.SaveFlashCip(fn : string);
var ini : TIniFile;
    s : string;
begin
  ini := TIniFile.Create(fn);
  ini.WriteString('Cipher','Notes', Notes);
  s := '$'+IntToHex(FlashKey[0],8);
  ini.WriteString('Cipher','Key1', s);
  s := '$'+IntToHex(FlashKey[1],8);
  ini.WriteString('Cipher','Key2', s);
  s := '$'+IntToHex(FlashKey[2],8);
  ini.WriteString('Cipher','Key3', s);
  s := '$'+IntToHex(FlashKey[3],8);
  ini.WriteString('Cipher','Key4', s);
  ini.WriteInteger('Cipher','Rounds',Rounds);
  s := '$'+IntToHex(LFSR1,8);
  ini.WriteString('Cipher','LFSR1', s);
  s := '$'+IntToHex(LFSR2,8);
  ini.WriteString('Cipher','LFSR2', s);
  s := '$'+IntToHex(LFSR16,4);
  ini.WriteString('Cipher','LFSR16', s);
  ini.Free;
end;

// =====================================
// Save EEPROM cipher
// =====================================
procedure THbCipher.SaveEECip(fn : string);
var ini : TIniFile;
    s : string;
begin
  ini := TIniFile.Create(fn);
  ini.WriteString('Cipher','EENotes', EENotes);
  s := '$'+IntToHex(EEKey[0],8);
  ini.WriteString('Cipher','EEKey1', s);
  s := '$'+IntToHex(EEKey[1],8);
  ini.WriteString('Cipher','EEKey2', s);
  s := '$'+IntToHex(EEKey[2],8);
  ini.WriteString('Cipher','EEKey3', s);
  s := '$'+IntToHex(EEKey[3],8);
  ini.WriteString('Cipher','EEKey4', s);
  ini.Free;
end;

// =====================================
// Create
// =====================================
constructor THbCipher.Create;
begin
  ReadFlashCip('FlashCipher.ini');
  ReadEECip('EECipher.ini');
  Calc_Key;
end;

// =====================================
// Destroy
// =====================================
destructor THbCipher.Destroy;
begin
  SaveFlashCip('FlashCipher.ini');
  SaveEECip('EECipher.ini');
  inherited Destroy;
end;

end.

