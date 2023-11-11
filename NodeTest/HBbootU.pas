unit HBbootU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, Dialogs, HBcmdU, HBrxtxU;

type

  { THbBoot }

  THbBoot = class(TStringList)
  private
    FTargetID : word;
    FState : byte;
    FCodeBuf : array [0..$FFFF] of byte;  // at least 64K bytes for boot + appcode
    FCodeCnt : integer;
    FBufCrc : word;
    FPos : integer;
    FAddrMax : integer;
    FChunk : string;
    FRptCnt : integer;
    procedure FClrBuf;
    function FHexStrToBuf(ss : string; var buf : array of byte) : integer;
    function FParseString(ss: string): string;
    function FParse: string;
    function FCodeBufCRC(len: integer): word;
    function FCodeChunk(adr: word; len: byte) : string;
    function FNextChunk : string;
    function FLastChunk : string;
  public
    ErrStr : string;
    property State : byte read FState;
    property BufCrc : word read FBufCrc;
    property AddrMax : integer read FAddrMax;
    property LastChunk : string read FLastChunk;
    function ReadSketch(fn : string) : string;
    function StartBatch(targID : word) : string;
    procedure Tick10ms;
    constructor Create;
    destructor Destroy; override;
  end;

var
  HBboot  : THbBoot;

//##############################################################################
implementation
//##############################################################################

{ THbBoot }

// =====================================
// Read sketch hex file
// =====================================
function THbBoot.ReadSketch(fn: string): string;
begin
  if self.FState = 0 then begin
    self.FClrBuf;
    self.LoadFromFile(fn);
    result := self.FParse;
  end else
    result := 'Bootloader is busy';
end;

// =====================================
// Start batch
// =====================================
function THbBoot.StartBatch(targID: word): string;
begin
  result := '';
  if self.FState = 0 then begin
    self.FTargetID := targID;
    self.FState := 1;
    self.FPos := $400;
    self.ErrStr := '';
  end else
    result := 'Bootloader is busy';
end;

// =====================================
// Every 10 ms
// =====================================
procedure THbBoot.Tick10ms;
var res : string;
begin
  case self.FState of
  // --------------
  1: begin
      FChunk := '';
      inc(self.FState);
  end;
  // --------------
  2: if not HBrxtx.TxBusy then begin
       FChunk := FNextChunk;
       if FChunk <> '' then begin
         FRptCnt := 0;
         res := HBrxtx.Tx(HBcmd.CmdBoot(self.FTargetID, FChunk));
         if res = '' then
           inc(self.FState)
         else begin
           ErrStr := 'Tx error: ' + res;
           self.FState := 0;  // abort
         end;
       end else begin
         self.FState := 100;  // last chank is descriptor
       end;
    end;
  // --------------
  3..50:
    begin
        if HBrxtx.OkErr = 0 then
            self.FState := 2
         else if HBrxtx.NoRply then begin
           ErrStr := 'No reply, abort';
           self.FState := 0; // abort
         end else if HBrxtx.OkErr > 0 then begin
            if FRptCnt < 3 then begin
              inc(FRptCnt);
              res := HBrxtx.Tx(HBcmd.CmdBoot(self.FTargetID, FChunk));
              if res = '' then
                inc(self.FState)
              else begin
                ErrStr := 'Tx error: ' + res;
                self.FState := 0;  // abort
              end;
            end else begin
              ErrStr := 'Communication error, give up after 3 repeats';
              self.FState := 0;  // abort
            end;
         end else
            inc(self.FState);
    end;
  // --------------
  51: self.FState := 0; // time-out
  // --------------
  100: begin
    FChunk := FLastChunk;      // descriptor
    FRptCnt := 0;
    res := HBrxtx.Tx(HBcmd.CmdBoot(self.FTargetID, FChunk));
    if res = '' then
      inc(self.FState)
    else begin
      ErrStr := 'Tx error: ' + res;
      self.FState := 0;  // abort
    end;
  end;
  // --------------
  101..150: if HBrxtx.OkErr = 0 then begin
      self.FState := 0;
    end else if HBrxtx.NoRply then begin
      ErrStr := 'No reply, abort';
      self.FState := 0; // abort
    end else if HBrxtx.OkErr > 0 then begin
      if FRptCnt < 3 then begin
        inc(FRptCnt);
        res := HBrxtx.Tx(HBcmd.CmdBoot(self.FTargetID, FChunk));
        if res = '' then
          self.FState := 101
        else begin
          ErrStr := 'Tx error: ' + res;
          self.FState := 0;  // abort
        end;
    end else begin
      ErrStr := 'Communication error, give up after 3 repeats';
      self.FState := 0;  // abort
    end;
  end else
    inc(self.FState);
  // --------------
  151: self.FState := 0; // time-out
  // --------------
  else
      self.FState := 0;
  end; // case
end;

// =====================================
// Create
// =====================================
constructor THbBoot.Create;
begin
  FState := 0;
end;

// =====================================
// Destroy
// =====================================
destructor THbBoot.Destroy;
begin
  inherited Destroy;
end;


// =====================================
// Clear buffer
// =====================================
procedure THbBoot.FClrBuf;
var i : integer;
begin
  for i:=0 to $FFFF do begin
    FCodeBuf[i] := $FF;
  end;
  FCodeCnt := 0;
  FAddrMax := 0;
end;

// =====================================
// Convert hex string into bytes
// =====================================
function THbBoot.FHexStrToBuf(ss : string; var buf : array of byte) : integer;
var i:integer;
    c : char;
    s : string;
begin
  result := 0;
  ss := AnsiUpperCase(ss);
  s := '';
  for i:=1 to length(ss) do begin
    c := ss[i];
    if (c in ['0'..'9']) or (c in ['A'..'F']) then begin
      s := s+c;
      if length(s) = 2 then begin
        buf[result] := StrToInt('$'+s);
        inc(result);
        s := '';
      end;
    end;
  end;
end;

// =====================================
// FParse string
// =====================================
function THbBoot.FParseString(ss: string): string;
var i, len, cnt, rectype, chsum : byte;
    start, adr : integer;
    buf : array [0..$FF] of byte;
begin
  result := '';
  if (length(ss)<10) or (length(ss)>$200) then begin
    result := 'String length out of bonds';
    exit;
  end;
  if ss[1] = ':' then begin
    len := FHexStrToBuf(ss, buf);
    if len > 0 then begin
      chsum := 0;
      for i:=0 to len-1 do
        chsum := chsum+buf[i];
      if chsum = 0 then begin  // must be 0
        cnt := buf[0];
        rectype := buf[3];
        if (rectype=0) or (rectype=1) then begin  // other record types ignored
          if (len = cnt+5) and (cnt > 0) then begin
            start := $100*buf[1] + buf[2];
              for i:=0 to cnt-1 do begin
                adr := start + i;
                // -----------------------
                // application code
                // -----------------------
                if (adr >= $400) then begin // code in bootloader area ignored
                  if adr > FAddrMax then
                    FAddrMax := adr;
                  if (FAddrMax > $FFFF) then
                    FAddrMax := $FFFF
                  else begin
                    FCodeBuf[adr] := buf[4 + i];
                    inc(FCodeCnt);
                  end;
                end;
              end; // for
           end else if (cnt > 0) then
            result := 'String lenth mismatch';
        end; // rectype
      end else // chsum
        result := 'Checksum mismatch';
    end; // len > 0
  end else // :
    result := 'First symbol in a string must be ":"';
end;

// =====================================
// FParse StringList (self)
// =====================================
function THbBoot.FParse: string;
var i, len : integer;
    s : string;
begin
  result := 'Empty file';
  for i:=0 to self.Count-1 do begin
    s := self.Strings[i];
    result := FParseString(s);
    if result <> '' then
      break;
  end;
  len := FAddrMax - $400;
  FBufCrc := FCodeBufCRC(len);
end;

// =====================================
// crc for FCodeBuf
// =====================================
{
 * Name  : FBufCrc-16 CCITT
 * Poly  : 0x1021    x^16 + x^12 + x^5 + 1
 * Init  : 0xFFFF
 * Revert: false
 * XorOut: 0x0000
 * Check : 0x3B0A ("123456789" hex)
}
function THbBoot.FCodeBufCRC(len: integer): word;
var
    crc : word;
    i, j : integer;
    val : word;
begin
  crc := $FFFF;
  for i:= $400 to ($400 + len) do begin
    val := word(FCodeBuf[i]);
    crc := crc xor (val shl 8);
    for j:=0 to 7 do begin
       if (crc and $8000) = 0 then
          crc := crc shl 1
       else
          crc := (crc shl 1) xor $1021;
    end;
  end;
  result := crc;
end;

// =====================================
// Prepare a chunk of FCodeBuf
// =====================================
function THbBoot.FCodeChunk(adr: word; len: byte) : string;
var i : integer;
    cs, b : byte; // checksum
    s : string;
begin
  result := '';
  if len > 0 then begin
    s := char(len);
    cs := len;
    b := adr shr 8;
    s := s + char(b);
    cs := cs + b;
    b := byte(adr);
    s := s + char(b) + char(0); // addr and record type
    cs := cs + b;
    for i:=0 to len-1 do begin
      b := FCodeBuf[adr+i];
      s := s + char(b);
      cs := cs + b;
    end;
    cs := (cs xor $FF) + 1; // two's compliment
    s := s + char(cs);
    result := s;
  end;
end;

// =====================================
// Preapare next chunk of FCodeBuf
// =====================================
function THbBoot.FNextChunk: string;
var len : integer;
begin
  result := '';
  if self.FPos < self.FAddrMax then begin
     len := self.FAddrMax - self.FPos + 1;
     if len > $20 then
        len := $20;
     result := FCodeChunk(self.FPos, byte(len));
     self.FPos := self.FPos + len;
  end;
end;

// =====================================
// Preapare last chunk of data
// =====================================
{
* A valid code in EEPROM defined by a descriptor, an 8-byte record in EEPROM at
* address 0x0010. Descriptor fields are as follows:
*
* ----------------------------------
* addr     content
* ----------------------------------
* 0x0010   0x55
* 0x0011   0xAA
* 0x0012   0xC3
* 0x0013   0x3C
* 0x0014   MSB byte of code length
* 0x0015   LSB byte of code length
* 0x0016   MSB byte of CRC
* 0x0017   LSB byte of CRC
* ----------------------------------
}
function THbBoot.FLastChunk: string;
var codelen : word;
begin
   FCodeBuf[$10] := $55;
   FCodeBuf[$11] := $AA;
   FCodeBuf[$12] := $C3;
   FCodeBuf[$13] := $3C;
   codelen := self.FAddrMax - $400 + 1;
   FCodeBuf[$14] := byte(codelen >> 8);
   FCodeBuf[$15] := byte(codelen and $FF);
   FCodeBuf[$16] := byte(self.FBufCrc >> 8);
   FCodeBuf[$17] := byte(self.FBufCrc and $FF);
   result := FCodeChunk($10, 8);
end;


end.

