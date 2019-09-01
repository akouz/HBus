// =============================================================================
// HBus serial port:
// - adds CRC, adds bit-stuffing and transmit messages, if echo does not match
//   then re-transmit 3 times before giving yp
// - receive messages, strips off byte-stuffing and checks CRC, if it is not an
//   echo of transmitted messages then adds received string to the list
// =============================================================================
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
unit HBrxtxU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, Dialogs, CPort, HBcipherU;

const
  _ESC           = $1B;
  _ESC_START_HB  = 2; // HBus
  _ESC_START_MQ  = 3; // MQTT
  _ESC_START_HBE = 4; // HBus encrypted
  _ESC_START_MQE = 5; // MQTT encrypted
  _ESC_END       = 7;
  _ESC_ESC       = 8;
  _ESC_2ESC      = 9;
  _PRI_LO        = $FF;
  _PRI_MED       = $FC;
  _PRI_HI        = $F0;
  TX_TMOUT       = 20;  // 200 ms

type
  THbMsg = record
    pri       : byte;      // priority (prefix)
    s         : string;    // data
    mqtt      : boolean;   // HBus/MQTT
    encrypted : boolean;
    err       : boolean;
    valid     : boolean;
    postpone  : byte;      // 10 ms ticks
  end;

type

  // =====================================  
  { THbRxtx }
  // =====================================  

  THbRxtx = class(TStringList)
  private
    Fcnt : integer;
    FGate: boolean;           // message gate
    FGateTmout : integer;
    FEsc:  boolean;           // ESC flag
    FLast: char;              // last received char outside gate
    FLastValid : boolean;
    FRxMsg:   THbMsg;         // decoded message
    Fexpected: string;        // string to send before encoding
    Fsent: string;            // last sent string, encoded
    FTxBusy:   boolean;       // transmitter is busy
    Fpri:     integer;        // message priority
    FTxTmout: integer;        // time-out
    FRtrCnt:  integer;        // retry count
    FStr : string;
    FDampHex : string;
    FDampStr : string;
    FDampCnt : integer;
    FDampPause : integer;
    FStr_tmout : integer;
    procedure FAddDampStr(c:char);
    procedure FAddDebugStr(c:char);
    function FAddChar(c: char): string;         // strip byte-stuffing
    function Fcrc(ss: string): word;            // calculate CRC
    function FAddCrc(ss: string): string;       // add CRC to a string
    function FCheckCrc(var ss: string): boolean;
    function FEncode(ss: string): string;
    function FEncodeHB(ss: string): string;
    function FEncodeMQ(ss: string): string;
    function FAddPriority(ss: string): string;  // add message priority
    procedure FDecode(ss: string);
  public
    ComPort: TComPort;
    PortOk : boolean;
    EncryptHB : boolean;
    EncryptMQ : boolean;
    ErrCnt:  integer;       // errors while receiving
    NoRx:    boolean;       // no receiption
    DbgList : TStringList;
    DampList : TStringList;
    TxStatus : integer;
    MsgID : byte;
    LastCrc : array [0..1] of word;
    Reply : string;         // when a reply received
    Cipher : THbCipher;
    property Gate : boolean read FGate;
    function Rx: THbMsg;    // get received string and remove it from the list
    function Tx(msg: THbMsg) : string; // transmit if not busy
    procedure FlushRx;
    procedure FlushTx;
    procedure Tick10ms;      // process it every 10 ms
    constructor Create(port : string);
    destructor Destroy; override;
  end;


//##############################################################################
implementation
//##############################################################################

{ THbRxtx }

// =====================================
// Add char to dump string
// =====================================
procedure THbRxtx.FAddDampStr(c : char);
begin
  FDampPause := 0;
  if ((c >= ' ') and (ord(c) < $7F)) then
    FDampStr := FDampStr + c
  else
    FDampStr := FDampStr + '?';
  FDampHex := FDampHex + IntToHex(ord(c),2) + ' ';
  if (FDampCnt and 7) = 7 then
     FDampHex := FDampHex + ' ';
  inc(FDampCnt);
  if (FDampCnt >= 16) then begin
    DampList.Add(FDampHex+' '+FDampStr);
    FDampCnt := 0;
    FDampHex := '';
    FDampStr := '';
  end;
end;

// =====================================
// Add char to debug string
// =====================================
procedure THbRxtx.FAddDebugStr(c : char);
begin
  if ((c >= ' ') and (ord(c) < $7F)) then
    FStr := FStr + c
  else begin
    FStr := Fstr + '[' + IntToHex(ord(c),2)+']';
  end;
  if (FStr <> '') then begin
    if (ord(c) = 10) then begin
       DbgList.Add(FStr);
       FStr := '';
       FStr_tmout := 0;
    end;
  end;
end;

 // =====================================  
 // Add char to a current receive buffer
 // =====================================  
function THbRxtx.FAddChar(c: char): string;
begin
  FAddDampStr(c);
  Result := '';
  // --------------------
  // char after ESC
  // --------------------
  if FEsc then begin // previous char was ESC
    FEsc := False;
    case Ord(c) of
      // --------------
      _ESC_START_HB.._ESC_START_MQE: // beginning of a frame
      begin
        if FStr <> '' then begin
          DbgList.Add(FStr);
          FStr := '';
          FStr_tmout := 0;
        end;
        if FLastValid then
          FRxMsg.pri := byte(FLast)
        else
          Inc(ErrCnt);
        FLastValid := false;
        if FGate then
          Inc(ErrCnt);  // START without STOP
        FGate := True;
        FGateTmout := 0;
        FRxMsg.s := '';
        if ((Ord(c) = _ESC_START_HB) or (Ord(c) = _ESC_START_HBE)) then
           FRxMsg.mqtt := false
        else
           FRxMsg.mqtt := true;
        if ((Ord(c) = _ESC_START_HBE) or (Ord(c) = _ESC_START_MQE)) then
           FRxMsg.encrypted := true
        else
           FRxMsg.encrypted := false;
      end;
      // --------------
      _ESC_END: // end of a frame
      begin
        if not FGate then
          Inc(ErrCnt)   // STOP without START
        else
        begin
          if (FRxMsg.encrypted) then
            Result := Cipher.decrypt(FRxMsg.s)
          else
            Result := FRxMsg.s; // message completed
          FRxMsg.s := '';
          FGate  := False;
        end;
        FLastValid := false;
      end;
      // --------------
      _ESC_ESC: // insert ESC
      begin
        if not FGate then
          Inc(ErrCnt)
        else
          FRxMsg.s := FRxMsg.s + char(_ESC);
      end;
      // --------------
      _ESC_2ESC: // insert two ESC
      begin
        if not FGate then
          Inc(ErrCnt)
        else  begin
          FRxMsg.s := FRxMsg.s + char(_ESC) + char(_ESC);
        end;
      end
      // --------------
      else begin
        Inc(ErrCnt);  // any other char after ESC is an error
        FLastValid := true;
        FLast := c;
      end;
    end;
    // --------------------
    // regular input
    // --------------------
  end  else  begin
    if c = char(_ESC) then
      FEsc := True  // consider next char
    else  begin
      if not Fgate then begin // char outside gate
        if FLastValid then
          FAddDebugStr(FLast); // most likely it is a debug message from node
      end else begin
        FRxMsg.s := FRxMsg.s + c;
      end;
      FLastValid := true;
      FLast := c;
    end;
  end; // case
end;

 // ===========================================
 // Calculate CRC
 // ===========================================
{
 * Name  : CRC-16 CCITT
 * Poly  : 0x1021    x^16 + x^12 + x^5 + 1
 * Init  : 0xFFFF
 * Revert: false
 * XorOut: 0x0000
 * Check : 0x3B0A ("123456789" hex)
}
function THbRxtx.Fcrc(ss: string): word;
var
  crc:  word;
  b:    byte;
  i, j: integer;
begin
  crc := $FFFF;
  if Length(ss) > 0 then  begin
    for i := 1 to Length(ss) do  begin
      b   := Ord(ss[i]);
      crc := crc xor (b shl 8);
      for j := 0 to 7 do begin
        if (crc and $8000) = 0 then
          crc := crc shl 1
        else
          crc := (crc shl 1) xor $1021;
      end;
    end;
  end;
  result := crc;
end;

 // ===========================================
 // Add crc
 // ===========================================
function THbRxtx.FAddCrc(ss: string): string;
var
  crc: word;
begin
  crc    := Fcrc(ss);
  result := ss + char(crc shr 8) + char(crc and $FF);
end;

 // ===========================================
 // Check crc
 // ===========================================
function THbRxtx.FCheckCrc(var ss: string): boolean;
var  len : integer;
begin
  result := False;
  LastCrc[0] := 0;
  LastCrc[1] := $FFFF;
  len := Length(ss);
  if len > 1 then begin
    LastCrc[0] := $100*ord(ss[len-1]) + ord(ss[len]); // supplied CRC
    ss     := copy(ss, 1, len - 2); // string without CRC
    LastCrc[1] := Fcrc(ss);
    result := (LastCrc[0] = LastCrc[1]);  // calculated CRC equal to supplied CRC
    if not result then  begin
       len := Length(ss);
    end;
  end;
end;

 // ===========================================
 // Add byte-stuffing
 // ===========================================
function THbRxtx.FEncode(ss: string): string;
var
  s:   string;
  i:   integer;
  c:   char;
  esc: boolean;
begin
  result := '';
  esc := false;
  for i := 1 to Length(ss) do begin
    c := ss[i];
    // -----------------------------
    if not esc then  begin
      if c = char(_ESC) then begin
        esc    := true;
        result := result + char(_ESC);  // start byte-stuffing
      end else
        result := result + c;
    // -----------------------------
    end else  begin
      esc := false;
      if c = char(_ESC) then // if second ESC
        result := result + char(_ESC_2ESC)
      else  begin            // if one ESC
        result := result + char(_ESC_ESC);
        result := result + c;
      end;
    end;
  end;
  if esc then // last char was ESC
    result := result + char(_ESC_ESC);
end;

 // =====================================  
 // Parse received string
 // =====================================  
procedure THbRxtx.FDecode(ss: string);
var
  i, j: integer;
  c: char;
  s: string;
begin
  for i := 1 to Length(ss) do  begin
    c := ss[i];
    s := FAddChar(c);
    if (s <> '') then  begin
      NoRx := False;
      if FCheckCrc(s) then begin   // if crc matches
        if (Fexpected <> '') and (s = Fexpected) then  begin // it is echo
          TxStatus := 2;
          Fexpected := '';
          Fsent  := '';
          FTxTmout := 0;
          FTxBusy  := False;
        end;
        if FRxMsg.mqtt then
          Self.Add(char(FRxMsg.pri) + 'M' + s)   // mark MQTT message
        else
          Self.Add(char(FRxMsg.pri) + 'H' + s);  // mark HBus message
      end else begin
        if FRxMsg.mqtt then
          Self.Add(char(FRxMsg.pri) + 'E' + s)    // mark error in MQTT frame
        else
          Self.Add(char(FRxMsg.pri) + 'e' + s);   // mark error in HBus frame
      end;
    end;
  end;
end;

 // =====================================  
 // Get received string
 // =====================================  
function THbRxtx.Rx: THbMsg;
var s : string;
begin
  Result.valid := false;
  if self.Count > 0 then  begin
    s := self.Strings[0];
    Result.pri := byte(s[1]);
    if (s[2] = 'E') or (s[2] = 'e') then begin
      Result.err := true;
      Result.mqtt := (s[2] = 'E')
    end else begin
      Result.err := false;
      Result.mqtt := (s[2] = 'M');         // first letter 'M' means MQTT message
    end;
    Result.s := Copy(s, 3, Length(s) - 2); // remove first letter
    self.Delete(0);                        // remove string from the list
    Result.valid := true;
    if (Result.mqtt = true) and (Result.err = false) then begin
       MsgID := ord(Result.s[6]);
    end;
  end;
end;

 // =====================================  
 // Encode string, e.g. add crc and byte-stuffing
 // =====================================  
function THbRxtx.FEncodeHB(ss: string): string;
var s : string;
begin
  Result := '';
  if not FTxBusy then begin
    Result    := '' + char(_PRI_LO) + char(_ESC);
    Fexpected := ss;
    s := FAddCrc(ss);
    if (EncryptHB) then begin
      s := Cipher.encrypt(s);
      Result := Result + char(_ESC_START_HBE);
    end else
      Result := Result + char(_ESC_START_HB);
    Result    := Result + FEncode(s);
    Result    := Result + char(_ESC) + char(_ESC_END);
    Fsent     := Result;
    FTxBusy   := True;
    FRtrCnt   := 0;
    FTxTmout  := TX_TMOUT;
  end;
end;
// =====================================  
function THbRxtx.FEncodeMQ(ss: string): string;
var s : string;
begin
  Result := '';
  if not FTxBusy then begin
    Result    := '' + char(_PRI_LO) + char(_ESC);
    Fexpected := ss;
    s := FAddCrc(ss);
    if (EncryptMQ) then begin
      s := Cipher.encrypt(s);
      Result := Result + char(_ESC_START_MQE);
    end else
      Result := Result + char(_ESC_START_MQ);
    Result    := Result + FEncode(s);
    Result    := Result + char(_ESC) + char(_ESC_END);
    Fsent     := Result;
    FTxBusy   := True;
    FRtrCnt   := 0;
    FTxTmout  := TX_TMOUT;
  end;
end;

// =====================================  
// When re-send, add message priority
// =====================================  
function THbRxtx.FAddPriority(ss : string) : string;
begin
  inc(Fpri);
  result := copy(Fsent, 2, Length(Fsent)-1);
  case Fpri of
    1: result := char(_PRI_MED) + result;
    else
        result := char(_PRI_HI) + result;
  end;
end;

// =====================================  
// Transmit
// =====================================  
function THbRxtx.Tx(msg: THbMsg): string;
var s : string;
begin
  result := '';
  if (ComPort.Connected and PortOk) then begin
    try
      if not FTxBusy then begin
        if msg.mqtt then
          s := FEncodeMQ(msg.s)
        else
          s := FEncodeHB(msg.s);
        if (s <> '') then begin
          ComPort.WriteStr(s);
          TxStatus := 1;
          result := s;
        end;
      end;
    except
      if ComPort.Connected then
        ComPort.Connected:=false;
      PortOk := false;
    end;
  end else
    ShowMessage('COM port not connected');
end;

 // =====================================  
 // Clear Rx
 // =====================================  
procedure THbRxtx.FlushRx;
begin
  ErrCnt := 0;
  FEsc   := False;
  FGate  := False;
  Clear;
end;

 // =====================================  
 // Clear Tx
 // =====================================  
procedure THbRxtx.FlushTx;
begin
  Fsent     := '';
  Fexpected := '';
  FTxTmout  := 0;
  FTxBusy   := False;
  TxStatus  := 0;
end;

// =====================================  
// Time tick 10 ms, count time-out and re-send
// =====================================  
procedure THbRxtx.Tick10ms;
var len : integer;
    s : string;
begin
  if ComPort.Connected and PortOk then begin
    try
      // Tx
      if (Fsent <> '') and (FTxTmout > 0) then  begin
        Dec(FTxTmout);
        if FTxTmout = 0 then begin
          if FRtrCnt < 4 then begin
            Inc(FRtrCnt);
            FTxTmout := TX_TMOUT;
            Fsent := FAddPriority(Fsent);
            ComPort.WriteStr(Fsent);
          end else  begin
            FlushTx;
            NoRx := True;
          end;
        end;
      end;
      // Rx
      if (FGateTmout < 1000) then
         inc(FGateTmout);
      if (FGateTmout > 20) then // after 200 ms
        FGate := false;         // gate is expired
      len := ComPort.InputCount;
      if len > 0 then begin
         ComPort.ReadStr(s, len);
         FDecode(s);
      end;
      // Debug
      if (FStr <> '') or (FLastValid) then begin
        inc(FStr_tmout);
        if FStr_tmout > 100 then begin
          if FLastValid then
             FAddDebugStr(FLast);
          DbgList.Add(FStr);
          FLastValid := false;
          FStr := '';
          FStr_tmout := 0;
        end;
      end;
      // Dump
      inc(FDampPause);
      if (FDampPause > 100) and (FDampHex <> '') then begin
        FDampPause := 0;
        DampList.Add(FDampHex+'  '+FDampStr);
        FDampCnt := 0;
        FDampHex := '';
        FDampStr := '';
      end;
    except
      if ComPort.Connected then
         ComPort.Connected:=false;
      PortOk := false;
    end;
  end;
end;

 // =====================================  
 // Create
 // =====================================  
constructor THbRxtx.Create(port : string);
begin
  Cipher := THbCipher.Create;
  ComPort:= TComPort.Create(nil);
  ComPort.BaudRate := br19200;
  ComPort.DataBits := dbEight;
  ComPort.StopBits := sbOneStopBit;
  ComPort.Port := port;
  try
    PortOk := true;
    ComPort.Open;
  except
    if ComPort.Connected then
      ComPort.Connected := false;
    PortOk := false;
  end;
  FlushTx;
  FlushRx;
  MsgID := 1;
  FStr := '';
  DbgList := TStringList.Create;
  DampList := TStringList.Create;
  FDampCnt := 0;
  FDampStr := '';
  FDampHex := '';
  FLastValid := false;
end;

 // =====================================  
 // Destroy
 // =====================================  
destructor THbRxtx.Destroy;
begin
  ComPort.Close;
  ComPort.Free;
  DbgList.Free;
  DampList.Free;
  Cipher.Free;
  inherited Destroy;
end;

end.
