// =============================================================================
//HBus node test
// =============================================================================
{
Author    A.Kouznetsov

Redistribution and use in source and binary forms, with or without modification, are permitted.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
unit main;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, FileUtil, CPort, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Registry, IniFiles, HBrxtxU, HBcmdU;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnRev : TButton;
    BtnStatus : TButton;
    BtnCollect : TButton;
    BtnPing : TButton;
    BtnNewID : TButton;
    BtnBoot : TButton;
    BtnBeep : TButton;
    BtnRdDescr : TButton;
    BtnWrDescr : TButton;
    CbPorts : TComboBox;
    EdGroup : TEdit;
    EdBootPause : TEdit;
    EdDuration : TEdit;
    EdDescr : TEdit;
    EdMsgId : TEdit;
    EdOwnId : TEdit;
    EdNewID : TEdit;
    EdPause : TEdit;
    EdSlots : TEdit;
    EdNode : TEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    LB : TListBox;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Timer10ms : TTimer;
    procedure BtnBeepClick(Sender : TObject);
    procedure BtnBootClick(Sender : TObject);
    procedure BtnCollectClick(Sender : TObject);
    procedure BtnNewIDClick(Sender : TObject);
    procedure BtnPingClick(Sender : TObject);
    procedure BtnRdDescrClick(Sender : TObject);
    procedure BtnRevClick(Sender : TObject);
    procedure BtnStatusClick(Sender : TObject);
    procedure BtnWrDescrClick(Sender : TObject);
    procedure CbPortsChange(Sender : TObject);
    procedure EdMsgIdDblClick(Sender : TObject);
    procedure EdNewIDDblClick(Sender : TObject);
    procedure EdNodeDblClick(Sender : TObject);
    procedure EdOwnIdDblClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Label2DblClick(Sender : TObject);
    procedure LBDblClick(Sender : TObject);
    procedure Timer10msTimer(Sender : TObject);
  private
    { private declarations }
    ComPort : string;
    NodeID : word;
    NewID : word;
  public
    { public declarations }
    function StrToHex(s : string) : string;
    procedure PrintHbMsg(msg : THbMsg);
    function num_str_c2pas(s : string) : string;
  end;

var
  Form1 : TForm1;
  HB : THbRxtx;
  HBcmd : THbCmd;
  TxMsg, RxMsg : THbMsg;

//##############################################################################
implementation
//##############################################################################

{ TForm1 }

{$R *.lfm}

// =====================================================
// Find all existing at the moment COM ports
// =====================================================
procedure TForm1.Label2DblClick(Sender : TObject);
var
  i, cnt: integer;
  reg: TRegistry;
  ts: TStringList;
  s: string;
begin
  CbPorts.Clear;
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.OpenKey('hardware\devicemap\serialcomm', False);
  ts := TStringList.Create;
  // ------------------------------
  // read all COM ports from registry
  // ------------------------------
  reg.GetValueNames(ts);
  // ------------------------------
  // out of all serial ports, select USB virtual COM ports
  // ------------------------------
  cnt := 0;
  for i := 0 to ts.Count - 1 do  begin
    s := reg.ReadString(ts.Strings[i]);
    if (s <> 'COM1') then begin
      CbPorts.AddItem(s, nil);
      if CbPorts.ItemIndex < 0 then
        CbPorts.ItemIndex := 0;
      if s = ComPort then
        CbPorts.ItemIndex := cnt;
      inc(cnt);
    end;
  end;
  ts.Free;
  reg.Free;
end;

// =====================================================
// Clear
// =====================================================
procedure TForm1.LBDblClick(Sender : TObject);
begin
  LB.Clear;
end;

// =====================================================
// 10 ms
// =====================================================
procedure TForm1.Timer10msTimer(Sender : TObject);
var i : integer;
    rx : THbMsg;
begin
  HBcmd.Tick10ms;
  HB.Tick10ms;
  rx := HB.Rx;
  if (HB.DbgStr.Count > 0) then begin
    for i:=0 to HB.DbgStr.Count-1 do
      LB.Items.Add(HB.DbgStr.Strings[i]);
    HB.DbgStr.Clear;
  end;
  if rx.valid then begin
    PrintHbMsg(rx);
  end;
  if HB.TxStatus = 2 then begin
    // LB.Items.Add('Echo OK');
    HB.TxStatus := 0;
  end;
end;

// =====================================================
// Convert binary message into hex str
// =====================================================
function TForm1.StrToHex(s : string) : string;
var i : integer;
    c : char;
begin
  result := '';
  for i:=1 to length(s) do begin
    c := s[i];
    result := result + IntToHex(ord(c),2) + ' ';
    if (i and 7)=0 then
      result := result + ' ';
  end;
end;

// =====================================================
// Print message
// =====================================================
procedure TForm1.PrintHbMsg(msg : THbMsg);
var s : string;
begin
  s := IntToHex(msg.pri, 2);
  if msg.hb then
    s := s + ' HBus '
  else
    s := s + ' MQTT ';
  s := s + StrToHex(msg.s);
  LB.Items.Add(s);;
end;

// =====================================================
//
// =====================================================
function TForm1.num_str_c2pas(s : string) : string;
var ss : string;
begin
  s := AnsiLowerCase(Trim(s));
  result := s;
  if length(s)>2 then begin
    ss := copy(s, 1, 2);
    if ss = '0x' then begin
      result := '$'+copy(s,3,length(s)-2);
    end;
  end;
end;

// =====================================================
// Create
// =====================================================
procedure TForm1.FormCreate(Sender : TObject);
var ini : TIniFile;
begin
  ini := TIniFile.Create('NodeTest.ini');
  ComPort := AnsiUpperCase(ini.ReadString('Serial','Port','COM1'));
  NodeID := ini.ReadInteger('Node','ID',$FFFF);
  ini.Free;
  EdNode.Text := '0x' + IntToHex(NodeID,4);
  Label2DblClick(Sender); // select COM port
  HB := THbRxtx.Create(ComPort);
  if HB.ComPort.Connected then
    Label3.Caption:='Connected'
  else
    Label3.Caption:='Disonnected';
  HBcmd := THbCmd.Create;
  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,4);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Destroy
// =====================================================
procedure TForm1.FormDestroy(Sender : TObject);
var ini : TIniFile;
begin
  ini := TIniFile.Create('NodeTest.ini');
  ini.WriteString('Serial','Port',AnsiUpperCase(ComPort));
  ini.WriteInteger('Node','ID',NodeID);
  ini.Free;
  HB.Free;
  HBcmd.Free;
end;

// =====================================================
// Change COM port
// =====================================================
procedure TForm1.CbPortsChange(Sender : TObject);
begin
  ComPort := CbPorts.Text;
  if HB.ComPort.Connected then begin
    HB.FlushTx;
    HB.ComPort.Close;
  end;
  Label3.Caption:='Disonnected';
  HB.ComPort.Port := ComPort;
  try
    HB.ComPort.Open;
    if HB.ComPort.Connected then
      Label3.Caption:='Connected';
  except
    on EComPort do begin
      HB.ComPort.Connected := false;
    end;
  end;
  HB.FlushRx;
  HB.FlushTx;
end;

// =====================================================
// Read node REV
// =====================================================
procedure TForm1.BtnRevClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRev(NodeID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Collect nodes
// =====================================================
procedure TForm1.BtnCollectClick(Sender : TObject);
var s : string;
    grp, slots : integer;
begin
  HBcmd.Flush;
  grp := StrToIntDef(EdGroup.Text,1);
  EdGroup.Text := IntToStr(grp);
  slots := StrToIntDef(EdSlots.Text,32);
  if (slots < 8) then
    slots := 8;
  if (slots > 255) then
    slots := 255;
  EdSlots.Text := IntToStr(slots);
  TxMsg := HBcmd.CmdCollect(grp, slots);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Boot
// =====================================================
procedure TForm1.BtnBootClick(Sender : TObject);
var pause : integer;
    s : string;
begin
  HBcmd.Flush;
  pause := StrToIntDef(EdBootPause.Text, 10); // sec
  EdBootPause.Text := IntToStr(pause);
  TxMsg := HBcmd.CmdBoot(NodeID, pause);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Beep
// =====================================================
procedure TForm1.BtnBeepClick(Sender : TObject);
var dur : integer;
    s : string;
begin
  HBcmd.Flush;
  dur := StrToIntDef(EdDuration.Text, 2); // sec
  EdDuration.Text := IntToStr(dur);
  TxMsg := HBcmd.CmdBeep(NodeID, dur);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
//  Set New ID
// =====================================================
procedure TForm1.BtnNewIDClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  EdNewIDDblClick(Sender);
  TxMsg := HBcmd.CmdSetID(NodeID, NewID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// PING
// =====================================================
procedure TForm1.BtnPingClick(Sender : TObject);
var pause : integer;
    s : string;
begin
  HBcmd.Flush;
  pause := StrToIntDef(EdPause.Text, 0); // sec
  EdPause.Text := IntToStr(pause);
  TxMsg := HBcmd.CmdPing(NodeID, pause);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Read Description
// =====================================================
procedure TForm1.BtnRdDescrClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRdDescr(NodeID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Write Description
// =====================================================
procedure TForm1.BtnWrDescrClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  s := Trim(EdDescr.Text);
  if length(s) > 63 then
    s := copy(s,1,64);
  EdDescr.Text := s;
  TxMsg := HBcmd.CmdWrDescr(NodeID, s);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Read node STATUS
// =====================================================
procedure TForm1.BtnStatusClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdStatus(NodeID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Node ID
// =====================================================
procedure TForm1.EdNodeDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNode.Text);
  NodeID := StrToIntDef(s, $FFFF);
  EdNode.Text := '0x'+IntToHex(NodeID,4);
end;

// =====================================================
// Own ID
// =====================================================
procedure TForm1.EdOwnIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdOwnId.Text);
  HBcmd.OwnID := StrToIntDef(s, $FFFF);
  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,4)
end;

// =====================================================
// MsgID
// =====================================================
procedure TForm1.EdMsgIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdMsgId.Text);
  HBcmd.MsgID := StrToIntDef(s, $FFFF);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4)
end;


// =====================================================
// New ID
// =====================================================
procedure TForm1.EdNewIDDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNewId.Text);
  NewID := StrToIntDef(s, $FFFF);
  EdNewID.Text := '0x'+IntToHex(NewID,4)
end;

end.

