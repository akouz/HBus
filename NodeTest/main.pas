// =============================================================================
// HBus node test
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
unit main;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, FileUtil, CPort, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Registry, IniFiles, Clipbrd, HBrxtxU, HBcmdU;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnPublish : TButton;
    BtnRev : TButton;
    BtnStatus : TButton;
    BtnCollect : TButton;
    BtnPing : TButton;
    BtnNewID : TButton;
    BtnBoot : TButton;
    BtnBeep : TButton;
    BtnRdDescr : TButton;
    BtnWrDescr : TButton;
    BtnCustomCmd : TButton;
    BtnRdTopic : TButton;
    BtnRegister : TButton;
    CbPorts : TComboBox;
    CbDamp : TCheckBox;
    CbStatus : TCheckBox;
    EdGroup : TEdit;
    EdBootPause : TEdit;
    EdDuration : TEdit;
    EdDescr : TEdit;
    EdCustomCmd : TEdit;
    EdTopic : TEdit;
    EdTopic1 : TEdit;
    EdTopicI : TEdit;
    EdMsgId : TEdit;
    EdOwnId : TEdit;
    EdNewID : TEdit;
    EdPause : TEdit;
    EdSlots : TEdit;
    EdNode : TEdit;
    EdTopicName : TEdit;
    EdTopicVal : TEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    LB : TListBox;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    Timer1sec : TTimer;
    TsHBus : TTabSheet;
    TabSheet2 : TTabSheet;
    Timer10ms : TTimer;
    procedure BtnBeepClick(Sender : TObject);
    procedure BtnBootClick(Sender : TObject);
    procedure BtnCollectClick(Sender : TObject);
    procedure BtnCustomCmdClick(Sender : TObject);
    procedure BtnPublishClick(Sender : TObject);
    procedure BtnNewIDClick(Sender : TObject);
    procedure BtnPingClick(Sender : TObject);
    procedure BtnRdDescrClick(Sender : TObject);
    procedure BtnRdTopicClick(Sender : TObject);
    procedure BtnRegisterClick(Sender : TObject);
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
    procedure LBKeyPress(Sender : TObject; var Key : char);
    procedure Timer10msTimer(Sender : TObject);
    procedure Timer1secTimer(Sender : TObject);
  private
    { private declarations }
    ComPortStr : string;
    NodeID : word;
    NewID : word;
    WasConnected : boolean;
  public
    { public declarations }
    function StrToHex(s : string; txt_i : byte) : string;
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
      if s = ComPortStr then
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
// Ctrl+C to copy selected text
// =====================================================
procedure TForm1.LBKeyPress(Sender : TObject; var Key : char);
var s : string;
    i : integer;
begin
  if Ord(Key) = 3 then begin
    if LB.SelCount > 0 then begin
      s := '';
      for i:=0 to LB.Items.Count-1 do begin
        if LB.Selected[i] then
        s := s + LB.Items.Strings[i] + char(13);
      end;
      Clipboard.AsText := s;
    end;
  end;
end;

// =====================================================
// 10 ms
// =====================================================
procedure TForm1.Timer10msTimer(Sender : TObject);
var i, cnt : integer;
    rx : THbMsg;
    msg_id : word;
begin
   if HB.ComPort.Connected then begin
     try
       HBcmd.Tick10ms;
        HB.Tick10ms;
        rx := HB.Rx;
        // debug messages
        cnt := HB.DbgList.Count;
        if (cnt > 0) then begin
          for i:=0 to cnt-1 do
            LB.Items.Add(' - dbg: '+HB.DbgList.Strings[i]);
          for i:=0 to cnt-1 do
            HB.DbgList.Delete(cnt-1-i);
        end;
        // dump
        cnt := HB.DampList.Count;
        if (cnt > 0) then begin
          if (cnt > 0) then begin
            if CbDamp.Checked then begin
              for i:=0 to cnt-1 do
                LB.Items.Add(' --- bus: '+HB.DampList.Strings[i]);
            end;
            for i:=0 to cnt-1 do
              HB.DampList.Delete(cnt-1-i);
          end;
        end;
        if rx.valid then begin
          if (rx.mqtt) and (rx.err = false) then begin
            msg_id := HB.MsgID;   // received
            if (msg_id >= HBcmd.MsgId) or ((HBcmd.MsgId > $FFF0) and (msg_id < $10)) then begin
               HBcmd.MsgID := msg_id +1;
               if (HBcmd.MsgID > $FFF0) or (HBcmd.MsgID = 0) then
                 HBcmd.MsgID := 1;
               EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
            end;
          end;
          PrintHbMsg(rx);
          rx.valid := false;
        end;
        if HB.TxStatus = 2 then begin
        // LB.Items.Add('Echo OK');
        HB.TxStatus := 0;
        end;
    except
      if HB.ComPort.Connected then
        HB.ComPort.Connected := false;
      HB.PortOk := false;
    end;
  end;
end;

// =====================================================
// 1 sec
// =====================================================
procedure TForm1.Timer1secTimer(Sender : TObject);
begin
  if (not HB.ComPort.Connected) and (not WasConnected) then begin
    Label2DblClick(Sender);
    CbPortsChange(Sender);
  end;
  WasConnected := HB.ComPort.Connected;
  if CbStatus.Checked then
     BtnStatusClick(Sender);
end;

// =====================================================
// Convert binary message into hex str
// =====================================================
function TForm1.StrToHex(s : string; txt_i : byte) : string;
var i, hl : integer;
    c : char;
begin
  result := '';
  if (txt_i = 0) then
    hl := length(s)
  else
    hl := txt_i; //
  for i:=1 to hl do begin
    c := s[i];
    result := result + IntToHex(ord(c),2) + ' ';
    if (i and 7)=0 then
      result := result + ' ';
  end;
  if (txt_i > 0) then begin
    result := result + copy(s, hl+1, length(s)- hl);
  end;
end;

// =====================================================
// Print message
// =====================================================
procedure TForm1.PrintHbMsg(msg : THbMsg);
var s : string;
    cmd : byte;
    OkErr : byte;
    TopicId, NewTopicId : word;
begin
  s := IntToHex(msg.pri, 2); // show priority byte (prefix)
  cmd := ord(msg.s[1]);
  OkErr := ord(msg.s[8]);
  // ---------------------------
  // HBus message
  // ---------------------------
  if (not msg.mqtt) then begin
    s := s + ' HBus ';
    if (cmd = $82) and (OkErr = 1) then    // STATUS reply
      s := s + StrToHex(msg.s, 8)          // JSON
    else if ((cmd = $88) or (cmd = 9)) then
      s := s + StrToHex(msg.s, 9)          // description
    else if (cmd = $A) or (cmd = $8A) then // custom cmd and reply
      s := s + StrToHex(msg.s, 8)
    else if (cmd = $8B) then begin         // topic
      if (OkErr = 0) then begin
        NewTopicId := $100*ord(msg.s[9]) + ord(msg.s[10]);
        s := s + StrToHex(msg.s, 10);
        s := s + ' <'+IntToStr(NewTopicId)+'>';
      end else begin
        s := s + StrToHex(msg.s, 0);
        EdTopicI.Text:='0';
      end;
    end else
      s := s + StrToHex(msg.s, 0); // binary
  // ---------------------------
  // MQTT message
  // ---------------------------
  end  else begin
    s := s + ' MQTT ';
    TopicId := $100*ord(msg.s[4]) + ord(msg.s[5]);
    if (OkErr = 1) then begin
      s := s + StrToHex(msg.s, 8) // JSON
    end else begin
      s := s + StrToHex(msg.s, 0); // binary
    end;
    s := s + ' <TopicId=';
    if (TopicId > 0) then
      s := s + IntToStr(TopicId) + '>'
    else
      s := s + '?>';
  end;
  LB.Items.Add(s);;
end;

// =====================================================
// C number string into Pascal number string
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
  ComPortStr := AnsiUpperCase(ini.ReadString('Serial','Port','COM1'));
  NodeID := ini.ReadInteger('Node','ID',$FFFF);
  EdTopic.Text := ini.ReadString('MQTT','topic','101');
  EdTopicVal.Text := ini.ReadString('MQTT','val','12.3');
  ini.Free;
  EdNode.Text := '0x' + IntToHex(NodeID,4);
  EdNewID.Text := EdNode.Text;
  Label2DblClick(Sender); // select COM port
  HB := THbRxtx.Create(ComPortStr);
  if HB.ComPort.Connected then
    Label3.Caption:='Connected'
  else
    Label3.Caption:='Disonnected';
  HBcmd := THbCmd.Create;
  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,4);
  EdOwnIdDblClick(Sender);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Destroy
// =====================================================
procedure TForm1.FormDestroy(Sender : TObject);
var ini : TIniFile;
begin
  ini := TIniFile.Create('NodeTest.ini');
  if HB.ComPort.Connected then
    ini.WriteString('Serial','Port',AnsiUpperCase(ComPortStr));
  ini.WriteInteger('Node','ID',NodeID);
  ini.WriteString('MQTT','topic',EdTopic.Text);
  ini.WriteString('MQTT','val',EdTopicVal.Text);
  ini.Free;
  HB.Free;
  HBcmd.Free;
end;

// =====================================================
// Change COM port
// =====================================================
procedure TForm1.CbPortsChange(Sender : TObject);
begin
  ComPortStr := CbPorts.Text;
  if HB.ComPort.Connected then begin
    HB.FlushTx;
    HB.ComPort.Close;
  end;
  Label3.Caption:='Disonnected';
  if Trim(ComPortStr) <> '' then begin
    HB.ComPort.Port := ComPortStr;
    try
      HB.PortOk := true;
      HB.ComPort.Open;
      if HB.ComPort.Connected then
        Label3.Caption:='Connected';
    except
      if HB.ComPort.Connected then
        HB.ComPort.Connected := false;
      HB.PortOk := false;
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
// Send costom command
// =====================================================
procedure TForm1.BtnCustomCmdClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdCustom(NodeID, EdCustomCmd.Text);
  if TxMsg.valid then begin
    s := HB.Tx(TxMsg);
    inc(HBcmd.MsgId);
  end;
end;

// =====================================================
// Send MQTT message PUBLISH
// =====================================================
procedure TForm1.BtnPublishClick(Sender : TObject);
var topicId, msg_id : word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDef(EdTopic.text,100);
  EdTopic.text := IntToStr(topicId);
  TxMsg := HBcmd.Publish(topicId, HBcmd.MsgID, EdTopicVal.Text);
  s := HB.Tx(TxMsg);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FFFE then
    HBcmd.MsgId := 1;
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4);
end;

// =====================================================
// Send MQTT message REGISTER
// =====================================================
procedure TForm1.BtnRegisterClick(Sender : TObject);
var topicId, msg_id : word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDef(EdTopic1.text,100);
  TxMsg := HBcmd.Register(topicId, HBcmd.MsgID, EdTopicName.Text);
  s := HB.Tx(TxMsg);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FFFE then
    HBcmd.MsgId := 1;
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
// Read TopicId and TopicName
// =====================================================
procedure TForm1.BtnRdTopicClick(Sender : TObject);
var ti : byte;
    s : string;
begin
  ti := StrToIntDef(EdTopicI.Text,0);
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRdTopic(NodeID, ti);
  s := HB.Tx(TxMsg);
  EdTopicI.Text := IntToStr(ti+1);
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
  NodeID := StrToIntDef(s, $07FF);
  EdNode.Text := '0x'+IntToHex(NodeID,4);
end;

// =====================================================
// Own ID
// =====================================================
procedure TForm1.EdOwnIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdOwnId.Text);
  HBcmd.OwnID := StrToIntDef(s, $07FF);
  if (HBcmd.OwnID < 1) then
     HBcmd.OwnID := 1;
  if (HBcmd.OwnID > $07FF) then
     HBcmd.OwnID := $7FF;
  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,4)
end;

// =====================================================
// MsgID
// =====================================================
procedure TForm1.EdMsgIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdMsgId.Text);
  HBcmd.MsgID := StrToIntDef(s, $FFFE);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,4)
end;


// =====================================================
// New ID
// =====================================================
procedure TForm1.EdNewIDDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNewId.Text);
  NewID := StrToIntDef(s, $07FF);
  if (NewID = 0) then
     NewID := 1;
  if (NewID > $07FF) then
     NewID := $07FF;
  EdNewID.Text := '0x'+IntToHex(NewID,4)
end;

end.

