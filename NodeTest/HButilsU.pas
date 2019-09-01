// =====================================================================
// HBus utils
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

unit HButilsU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, IniFiles, Windows, lazutf8sysutils;

function GetLocalTimeZone: Integer;
function DataTimeToEpoch2001(dt : TDateTime) : longword;
function GetUTCtime: longword;
function UTCDateTime: TDateTime;
function Epoch2001ToUTCtime(e2001: longword): TDateTime;
function Epoch2001ToLocalTime(e2001: longword): TDateTime;

//##############################################################################
implementation
//##############################################################################

// ===========================================
// Time zone
// ===========================================
function GetLocalTimeZone: Integer;   // minutes to UTC
var
  BiasType: Byte;
  TZInfo: TTimeZoneInformation;
const
  BS_TYPE_UKNOWN = 0 { Unknown };
  BS_TYPE_STANDARD = 1 { Standard Time };
  BS_TYPE_DAYLIGHT = 2 { Daylight Time };
begin
  Result := 0;
  BiasType := GetTimeZoneInformation(TZInfo);
  if (BiasType=BS_TYPE_UKNOWN) then
    Exit;
  if (BiasType=BS_TYPE_DAYLIGHT) then
    Result := TZInfo.Bias + TZInfo.DaylightBias
  else if (BiasType=BS_TYPE_STANDARD) then
    Result := TZInfo.Bias + TZInfo.StandardBias;
end;

// ===========================================
// Convert TDateTime into epoch2001
// ===========================================
// epoch2001 is sec since 1/1/2001 00:00:00
// dt must be UTC time
function DataTimeToEpoch2001(dt : TDateTime) : longword;
var
  d0: TDateTime;
begin
  d0 := 36892; // StrToDateTime('1/1/2001 00:00:00');
  result := Round(dt*86400) - Trunc(d0*86400);
end;

// ===========================================
// Converts current UTC time into epoch2001
// ===========================================
// epoch2001 is sec since 1/1/2001 00:00:00
function GetUTCtime: longword;
var
  dt, d0 : TDateTime;
begin
  d0 := 36892; // StrToDateTime('1/1/2001 00:00:00');
  dt := NowUTC();
  result := Trunc((dt - d0)*86400);
end;

// ===========================================
// UTC time as TDateTime
// ===========================================
function UTCDateTime: TDateTime;
var
  T: TSystemTime;
begin
  GetSystemTime(T);
  result := EncodeDate(T.wYear, T.wMonth, T.wDay) +
     EncodeTime(T.wHour, T.wMinute, T.wSecond, T.wMilliSeconds);
end;

// ===========================================
// Converts epoch2001 into UTC TDateTime
// ===========================================
// epoch2001 is sec since 1/1/2001 00:00:00
function Epoch2001ToUTCtime(e2001: longword): TDateTime;
var
  dt, d0 : TDateTime;
begin
  d0 := 36892; // StrToDateTime('1/1/2001 00:00:00');
  dt := e2001/86400 + d0;
  result := dt;
end;

// ===========================================
// Converts epoch2001 into local TDateTime
// ===========================================
// epoch2001 is sec since 1/1/2001 00:00:00
function Epoch2001ToLocalTime(e2001: longword): TDateTime;
var
  diff : integer;
begin
  diff := GetLocalTimeZone * 60; // sec
  result := Epoch2001ToUTCtime(e2001 - diff);
end;

end.
