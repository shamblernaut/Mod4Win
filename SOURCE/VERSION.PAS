{ version checking unit by Jensi, copyright (c) 1995 JSInc. }

unit version;

interface

var ProductVersion: word;

implementation

uses Strings, WinProcs, WinDOS, Ver, OMemory;

procedure GetVersionInfo;
var
  FileName: array [0..fsPathName] of char;
  Handle, Len: longint;
  Data: PChar;
  ResultBuffer: ^Tvs_FixedFileInfo;
  ResultLen: word;
  ProductVersionMS, ProductVersionLS: longint;
begin
  Len := GetFileVersionInfoSize (StrPCopy (FileName, ParamStr (0)), Handle);
  if Len = 0 then begin ProductVersion := 0; exit; end;
  Data := MemAlloc (Len);
  GetFileVersionInfo (FileName, Handle, Len, Data);
  VerQueryValue (Data, '\', pointer (ResultBuffer), ResultLen);
  ProductVersionMS := ResultBuffer^.dwProductVersionMS;
  ProductVersionLS := ResultBuffer^.dwProductVersionLS;
  ProductVersion := HiWord (ProductVersionMS) * 100 + LoWord (ProductVersionMS) * 10 + HiWord (ProductVersionLS);
  FreeMem (Data, Len);
end;

begin
  GetVersionInfo;
end.
