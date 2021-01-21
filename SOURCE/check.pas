UNIT Check;

{$C Fixed Preload Permanent}
INTERFACE

IMPLEMENTATION

USES Strings, WinTypes, WinProcs, Win31, WinDOS, OMemory, ModC;

CONST
  CheckStr: ARRAY[0..7] OF char=('B','L','O','E','D','I','E',#0);
  id_checkfailed = 200;

TYPE
  TCheckRec = RECORD
    FileSize, CheckSum: longint;
  END;
  bytebuffer = ARRAY[0..32*1024-1] OF byte;

{$W-}

PROCEDURE Error;
VAR s: ARRAY[0..fsPathName] OF char;
BEGIN
  MessageBox(0,'The integrity of this file could not be verified and may have been altered.'#13#10+
               'Please check for virusses and debuggers and then reinstall Mod4Win!',
               StrPCopy (s, ParamStr (0)),mb_ok OR mb_iconstop);
  IF PrefixSeg = 0 THEN
  { is a library }
  asm
    mov  sp, bp  { remove error's stack frame }
    pop  bp
    pop  cx      { discard near return address }
    XOR  ax, ax  { indicate an error to Windows }
    mov  sp, bp  { remove check's stack frame }
    pop  bp
    pop  cx      { discard far return address }
    pop  cx
    retf         { return to Windows and remove DLL }
  END ELSE
  { is an application }
  Halt($FF);
END;

VAR
  CheckRec: TCheckRec ABSOLUTE CheckStr;
  f:FILE;
  buf: ^bytebuffer;
  i, j: longint;
  numread:word;

BEGIN
  { open file }
  filemode:=0; Assign(f,ParamStr (0)); Reset(f,1); IF IOResult<>0 THEN Error;
  { check file size }
  IF CheckRec.FileSize <> FileSize (f) THEN Error;
  { calculate checksum }
  buf:=GlobalAllocPtr(gmem_nocompact,SizeOf (bytebuffer));
  j:=0;
  REPEAT
    BlockRead(f,buf^,SizeOf (buf^),numread);
    IF numread>0 THEN FOR i:=0 TO numread-1 DO inc (j, buf^[i]);
  UNTIL numread=0;
  Close(f);
  GlobalFreePtr(buf);
  FOR i:=Low (CheckStr) TO High (CheckStr) DO dec (j, Ord(CheckStr[i]));
  IF CheckRec.CheckSum <> j THEN Error;
END.
