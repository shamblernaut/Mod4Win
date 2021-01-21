{$R INST_EXE.RES}
USES GetOS,WinProcs,WinTypes,Strings,WinDOS,ShellApi{$IFDEF CHECK},Check{$ENDIF};

FUNCTION SetDOSFAttr(aFile:PChar;aAttr:Word):Bool;Assembler;
ASM
  xor ax,ax; DB 66h; cmp word ptr aFile,0; jz @Exit
  mov cx, aAttr
  push ds
  lds dx, aFile
  mov ax, $4301
  INT $21
  jnc @Ok
  pop ds
  mov DosError,ax
  xor ax,ax
  jmp @Exit
@Ok :
  mov ax, 1
@Out:
  pop  ds
@Exit:
END;

FUNCTION EraseDOSFile(aFile:PChar):Bool;assembler;
ASM
  xor ax,ax; DB 66h; cmp word ptr aFile,0; jz @Exit
  push ds
  lds dx, aFile
  mov ax, $4100
  INT $21
  jnc @Ok
  pop ds
  mov DosError,ax
  xor ax,ax
  jmp @Exit
@Ok :
  mov ax, 1
@Out:
  pop  ds
@Exit:
END;

FUNCTION long2ptr(s:word;l:longint):pointer;ASSEMBLER;
ASM
  {Segment berechnen}
  mov ax,word ptr l+2
  mul selectorinc
  mov dx,s
  add dx,ax
  {Offset schreiben}
  mov ax,word ptr l
  {Ergebnis steht in DX:AX}
END;

FUNCTION WriteResToDisk(aInst:Word;Section,ResName:PChar;aName:pChar):LongInt;
VAR ResFindhandle,
    ResLoadhandle :thandle;
    ResPointer    :Pointer;
    fHandle       :Integer;
    ResSize,AktPos:LongInt;
    BlSize        :Word;
BEGIN
  ResFindHandle:=FindResource(aInst,ResName,Section);
  WriteResToDisk:=0;
  IF ResFindHandle=0 THEN EXIT{!};
  ResLoadhandle:=LoadResource(aInst,ResFindhandle);
  IF ResLoadhandle=0 THEN EXIT{!};
  ResPointer   :=LockResource(ResLoadhandle);
  IF ResPointer =NIL THEN BEGIN FreeResource(ResLoadhandle);EXIT{!}; END;
  ResSize      :=SizeOfResource(aInst,ResFindHandle);
  {File kopieren}
  SetDOSFAttr(aName,faarchive);
  fHandle:=_LCreat(aName,0);
  IF fHandle=-1 THEN BEGIN UnlockResource(ResLoadhandle);FreeResource(ResLoadhandle);EXIT{!};END;
  AktPos:=0;
  WHILE ResSize>0 DO
  BEGIN
    BlSize:=32768; IF (ResSize-BlSize)<0 THEN BlSize:=ResSize; DEC(ResSize,BlSize);
    _LWrite(fHandle,long2ptr(SEG(ResPointer^),AktPos),BlSize);
    INC(AktPos,BlSize);
  END;
  _LClose(fHandle);
  UnlockResource(ResLoadhandle); FreeResource(ResLoadhandle);
  WriteResToDisk:=1;
END;

CONST DRV_CONFIGURE = $0007;
TYPE tDriverProc=FUNCTION(dwDriverID:Longint;hDriver,wMsg:Word; lParam1,lParam2:Longint):Longint;

PROCEDURE UnAccociateExtension(Ext:PChar);
VAR XT:ARRAY[0..10]OF Char;
    ModCommand:ARRAY[0..255]OF Char;
    i:Longint;
BEGIN
  WriteProfileString('mci extensions',Ext,NIL);
  StrCopy(XT,'.'#0);
  StrCat(XT,Ext);
  I:=255;
  IF (RegQueryValue(hKey_Classes_Root,'ModulePlayer\shell\open\command',@ModCommand,i)<>ERROR_SUCCESS)OR(I<3)THEN
  BEGIN
    WriteProfileString('extensions',Ext,NIL);
    RegDeleteKey(hKey_Classes_Root,XT);
  END ELSE
  BEGIN
    IF StrRScan(@ModCommand,'%')<>NIL THEN StrRScan(@ModCommand,'%')^:=#0;
    StrCat(@ModCommand,'^.'#0);
    StrCat(@ModCommand,Ext);
    WriteProfileString('extensions',Ext,@ModCommand);
    RegSetValue(hKey_Classes_Root, XT, reg_sz, 'ModulePlayer'#0, StrLen ('ModulePlayer'#0));
  END;
END;

PROCEDURE AccociateExtension(Ext:PChar);
VAR XT:ARRAY[0..80]OF Char;
BEGIN
  WriteProfileString('mci extensions',EXT,'M4W_MCI');
  StrCopy(XT,'MPlayer.exe /Play /Close ^.'#0);
  StrCat(XT,Ext);
  WriteProfileString('extensions',EXT,XT);
  StrCopy(XT,'.'#0);
  StrCat(XT,Ext);
  RegSetValue (hKey_Classes_Root, XT, reg_sz, 'mplayer'#0, StrLen ('mplayer'#0));
END;

VAR PC:ARRAY[0..260]OF Char;
    DProc:tDriverProc;
    i:Word;
BEGIN
  IF OS=Windows_NT THEN
  BEGIN
    MessageBox(0,'The MOD4WIN MCI driver works only in Windows 95 and Windows 3.1x!','Error',MB_IconStop);
    Exit;
  END;
  GetPrivateProfileString('MCI','M4W_MCI','SCHNULLI',PC,SizeOf(PC),'SYSTEM.INI');
  IF (StrIComp(PC,'SCHNULLI')<>0)
  AND((ParamStr(1)<>'/I')OR(ParamStr(1)<>'/i')) THEN
  BEGIN
    IF (ParamStr(1)='/U')OR(ParamStr(1)='/u')
    OR (MessageBox(0,'Dou you want to remove the driver for controlling'#10+
                     'MOD4WIN''s player engine via the standard MCI?','Question',MB_YesNo OR MB_IconQuestion)=ID_Yes) THEN
    BEGIN
      WritePrivateProfileString('MCI','M4W_MCI',NIL,'SYSTEM.INI');
      WritePrivateProfileString('drivers.desc','M4W_MCI.DRV',NIL,'CONTROL.INI');
      UnAccociateExtension('NST'#0);
      UnAccociateExtension('MOD'#0);
      UnAccociateExtension('WOW'#0);
      UnAccociateExtension('OKT'#0);
      UnAccociateExtension('STM'#0);
      UnAccociateExtension('S3M'#0);
      UnAccociateExtension('669'#0);
      UnAccociateExtension('FAR'#0);
      UnAccociateExtension('MTM'#0);
      UnAccociateExtension('XM'#0);
      GetSystemDirectory(PC,SizeOf(PC));
      StrCat(PC,'\'#0);
      StrCat(PC,'M4W_MCI.DRV'#0);
      SetDOSFAttr(PC,faarchive);
      IF NOT EraseDOSFile(PC) THEN MessageBox(0,'Could not delete M4W_MCI.DRV!','Warning',MB_IconExclamation);
    END;
  END ELSE
  BEGIN
    IF (ParamStr(1)='/I')OR(ParamStr(1)='/i')
    OR (MessageBox(0,'Dou you want to install the driver for controlling'#10+
                     'MOD4WIN''s player engine via the standard MCI?','Question',MB_YesNo OR MB_IconQuestion)=ID_Yes) THEN
    BEGIN
      WritePrivateProfileString('MCI','M4W_MCI','M4W_MCI.DRV','SYSTEM.INI');
      WritePrivateProfileString('drivers.desc','M4W_MCI.DRV','[MCI] MOD4WIN Driver','CONTROL.INI');
      AccociateExtension('NST'#0);
      AccociateExtension('MOD'#0);
      AccociateExtension('WOW'#0);
      AccociateExtension('OKT'#0);
      AccociateExtension('STM'#0);
      AccociateExtension('S3M'#0);
      AccociateExtension('669'#0);
      AccociateExtension('FAR'#0);
      AccociateExtension('MTM'#0);
      AccociateExtension('XM'#0);
      GetSystemDirectory(PC,SizeOf(PC));
      StrCat(PC,'\'#0);
      StrCat(PC,'M4W_MCI.DRV'#0);
      IF WriteResToDisk(hInstance,'EXECUTABLES',MakeIntResource(100),PC)=0
      THEN MessageBox(0,'Could not write M4W_MCI.DRV!','Error!',MB_IconStop)
      ELSE IF (ParamStr(1)<>'/I')AND(ParamStr(1)<>'/i') THEN
      BEGIN
        I:=LoadLibrary(PC);
        IF I>32 THEN
        BEGIN
          @DProc:=GetProcAddress(i,'DRIVERPROC');
          IF @DProc<>NIL THEN DProc(0,0,DRV_CONFIGURE,0,0);
          FreeLibrary(i);
        END;
      END;
    END;
  END;
END.