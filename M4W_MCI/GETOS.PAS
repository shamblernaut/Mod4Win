UNIT GetOS;
INTERFACE
USES WinTypes,WinProcs;

CONST Windows_NT=2;
      Windows_95=1;
      Windows_31=0;

      Ex_OldWin =0;
      Ex_NewWin =1;
      Ex_W32S   =2;

CONST OS  :Byte=0;
      ExOS:Byte=0;

TYPE pOSVERSIONINFO=^tOSVERSIONINFO;
     tOSVERSIONINFO=RECORD
       dwOSVersionInfoSize:Longint;
       dwMajorVersion:Longint;
       dwMinorVersion:Longint;
       dwBuildNumber:Longint;
       dwPlatformId:Longint;
       szCSDVersion:ARRAY[1..128]OF Char;
     END;

IMPLEMENTATION

CONST lGetVersionExA:LongInt=0;
CONST hKrnl16  :THandle = 0;
CONST hKrnl32  :LongInt = 0;
CONST _32bitOn:Bool    = False;

TYPE TLoadLibraryEx32W =FUNCTION(LibName:PChar;hFile:LongInt;dwFlags:LongInt):LongInt; {513}
     TFreeLibrary32W   =FUNCTION(hMod32:LongInt):LongInt;                              {514}
     TGetProcAddress32W=FUNCTION(hMod32:LongInt;Procname:PChar):LongInt;               {515}
     TCallProc32W      =Pointer;

VAR lLoadLibraryEx32W  :TLoadLibraryEx32W;
    lFreeLibrary32W    :TFreeLibrary32W;
    lGetProcAddress32W :TGetProcAddress32W;
    CallProc32W        :TCallProc32W;

CONST
    VER_PLATFORM_WIN32s       =0;
    VER_PLATFORM_WIN32_WINDOWS=1;
    VER_PLATFORM_WIN32_NT     =2;

FUNCTION  GetVersionEx(Info:pOSVERSIONINFO):Longint;ASSEMBLER;
ASM
  DB 66h; xor ax,ax; DB 66h; xor dx,dx
  DB 66h; cmp word ptr lGetVersionExA,0
  jz @Exit
  les di,Info
  mov ax,Type tOSVERSIONINFO
  mov word ptr tOSVERSIONINFO(ES:[di]).dwOSVersionInfoSize,ax
  mov word ptr tOSVERSIONINFO(ES:[di]).dwOSVersionInfoSize+2,0
  DB 66h; push word ptr Info
  DB 66h; push word ptr lGetVersionExA
  push 0000000000000000b; push 0000000000000001b
  push 0; push 1   {1 dword parm}
  Call CallProc32W
@Exit:
END;

VAR OS_Info:tOSVERSIONINFO;
BEGIN
  {$I-}
  hKrnl16:=GetModuleHandle('KERNEL');
  @lLoadLibraryEx32W :=GetProcAddress(hKrnl16,'LoadLibraryEx32W');
  @lFreeLibrary32W   :=GetProcAddress(hKrnl16,'FreeLibrary32W');
  @lGetProcAddress32W:=GetProcAddress(hKrnl16,'GetProcAddress32W');
  CallProc32W        :=GetProcAddress(hKrnl16,'CallProc32W');
  _32bitOn:=@lLoadLibraryEx32W<>NIL;
  IF _32BitOn THEN
  BEGIN
    hKrnl32  :=lLoadLibraryEx32W('KERNEL32',0,0);
    lGetVersionExA:=lGetProcAddress32W(hKrnl32,'GetVersionExA');
    lFreeLibrary32W(hKrnl32);
  END ELSE OS:=Windows_NT;
  IF GetVersionEx(@OS_Info)=0 THEN
  BEGIN
    OS:=Windows_31;
    OS_Info.dwMajorVersion:=3;
    OS_Info.dwMinorVersion:=11;
    OS_Info.dwBuildNumber:=1;
    OS_Info.dwPlatformId:=0;
    FillChar(OS_Info.szCSDVersion,SizeOf(OS_Info.szCSDVersion),0);
  END ELSE
  BEGIN
    CASE OS_Info.dwPlatformId OF
      VER_PLATFORM_WIN32s       :BEGIN OS:=Windows_31; ExOs:=Ex_OldWin OR Ex_W32S; END;
      VER_PLATFORM_WIN32_WINDOWS:BEGIN OS:=Windows_95; ExOs:=Ex_NewWin;            END;
      VER_PLATFORM_WIN32_NT     :BEGIN
                                   OS:=Windows_NT;
                                   IF OS_Info.dwMajorVersion<4 THEN ExOs:=Ex_OldWin
                                                               ELSE ExOs:=Ex_NewWin;
                                 END;
    END;
  END;
END.