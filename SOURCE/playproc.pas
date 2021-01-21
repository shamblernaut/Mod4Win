UNIT Playproc;
{ $C Fixed Preload Permanent}
{$DEFINE DLL}
INTERFACE

USES Version,modc,winprocs,wintypes,win31{$IFNDEF DLL},Player32{$ENDIF},Coroner;

CONST modinfo:pModulInfo=NIL;

TYPE Tmod_Stop           =PROCEDURE;
     tCriticalPlayerClose=FUNCTION :Pointer;
     Tmod_Play           =FUNCTION (hPWindow:hWnd; Filename, SaveName: pChar; FNamesOEM: Bool;
                                    Settings:pSettings; PauseIt, _RepeatSong:Boolean): Word;
     Tmod_SettingsChanged=PROCEDURE(VAR settings:tsettings; VAR Error:word);
     Tmod_Repeat         =PROCEDURE(rep:boolean);
     tRepeatPattern      =PROCEDURE(How:Boolean);
     Tmod_GetDataPtr     =FUNCTION :POINTER;
     Tmod_GetTimePattern =FUNCTION (Time:Longint):Byte;
     Tmod_EnableChn      =PROCEDURE(Chn:Byte; How:Boolean);
     Tmod_EnableAllChn   =PROCEDURE(How:Boolean);
     Tmod_IsEnabled      =FUNCTION (Chn:Byte):Boolean;
     Tmod_CanPlay        =FUNCTION (VAR Settings: TSettings; OvrRateChng: boolean): boolean;
     Tmod_GetVersion     =FUNCTION:word;
     tPatternSearch      =PROCEDURE (NewPattern:Word);


VAR  mod_Stop,
     mod_Timer          :   Tmod_Stop;
     CriticalPlayerClose:tCriticalPlayerClose;
     mod_Play:            Tmod_Play;
     mod_GetTimePattern:  Tmod_GetTimePattern;
     mod_SettingsChanged: Tmod_SettingsChanged;
     mod_Pause,
     mod_EndPause,
     mod_Repeat:          Tmod_Repeat;
     mod_GetDataPtr:      Tmod_GetDataPtr;
     mod_EnableChn:       Tmod_EnableChn;
     mod_EnableAllChn:    Tmod_EnableAllChn;
     mod_IsEnabled:       Tmod_IsEnabled;
     mod_CanPlay:         Tmod_CanPlay;
     mod_GetVersion:      Tmod_GetVersion;
     PatternSearch       :tPatternSearch;
     RepeatPattern       :tRepeatPattern;

PROCEDURE ReleasePlayer;
PROCEDURE InstallPlayer;

IMPLEMENTATION

VAR s:STRING;
    hplayer:Thandle;
    olderrmode:word;


PROCEDURE ReleasePlayer;
BEGIN
{$IFDEF DLL}
  IF hPlayer<>0 THEN FreeLibrary(hplayer);
  hPlayer:=0;
{$ENDIF}
END;

PROCEDURE InstallPlayer;
BEGIN
{$IFDEF DLL}
  s:=m4wdir+DLL+#0;
  olderrmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  hplayer:=loadMylibrary(@s[1]);
  SetErrorMode(olderrmode);
  {IST HANDLE VALID!}
  IF hplayer>32 THEN
  BEGIN
    IF GetModuleUsage(hplayer)>1 THEN
    BEGIN
      MessageBox(0,DLL+' is already in use by another application.','Fatal Error',mb_ok OR mb_iconstop);
      Halt(0);
    END;
    @mod_Stop            :=GetProcAddress(hplayer,makeintresource(1));
    @mod_Play            :=GetProcAddress(hplayer,makeintresource(2));
    @mod_Pause           :=GetProcAddress(hplayer,makeintresource(3));
    @mod_EndPause        :=GetProcAddress(hplayer,makeintresource(4));
    @mod_GetTimePattern  :=GetProcAddress(hplayer,makeintresource(5));
    @mod_SettingsChanged :=GetProcAddress(hplayer,makeintresource(6));
    @mod_Repeat          :=GetProcAddress(hplayer,makeintresource(7));
    @mod_GetDataPtr      :=GetProcAddress(hplayer,makeintresource(8));
    @mod_EnableChn       :=GetProcAddress(hplayer,makeintresource(9));
    @mod_IsEnabled       :=GetProcAddress(hplayer,makeintresource(10));
    @mod_CanPlay         :=GetProcAddress(hplayer,makeintresource(11));
    @PatternSearch       :=GetProcAddress(hplayer,makeintresource(12));
    @RepeatPattern       :=GetProcAddress(hplayer,makeintresource(13));
    @mod_GetVersion      :=GetProcAddress(hplayer,makeintresource(14));
    @mod_EnableAllChn    :=GetProcAddress(hplayer,makeintresource(15));
    @CriticalPlayerClose :=GetProcAddress(hplayer,makeintresource(16));
    @mod_Timer           :=GetProcAddress(hplayer,makeintresource(99));
    IF (@mod_Stop=NIL)
     OR(@mod_Play=NIL)
     OR(@mod_Pause=NIL)
     OR(@mod_EndPause=NIL)
     OR(@mod_GetTimePattern=NIL)
     OR(@mod_SettingsChanged=NIL)
     OR(@mod_Repeat=NIL)
     OR(@mod_GetDataPtr=NIL)
     OR(@mod_EnableChn=NIL)
     OR(@mod_EnableAllChn=NIL)
     OR(@mod_IsEnabled=NIL)
     OR(@mod_CanPlay=NIL)
     OR(@mod_GetVersion=NIL)
     OR(@mod_Timer=NIL)
     OR(@CriticalPlayerClose=NIL)
     THEN
     BEGIN
       MessageBox(0,'Could not load '+DLL+'.','Fatal Error',mb_ok OR mb_iconstop);
       Halt(0);
     END;
     IF mod_GetVersion DIV 10 <> ProductVersion DIV 10 THEN
     BEGIN
       MessageBox(0,'Wrong version of '+DLL+'.','Fatal Error',mb_ok OR mb_iconstop);
       Halt(0);
     END;
     modinfo             :=MOD_GetDataPtr;
  END ELSE
  BEGIN
    MessageBox(0,DLL+' not found or corrupt.','Fatal Error',mb_ok OR mb_iconstop);
    Halt(0);
  END;
{$ELSE}
  mod_Stop            :=Stop;
  mod_Play            :=Play;
  mod_Pause           :=BeginPause;
  mod_EndPause        :=EndPause;
  mod_GetTimePattern  :=GetTimePattern;
  mod_SettingsChanged :=SettingsChanged;
  mod_Repeat          :=HandleRepeat;
  mod_GetDataPtr      :=GetDataPtr;
  mod_EnableChn       :=EnableChn;
  mod_IsEnabled       :=IsEnabled;
  mod_CanPlay         :=CanPlay;
  mod_Getversion      :=GetVersion;
  mod_Timer           :=AppTaskProc;
  modinfo             :=MOD_GetDataPtr;
{$ENDIF}
END;

VAR OldExit: pointer;
    OldCriticalExit: pointer;

{$IFOPT S+} {$DEFINE StackCheck} {$ENDIF} {$S-}
PROCEDURE CriticalExitProc; far;
BEGIN
  @CriticalExit:=OldCriticalExit;
  CatchBuf:=CriticalPlayerClose;
  IF CatchBuf=NIL THEN
  BEGIN
    ReleasePlayer;
    ExitProc := OldExit;
  END;
END;

PROCEDURE SafeExit; far;
BEGIN
  ExitProc := OldExit;
  ReleasePlayer;
END;

{$IFDEF StackCheck} {$S+} {$ENDIF}

BEGIN
  IF HPrevInst = 0 THEN
  BEGIN
    OldExit := ExitProc;
    ExitProc := @SafeExit;
    InstallPlayer;
    AddCriticalSeg(Seg(MODINFO^));
    OldCriticalExit:=@CriticalExit;
    CriticalExit:=CriticalExitProc;
  END;
END.