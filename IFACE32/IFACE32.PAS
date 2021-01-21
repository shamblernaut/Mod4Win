UNIT Iface32;
{ $C Fixed Preload Permanent}
INTERFACE

{$IFDEF WIN32}

USES windows,dialogs,SysUtils,Messages;

{$ELSE}

USES WinProcs,WinTypes,Strings;
TYPE SmallInt=Integer;

{$ENDIF}

TYPE  TModType=(NoiseTr,StarTr,ProTr,Ftrk,WOW,MTM,OKTALYZ,f669,STM,fFAR,ULT,S3M,UNIS,DMF,FT2);

CONST
  dev_DAC  =  0;
  dev_DS   =  256;{DirectSound}
  dev_HDD  = -2;
  dev_OPL4 = -3;
  dev_GF1  = -4;
  dev_EWS  = -5;
  wm_PutMem=WM_User+200;
  wm_GetMem=WM_User+201;
  wm_PutInf=WM_User+202;
  wm_GetInf=WM_User+203;
  wm_Seek  =WM_User+204;

TYPE PSettings=^tSettings;
     TSettings=RECORD
       Deviceid  :SmallInt;
       Name      :ARRAY[0..40] OF Char;
       samplerate:word;{11000..48000}
       stereo    :byte;{1/2}
       bits      :byte;{8/16}
       nBuffers  :byte;{2..99}
       Surround  :Boolean;
       NoBPM     :Boolean;{False}
       TMode     :Byte;{0..MMTask, 1...WM_Timer, 2..Interrrupt}
       PreAmp    :Byte;{Bit7=1...Clipping;Bit 6..0= 20+PreAmp}{ $80 }
       Oversamp  :Boolean;
       Panning   :Boolean;
       MemSize   :longint;{Returns GF1/InterWave/OPL4/SAM9407 Memory Size}
     END;

CONST
  defDACsetting:tsettings=(deviceid:-1; Name:'Wave Mapper'; samplerate:44100; stereo:2;
                           bits:16; nBuffers:24; surround:true; NoBPM:false; tmode:0;
                           PreAmp:148; OverSamp:true; Panning:true; MemSize:0);
  defHDDsetting:tsettings=(deviceid:dev_HDD; Name:'Direct To Disk Recording'; samplerate:44100; stereo:2;
                           bits:16; nBuffers:4; surround:false; NoBPM:false; tmode:0;
                           PreAmp:148; OverSamp:true; Panning:true; MemSize:0);
  defOPLsetting:tsettings=(deviceid:dev_OPL4; Name:'AudioTrix Pro (OPL4)'; samplerate:44100; stereo:2;
                           bits:16; nBuffers:0; surround:false; NoBPM:false; tmode:2;
                           PreAmp:20; OverSamp:false; Panning:true; MemSize:0);
  defGF1setting:tsettings=(deviceid:dev_GF1; Name:'Gravis Ultrasound (GF1)'; samplerate:44100; stereo:2;
                           bits:16; nBuffers:0; surround:false; NoBPM:false; tmode:2;
                           PreAmp:20; OverSamp:false; Panning:true; MemSize:0);
  defEWSsetting:tsettings=(deviceid:dev_EWS; Name:'TerraTec EWS(Dream SAM9407)'; samplerate:48000; stereo:2;
                           bits:16; nBuffers:0; surround:false; NoBPM:false; tmode:2;
                           PreAmp:20; OverSamp:false; Panning:true; MemSize:0);

{************************************************************************************************************}
{*                                                                                                          *}
{*     Direct Sound Interface                                                                               *}
{*                                                                                                          *}
{************************************************************************************************************}
TYPE tDSSettings=RECORD
        Device         :Longint;{0...DS; 1...GUS; 2...OPL4}
        SampleRate     :Longint;{11025...48000 Hz}
        BitsPerSample  :Longint;{8/16}
        nChannels      :Longint;{1/2}
        Clipping       :Longint;{0/1}
        Panning        :Longint;{1=use panning; 0=snap to left middle or right}
        Surround       :Longint;{0=Surround off;1=Surround on}
        OverSampling   :Longint;{0: Don't use IDO; 1: Use IDO}
        Mix_dB         :Longint;{-20...+20 dB digital Mixer Volume}
        SmallBufferSize:Longint;{Buffersize to be filled everytime DS_KeepAlive is called}
        nSmallBuffers  :Longint;{Buffersize to be returned everytime DS_GetBuffer is called}
        nBigBuffers    :Longint;{How many big sample buffers to be used [3..99]}
      END;

      {Pointer into the Player32's info's}
      pDS_Info=^tDS_Info;
      tDS_Info=RECORD
        CurrTime  :Longint;     {PlayTime in ms of the last by DS_GetBuffer returned buffer}
        TotalTime :Longint;     {Total PlayTime of the MOD in ms}
        CurrLine  :Longint;     {Row of the pattern of the last by DS_GetBuffer returned buffer}
        CurrPttrn :Longint;     {Pattern of the last by DS_GetBuffer returned buffer}
        Repeating :Longint;     {1 if in the last by DS_GetBuffer returned buffer the MOD is repeating}
        BufTimeLen:Longint;     {Length of the current ahead mixed buffer in ms}
        CPUUsage  :Longint;     {current CPU Load in %*1000}
        BuffLen   :Longint;     {Length in Bytes of the last by DS_GetBuffer returned buffer}
        BuffPtr   :Pointer;     {16:16 Pointer to the last by DS_GetBuffer returned buffer}
      END;

VAR  SharedMem:RECORD
       CASE Word OF
         1:(Bytes:ARRAY[0..2047]OF Byte;);
         2:(Filename:ARRAY[0..260] OF CHAR;);
         3:(SharedSettings:tSettings;);
         4:(SharedEnChn:RECORD
              Chn:Byte;
              How:Boolean;
            END;);
         5:(SharedCanPlay:RECORD
              Settings: TSettings;
              OvrRateChng: boolean
            END;);
         6:(SharedBool:Boolean;);
         7:(SharedLong:Longint;);
         8:(SharedByte:Byte;);
         9:(Longs:ARRAY[0..511]OF Longint;);
         1001:(SharedDS_Play:RECORD
                 FileName:ARRAY [0..260]OF Char;
                 Settings:tDSSettings;
                 Error:Longint;
               END;);
     END;

CONST
   {Fehler ID's}
    id_NoMem         =  1;
    id_FileNotFound  =  2;
    id_NoDevice      =  3;
    id_SyncDev       =  4;
    id_NoModFile     =  5;
    id_FileCorrupted =  6;
    id_NixFile       =  9;
    id_InvModFile    = 10;
    id_FileUnsupport = 11;
    {Wave-Errors>10000 MMSysGetErrorMessage(Error-10000)...}

FUNCTION  IsMOD(FileName:PChar):Boolean;{GetTotalTime and return true if the file is a MOD}
FUNCTION  MOD_CanPlay(VAR Settings: tSettings; OvrRateChng: WordBool): WordBool;{Can be used while playback!}
FUNCTION  MOD_Play(FileName:PChar):Word;
FUNCTION  MOD_Stopped:Boolean;
PROCEDURE MOD_Stop;
FUNCTION  MOD_ChangeSettings(VAR Settings:tSettings):Word;
PROCEDURE MOD_Pause(GoOn:Boolean);{Go On in Background?}
PROCEDURE MOD_EndPause(GoOn:Boolean);
PROCEDURE MOD_SetRepeat(Rep:boolean);
FUNCTION  MOD_CPUUsage:Longint; {CPU_Load in %*1000}
FUNCTION  MOD_CurrTime:Longint; {Time in ms; CAN EVEN BE USED WITH DS_XXX}
FUNCTION  MOD_TotalTime:Longint;{Time in ms; CAN EVEN BE USED WITH DS_XXX}

PROCEDURE MOD_BeginSeek;{causes a Pause...}
FUNCTION  MOD_EndSeek(aTime:Longint;FileName:PChar):Word;

{unwichtich...}
FUNCTION  MOD_GetTimePattern(Time:Longint):Byte;{I have the Time...what Pattern will be played?}
PROCEDURE MOD_EnableChn(Chn:Byte; How:Boolean); {Switch Chn ON/OFF}
PROCEDURE MOD_EnableAllChn(How:Boolean);        {Switch All Channels ON/OFF}
FUNCTION  MOD_IsEnabled(Chn:Byte):Boolean;      {Is Chn On?}
FUNCTION  MOD_GetVersion: word;

PROCEDURE ReleasePlayer;
PROCEDURE InstallPlayer;

{************************************************************************************************************}
{*                                                                                                          *}
{*     Direct Sound Interface                                                                               *}
{*                                                                                                          *}
{************************************************************************************************************}
PROCEDURE DS_STOP;{release all memory...}
FUNCTION  DS_Play(FileName:pChar;Settings:tDSSettings;VAR Error:Longint):pDS_Info;{Load MOD into memory}
PROCEDURE DS_Seek(Time:Longint);{Seek to Time in current loaded MOD}
FUNCTION  DS_KeepAlive:Longint; {Fill one SmallBuffer and return one if another SmallBuffer is ready to fill}
FUNCTION  DS_GetBuffer:Longint; {Get Handle for Next LargeBuffer if available or NULL}
FUNCTION  DS_DoneBuffer(What:Longint):Longint;{Mark one LargeBuffer as played...so it can be refilled}

IMPLEMENTATION

VAR hPlayerWnd     :Thandle;{Handle of the Iface16.BIN Window}
    LastPlayedName :ARRAY[0..260]OF Char; {For EndSeek}

PROCEDURE PutSharedMem(Start,Size:Word);{Copy Memory between Tasks...(WinNT really SUCKS!)}
VAR I:Word;
BEGIN
  FOR i:=Start DIV 4 TO (Size+3)DIV 4 DO SendMessage(hPlayerWnd,WM_PutMem,i,SharedMem.Longs[i]);
END;

PROCEDURE GetSharedMem(Start,Size:Word);{Copy Memory between Tasks...(WinNT really SUCKS!)}
VAR I:Word;
BEGIN
  FOR i:=Start DIV 4 TO (Size+3)DIV 4 DO SharedMem.Longs[i]:=SendMessage(hPlayerWnd,WM_GetMem,i,0);
END;

FUNCTION IsMOD(FileName:PChar):Boolean;
BEGIN
  IsMOD:=False;
  IF (FileName=NIL)OR(FileName^='') THEN Exit;
  StrCopy(SharedMem.FileName,Filename);
  PutSharedMem(0,SizeOf(SharedMem.Filename));
  StrCopy(LastPlayedName,FileName);
  IsMOD:=(SendMessage(hPlayerWnd,wm_Command,16,0)=0)AND(MOD_TotalTime<>0);
END;

FUNCTION MOD_Stopped:Boolean;
BEGIN
  MOD_Stopped:=SendMessage(hPlayerWnd,wm_Command,102,0)<>0;
END;

FUNCTION  MOD_CPUUsage:Longint;
BEGIN
  MOD_CPUUsage:=SendMessage(hPlayerWnd,wm_Command,103,0);
END;

FUNCTION MOD_CurrTime:Longint;
BEGIN
  MOD_CurrTime:=SendMessage(hPlayerWnd,wm_Command,100,0);
END;

FUNCTION MOD_TotalTime:Longint;
BEGIN
  MOD_TotalTime:=SendMessage(hPlayerWnd,wm_Command,101,0);
END;

PROCEDURE MOD_Stop;
BEGIN
  SendMessage(hPlayerWnd,wm_Command,1,0);
  LastPlayedName[0]:=#0;
END;

FUNCTION MOD_Play(FileName:PChar):Word;
VAR F1,F2:ARRAY[0..255]OF Char;
BEGIN
  MOD_Play:=id_FileNotFound;
  IF (FileName=NIL)OR(FileName^='') THEN Exit;
  StrCopy(SharedMem.FileName,Filename);
  PutSharedMem(0,SizeOf(SharedMem.Filename));
  StrCopy(LastPlayedName,FileName);
  MOD_Play:=SendMessage(hPlayerWnd,wm_Command,2,0);
END;

PROCEDURE MOD_BeginSeek;
BEGIN
  MOD_Pause(True);
END;

FUNCTION MOD_EndSeek(aTime:Longint;FileName:PChar):Word;
Var Settings:TSettings;
BEGIN
  MOD_EndSeek:=0;
  Settings:=DefDACSetting;
  IF StrIComp(LastPlayedName,FileName)<>0 THEN MOD_EndSeek:=MOD_Play(Filename);
  SendMessage(hPlayerWnd,wm_Seek,0,aTime);
END;

FUNCTION MOD_ChangeSettings(VAR settings:tsettings):Word;
BEGIN
  SharedMem.SharedSettings:=Settings;
  PutSharedMem(0,SizeOf(SharedMem.SharedSettings));
  MOD_ChangeSettings:=SendMessage(hPlayerWnd,wm_Command,6,0);
  GetSharedMem(0,SizeOf(SharedMem.SharedSettings));
  Settings:=SharedMem.SharedSettings;
END;

PROCEDURE MOD_Pause(GoOn:Boolean);
BEGIN
  WITH SharedMem DO SharedBool:=GoOn;
  PutSharedMem(0,1);
  SendMessage(hPlayerWnd,wm_Command,3,0);
END;

PROCEDURE MOD_EndPause(GoOn:Boolean);
BEGIN
  WITH SharedMem DO SharedBool:=GoOn;
  PutSharedMem(0,1);
  SendMessage(hPlayerWnd,wm_Command,4,0);
END;

PROCEDURE MOD_SetRepeat(rep:boolean);
BEGIN
  WITH SharedMem DO SharedBool:=rep;
  PutSharedMem(0,1);
  SendMessage(hPlayerWnd,wm_Command,7,0);
END;

FUNCTION  MOD_GetTimePattern(Time:Longint):Byte;
BEGIN
  WITH SharedMem DO SharedLong:=Time;
  PutSharedMem(0,4);
  MOD_GetTimePattern:=SendMessage(hPlayerWnd,wm_Command,5,0);
END;

PROCEDURE MOD_EnableChn(Chn:Byte; How:Boolean);
BEGIN
  WITH SharedMem DO
  BEGIN
    SharedEnChn.Chn:=Chn;
    SharedEnChn.How:=How;
  END;
  PutSharedMem(0,SizeOf(SharedMem.SharedEnChn));
  SendMessage(hPlayerWnd,wm_Command,9,0);
END;

PROCEDURE MOD_EnableAllChn(How:Boolean);
BEGIN
  WITH SharedMem DO SharedBool:=How;
  PutSharedMem(0,1);
  SendMessage(hPlayerWnd,wm_Command,15,0);
END;

FUNCTION  MOD_IsEnabled(Chn:Byte):Boolean;
BEGIN
  WITH SharedMem DO SharedByte:=Chn;
  PutSharedMem(0,1);
  MOD_IsEnabled:=Boolean(SendMessage(hPlayerWnd,wm_Command,10,0));
END;

FUNCTION  MOD_CanPlay(VAR Settings: tSettings; OvrRateChng: WordBool): WordBool;{Can be used while playback!}
BEGIN
  WITH SharedMem DO
  BEGIN
    SharedCanPlay.Settings:=Settings;
    SharedCanPlay.OvrRateChng:=OvrRateChng;
  END;
  PutSharedMem(0,SizeOf(SharedMem.SharedCanPlay));
  MOD_CanPlay:=Boolean(SendMessage(hPlayerWnd,wm_Command,11,0));
  GetSharedMem(0,SizeOf(SharedMem.SharedCanPlay));
  WITH SharedMem DO Settings:=SharedCanPlay.Settings;
END;

FUNCTION  MOD_GetVersion: word;
BEGIN
  MOD_GetVersion:=SendMessage(hPlayerWnd,wm_Command,14,0);
END;

{************************************************************************************************************}
{*                                                                                                          *}
{*     Direct Sound Interface                                                                               *}
{*                                                                                                          *}
{************************************************************************************************************}
PROCEDURE DS_STOP;
BEGIN
  SendMessage(hPlayerWnd,wm_Command,1000,0);
END;

FUNCTION  DS_Play(FileName:pChar;Settings:tDSSettings;VAR Error:Longint):pDS_Info;
VAR F1,F2:ARRAY[0..255]OF Char;
BEGIN
  StrCopy(SharedMem.SharedDS_Play.FileName,Filename);
  SharedMem.SharedDS_Play.Settings:=Settings;
  SharedMem.SharedDS_Play.Error:=0;
  PutSharedMem(0,SizeOf(SharedMem.SharedDS_Play));
  StrCopy(LastPlayedName,FileName);
  {$IFDEF WIN32}
  DS_Play:=Pointer(SendMessage(hPlayerWnd,wm_Command,1001,1));
  {$ELSE}
  DS_Play:=Pointer(SendMessage(hPlayerWnd,wm_Command,1001,0));
  {$ENDIF}
  GetSharedMem(0,SizeOf(SharedMem.SharedDS_Play));
  Error:=SharedMem.SharedDS_Play.Error;
END;

PROCEDURE DS_Seek(Time:Longint);
BEGIN
  SharedMem.SharedLong:=Time;
  PutSharedMem(0,4);
  SendMessage(hPlayerWnd,wm_Command,1002,0);
END;

FUNCTION  DS_KeepAlive:Longint;
BEGIN
  DS_KeepAlive:=SendMessage(hPlayerWnd,wm_Command,1003,0);
END;

FUNCTION  DS_GetBuffer:Longint;
BEGIN
  {$IFDEF WIN32}
  DS_GetBuffer:=SendMessage(hPlayerWnd,wm_Command,1004,1);
  {$ELSE}
  DS_GetBuffer:=SendMessage(hPlayerWnd,wm_Command,1004,0);
  {$ENDIF}
END;

FUNCTION  DS_DoneBuffer(What:Longint):Longint;
BEGIN
  SharedMem.SharedLong:=What;
  PutSharedMem(0,4);
  DS_DoneBuffer:=SendMessage(hPlayerWnd,wm_Command,1005,0);
END;

PROCEDURE ReleasePlayer;
BEGIN
  SendMessage(hPlayerWnd,WM_Close,0,0);
END;

PROCEDURE InstallPlayer;
VAR FirstCount:Longint;
BEGIN
  IF WinExec('iface16.bin',sw_shownormal)<32 THEN
  BEGIN
    {$IFDEF WIN32}
    MessageDlg('Iface16.bin not found!',mtError,[mbok],0)
    {$ELSE}
    MessageBox(0,'Iface16.bin not found!',NIL,0)
    {$ENDIF}
  END ELSE
  BEGIN
    hPlayerWnd:=0;
    FirstCount:=GetTickCount;
    WHILE (hPlayerWnd=0)AND(GetTickCount-FirstCount<5000) DO
    BEGIN
      Yield;
      hPlayerWnd:=FindWindow('Player32InterfaceClass','Player32Interface');
      Yield;
    END;
    IF hPlayerWnd=0 THEN
    BEGIN
      {$IFDEF WIN32}
      MessageDlg('Player32 Interface does not work properly!',mtError,[mbok],0);
      {$ELSE}
      MessageBox(0,'Player32 Interface does not work properly!',NIL,0);
      {$ENDIF}
    END;
  END;
END;

VAR OldExit: pointer;
PROCEDURE SafeExit; far;
BEGIN
  ExitProc := OldExit;
  ReleasePlayer;
END;

BEGIN
  IF HPrevInst = 0 THEN
  BEGIN
    OldExit := ExitProc;
    ExitProc := @SafeExit;
    InstallPlayer;
  END;
END.
