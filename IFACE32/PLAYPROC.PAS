UNIT Playproc;
{ $C Fixed Preload Permanent}
INTERFACE

USES winprocs,wintypes,win31;

{*******************************************PLAYER.DLL***************************************************}
CONST DLL='Player32.DLL';

TYPE  TModType=(NoiseTr,StarTr,ProTr,Ftrk,WOW,MTM,OKTALYZ,f669,STM,fFAR,ULT,S3M,UNIS,DMF,FT2);
TYPE
     TInstrument=RECORD
                   Name      :STRING[41];
                   Offset    :Longint;
                   IEnd      :Longint;
                   LoopStart :Longint;
                   Loopend   :Longint;
                   Volume    :Byte;
                   FineTune  :Byte;
                   Bits      :Byte; {Bit 5..Stereo; Bit6..Gepackt; Bit7..S3M-Adlib-Instrument;}
                   Tuning    :Word;
                 END;

CONST
   LastEffect  = $44;
TYPE
   EffectSet   = SET OF 0..LastEffect;
   EffectState = ARRAY[1..32]OF EffectSet;
   tByteState  = ARRAY[1..32]OF byte;

   pCommentBuf = ^tCommentBuf;
   tCommentBuf = ARRAY[0..65519]OF Char;

TYPE PModulInfo=^TModulInfo;
     TModulInfo=RECORD
       FileType      :TModType;              {FileFormat}
       Dummy         :Byte;
       FileSiz       :WORD;                  {Dateigröße in kByte}
       ModName       :STRING;
       NumTracks     :Word;                  {Anzahl der Kanäle pro Notenzeile}
       DiffTracks    :Word;                  {Anzahl der voneinander verschiedenen gespielten Tracks}
       NumInstruments:Word;
       Instruments   :ARRAY[0..255]OF tInstrument;
       DummyPatterns :Word;                  {Anzahl der gespeicherten aber nicht gespielten Patterns}
       ExtraBytes    :Longint;               {Überlänge des Files}
       MissingBytes  :Longint;               {fehlende Bytezahl}
       EffectsUsedSet:EffectSet;
       PanningOff    :WordBool;              {Panning wird nicht benutzt}
       SongMem       :Longint;               {Speichergröße für Pattern und Samples}
       PlayMemTop    :Longint;               {Speichergröße für PlayBuffers}
       LoopFrom      :Integer;               {Pattern, von dem aus geloopt wird}
       LoopTo        :Integer;               {Pattern, zu welchem geloopt wird}
       Playing       :WordBool;              {Modul spielt}
       CPUUsage      :Longint;               {aktuelle CPU-Auslastung}
       TotalTime     :Longint;               {GesamtSpielzeit [ms]}
       CurrTime      :Longint;               {Aktuell zu hörende Zeit [ms]}
       BufTimeLen    :Longint;               {SamplePufferLänge [ms]}
       CurrPttrn     :Word;                  {Aktuell zu hörendes Pattern}
       NumPatterns   :Word;                  {Größe des Patternarrangements}
       DiffPatterns  :Word;                  {Anzahl der voneinander verschiedenen gespielten Pattern}
       CurrLine      :word;
       CurrEffects   :EffectState;           {zu hörende Effekte}
       CurrInstr     :tByteState;
       CurrVols      :tByteState;
       CurrNotes     :tByteState;
       CurrSpd       :Byte;
       CurrBPM       :Byte;
       Commentlen    :Word;
       Comment       :pCommentBuf;
       hTask         :Word;
       Dummy2        :Longint;
       Dummy3        :Longint;
     END;

CONST
  dev_DAC  =  0;
  dev_DS   =  256;{DirectSound}
  dev_HDD  = -2;
  dev_OPL4 = -3;
  dev_GF1  = -4;
  dev_EWS  = -5;

TYPE PSettings=^tSettings;
     TSettings=RECORD
       Deviceid  :integer;
       Name      :STRING[40];
       samplerate:word;
       stereo    :byte;
       bits      :byte;
       nBuffers  :byte;
       Surround  :Boolean;
       NoBPM     :Boolean;
       TMode     :Byte;
       PreAmp    :Byte;
       Oversamp  :Boolean;
       Panning   :Boolean;
       MemSize   :longint;
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

CONST modinfo:pModulInfo=NIL;

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

TYPE Tmod_Stop=PROCEDURE;
     tCriticalPlayerClose =FUNCTION :Pointer;
     Tmod_Play            =FUNCTION (hPWindow:hWnd; Filename, SaveName: pChar; FNamesOEM: Bool;
                                    Settings:pSettings; PauseIt, _RepeatSong:Boolean): Word;
     Tmod_SettingsChanged =PROCEDURE(VAR settings:tsettings; VAR Error:word);
     Tmod_Repeat          =PROCEDURE(rep:boolean);
     Tmod_GetDataPtr      =FUNCTION :POINTER;
     Tmod_GetTimePattern  =FUNCTION (Time:Longint):Byte;
     Tmod_EnableChn       =PROCEDURE(Chn:Byte; How:Boolean);
     Tmod_EnableAllChn    =PROCEDURE(How:Boolean);
     Tmod_IsEnabled       =FUNCTION (Chn:Byte):Boolean;
     Tmod_CanPlay         =FUNCTION (VAR Settings: TSettings; OvrRateChng: boolean): boolean;
     Tmod_GetVersion      =FUNCTION : word;
     tDS_Play             =FUNCTION (FileName:pChar;Settings:tDSSettings;VAR Error:Longint):pDS_Info;
     tDS_Seek             =PROCEDURE(Time:Longint);
     tDS_KeepAlive        =FUNCTION :Longint;
     tDS_DoneBuffer       =FUNCTION (What:Longint):Longint;

VAR  mod_Stop,
     mod_Timer,
     mod_SampleForeGnd,
     mod_SampleBackGnd:   Tmod_Stop;
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
     DS_STOP:             tMOD_Stop;
     DS_Play:             tDS_Play;
     DS_Seek:             tDS_Seek;
     DS_KeepAlive,
     DS_GetBuffer:        tDS_KeepAlive;
     DS_DoneBuffer:       tDS_DoneBuffer;

PROCEDURE ReleasePlayer;
PROCEDURE InstallPlayer;

IMPLEMENTATION

VAR s:STRING;
    hplayer:Thandle;
    olderrmode:word;


PROCEDURE ReleasePlayer;
BEGIN
  FreeLibrary(hplayer);
END;

PROCEDURE InstallPlayer;
BEGIN
  s:=DLL+#0;
  olderrmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  hplayer:=loadlibrary(@s[1]);
  SetErrorMode(olderrmode);
  IF GetModuleUsage(hplayer)>1 THEN
  BEGIN
    FreeLibrary(hplayer);
    MessageBox(0,DLL+' already in use!','Fatal Error',mb_ok OR mb_iconstop);
    Halt(0);
  END;
  {IST HANDLE VALID!}
  IF hplayer>32 THEN
  BEGIN
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
    @mod_SampleForeGnd   :=GetProcAddress(hplayer,makeintresource(12));
    @mod_SampleBackGnd   :=GetProcAddress(hplayer,makeintresource(13));
    @mod_GetVersion      :=GetProcAddress(hplayer,makeintresource(14));
    @mod_EnableAllChn    :=GetProcAddress(hplayer,makeintresource(15));
    @CriticalPlayerClose :=GetProcAddress(hplayer,makeintresource(16));
    @mod_Timer           :=GetProcAddress(hplayer,makeintresource(99));
    @DS_STOP             :=GetProcAddress(hplayer,makeintresource(1000));
    @DS_Play             :=GetProcAddress(hplayer,makeintresource(1001));
    @DS_Seek             :=GetProcAddress(hplayer,makeintresource(1002));
    @DS_KeepAlive        :=GetProcAddress(hplayer,makeintresource(1003));
    @DS_GetBuffer        :=GetProcAddress(hplayer,makeintresource(1004));
    @DS_DoneBuffer       :=GetProcAddress(hplayer,makeintresource(1005));
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
     OR(@mod_SampleForeGnd=NIL)
     OR(@mod_SampleBackGnd=NIL)
     OR(@mod_GetVersion=NIL)
     OR(@mod_Timer=NIL)
     OR(@CriticalPlayerClose=NIL)
     OR(@DS_STOP=NIL)
     OR(@DS_Play=NIL)
     OR(@DS_Seek=NIL)
     OR(@DS_KeepAlive=NIL)
     OR(@DS_GetBuffer=NIL)
     OR(@DS_DoneBuffer=NIL)
     THEN
     BEGIN
       MessageBox(0,'Could not load '+DLL+'.','Fatal Error',mb_ok OR mb_iconstop);
       Halt(0);
     END;
     modinfo             :=MOD_GetDataPtr;
  END ELSE
  BEGIN
    MessageBox(0,DLL+' not found or corrupt.','Fatal Error',mb_ok OR mb_iconstop);
    Halt(0);
  END;
END;

VAR OldExit: pointer;

{$IFOPT S+} {$DEFINE StackCheck} {$ENDIF} {$S-}
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
  END;
END.