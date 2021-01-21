{$N+,E-}
{$DEFINE HUGEMEM}
{$DEFINE PrtLayer}
{ $DEFINE CHECK}
{ $DEFINE DEMO}
{ $DEFINE MCI}
{ $DEFINE MCI_Debug}
{$DEFINE CompressTracks}
{ $DEFINE PROFILE_TIME}

{$IFNDEF MCI}
{$DEFINE WAVE}
LIBRARY Player32;
{$D Module Player Library used by MOD4WIN - (c) 1993-98 by Kay Bruns and JSInc. }
{$M 16384,4600}
{$R PLAYER.RES}
{$ELSE}
LIBRARY M4W_MCI;
{$M 16384,4600}
{$IFDEF RJM4W}
{$D M4W:[MCI] RJs MOD4WIN MCI Driver}
{$R RJMOD4WN.RES}
{$ELSE RJM4W}
{$D M4W:[MCI] MOD4WIN MCI Driver}
{$R M4W_MCI.RES}
{$ENDIF RJM4W}
{$ENDIF}

{$C Fixed Preload Permanent}
{$S 60000}
USES
  {$IFDEF MCI}   Ctl3DV2, {$ENDIF}
  {$IFDEF Check} Check,   {$ENDIF}
  {$IFDEF WAVE}  Wave,    {$ENDIF WAVE}
  FILE32, W32DEFS,
  VALCONV, Version, WaitBox, Maths, GetOS,
  {$IFNDEF MCI}   MemList, {$ENDIF}
  DacMix, ModC, PlayDefs, Loaders, Task,
  {$IFDEF PrtLayer} PrtLayer,{$ENDIF}
  {$IFDEF HUGEMEM} HugeMem, {$ENDIF} Crc,
  WinTypes, WinProcs, WinDOS, Win31, MMSystem, Strings;

PROCEDURE GetAllDOSMem;
VAR Current:pPointer;
BEGIN
  IF (First<>NIL)OR(OS<>WINDOWS_31) THEN Exit;
  First:=Ptr(LoWord(GlobalDosAlloc(4096)),0);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current^:=Ptr(LoWord(GlobalDosAlloc(4096)),0); Current:=Current^; END;
END;

PROCEDURE FreeAllDOSMem;
VAR Current:pPointer;
BEGIN
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current:=Current^; GlobalDosFree(HiWord(FirstSeg)); First:=Current; END;
  First:=NIL;
END;

{$IFDEF PROFILE_TIME}
VAR DebugTimes:ARRAY[0..15]OF Longint;
PROCEDURE StartDebugTimer(TimerID:Byte);
BEGIN
  IF NOT (TimerID IN [0..15])THEN Exit;
  DebugTimes[TimerID]:=TimeGetTime;
END;

PROCEDURE OutputDebugTime(TimerID:Byte;Message:pChar);
VAR PC:ARRAY[0..20]OF Char;
    L:Longint;
BEGIN
  IF NOT (TimerID IN [0..15])THEN Exit;
  L:=TimeGetTime-DebugTimes[TimerID];
  OutputDebugString(Message);
  Str(L:10,PC);
  OutputDebugString(PC);
  OutputDebugString(#13#10);
END;
{$ENDIF PROFILE_TIME}

{$I PLAY.INC}

PROCEDURE AssignPeakFile;
VAR S:STRING;
BEGIN
  Byte(S[0]):=GetSystemDirectory(@S[1],255);
  IF S[Length(S)]<>'\' THEN S:=S+'\';
  S:=S+'MOD4WIN.PKS';
  Assign(PeakFile,S);
  {$IFNDEF MCI}
  S:=ParamStr(0);
  WHILE (Length(S)>0)AND(S[Length(S)]<>'\') DO Dec(Byte(S[0]));
  S:=S+'MOD4WIN.PKS';
  Assign(LocalPeakFile,S);
  {$ENDIF MCI}
END;

TYPE tPeakStruct=RECORD
       fSize:Longint;
       Check:Word;
       PreAmp:Word;
     END;
     pPeakArray=^tPeakArray;
     tPeakArray=ARRAY[1..2048]OF tPeakStruct;

PROCEDURE GetPeakStruct(aFile:pChar;FNamesOEM:Bool;VAR PeakStruct:tPeakStruct);
VAR FHandle   : Handle32;
    FIs32     : WordBool;
BEGIN
  FHandle:=_lOpen32(aFile, FNamesOEM, GENERIC_READ, FILE_SHARE_READ OR FILE_SHARE_WRITE, @FIs32);
  PeakStruct.FSize:=_lSize32(FHandle, FIs32);
  _lClose32(FHandle, FIs32);
  PeakStruct.Check:=CalcBuf_CRC16(CCT_PolyCRC16,Seg(Mi.ModName),Ofs(Mi.ModName)+1,Length(Mi.ModName));
END;

FUNCTION FindPeakStruct(VAR aFile:FILE;VAR PeakStruct:tPeakStruct):Boolean;
VAR PeakArray:pPeakArray;
    i,NumRead:Word;
LABEL Success;
BEGIN
  FindPeakStruct:=False;
  FileMode:=0;
  InOutRes:=0;
  Reset(aFile,SizeOf(tPeakStruct));
  IF IOResult<>0 THEN Exit;
  PeakArray:=GlobalAllocPtr(gptr,SizeOf(tPeakArray));
  WHILE NOT EOF(aFile)DO
  BEGIN
    BlockRead(aFile,PeakArray^,2048,NumRead);
    IF NumRead<>0 THEN FOR I:=1 TO NumRead DO
    IF (PeakArray^[i].fSize=PeakStruct.fSize) AND(PeakArray^[i].Check=PeakStruct.Check)THEN
    BEGIN
      PeakStruct.PreAmp:=PeakArray^[i].PreAmp;
      FindPeakStruct:=True;
      GOTO Success;
    END;
  END;
Success:
  GlobalFreePtr(PeakArray);
  Close(aFile);
END;

FUNCTION StorePeakStruct(VAR aFile:FILE;VAR PeakStruct:tPeakStruct;OverwriteOld:Boolean):Boolean;
VAR PA:pPeakArray;
    i,NumRead:Word;
LABEL Success;
BEGIN
  FileMode:=2;
  InOutRes:=0;
  Reset(aFile,SizeOf(tPeakStruct));
  IF IOResult<>0 THEN ReWrite(aFile,SizeOf(tPeakStruct));
  IF IOResult<>0 THEN Exit;
  PA:=GlobalAllocPtr(gptr,SizeOf(tPeakArray));
  WHILE NOT EOF(aFile)DO
  BEGIN
    BlockRead(aFile,PA^,2048,NumRead);
    IF NumRead<>0 THEN FOR I:=1 TO NumRead DO
    IF (PA^[i].fSize=PeakStruct.fSize) AND(PA^[i].Check=PeakStruct.Check)THEN
    BEGIN
      IF OverwriteOld THEN
      BEGIN
        PA^[i]:=PeakStruct;
        Seek(aFile,FilePos(aFile)-NumRead+I-1);
        BlockWrite(aFile,PA^[i],1);
      END;
      GOTO Success;
    END;
  END;
  Seek(aFile,FileSize(aFile));
  BlockWrite(aFile,PeakStruct,1);
Success:
  GlobalFreePtr(PA);
  Close(aFile);
END;

PROCEDURE CopyFile(VAR Src,Dst:FILE);
VAR BUF:Pointer;
    NumRead:Word;
BEGIN
  InOutRes:=0;
  FileMode:=0;
  Reset(Src,1);
  IF IOResult<>0 THEN Exit;
  SetFAttr(Dst,FaArchive);
  IF IOResult<>0 THEN Exit;
  FileMode:=1;
  ReWrite(Dst,1);
  IF IOResult<>0 THEN Exit;
  BUF:=GlobalAllocPtr(gptr,16384);
  WHILE NOT Eof(Src)DO
  BEGIN
    BlockRead(Src,Buf^,16384,NumRead);
    BlockWrite(Dst,Buf^,NumRead);
  END;
  Close(Src);
  Close(Dst);
  GlobalFreePtr(BUF);
END;

FUNCTION SortProc(Item12:Pointer;dwUser:LongInt):Integer;FAR;
TYPE tItems=ARRAY[1..2]OF COMP;
     pItems=^tItems;
VAR ITEM:Comp;
BEGIN
  ITEM:=pITEMS(Item12)^[1]-pITEMS(Item12)^[2];
  IF (ITEM=0)THEN SortProc:= 0 ELSE
  IF (ITEM>0)THEN SortProc:= 1 ELSE SortProc:= -1;
END;

PROCEDURE MergePeakFiles;
{$IFNDEF MCI}
VAR PeakArray:pPeakArray;
    i,NumRead:Word;
    MemList:pMemList;
{$ENDIF}
BEGIN
  {$IFNDEF MCI}
  InOutRes:=0;
  FileMode:=0;
  Reset(LocalPeakFile,SizeOf(tPeakStruct));
  IF IOResult<>0 THEN Exit;
  MemList:=NEW(pMemList,Init(SizeOf(tPeakStruct)));
  PeakArray:=GlobalAllocPtr(gptr,SizeOf(tPeakArray));
  IF FileSize(LocalPeakFile)>0 THEN
  WHILE NOT EOF(LocalPeakFile)DO
  BEGIN
    BlockRead(LocalPeakFile,PeakArray^,2048,NumRead);
    IF NumRead<>0 THEN FOR I:=1 TO NumRead DO MemList^.AddListItem(SortProc,PeakArray^[i],0);
  END;
  Close(LocalPeakFile);
  InOutRes:=0;
  Reset(PeakFile,SizeOf(tPeakStruct));
  IF IOResult=0 THEN
  BEGIN
    IF FileSize(PeakFile)>0 THEN
    WHILE NOT EOF(PeakFile)DO
    BEGIN
      BlockRead(PeakFile,PeakArray^,2048,NumRead);
      IF NumRead<>0 THEN FOR I:=1 TO NumRead DO MemList^.AddListItem(SortProc,PeakArray^[i],0);
    END;
    Close(PeakFile);
  END;
  IF MemList^.GetCount>1 THEN WITH MemList^ DO
  BEGIN
    i:=0;
    InOutRes:=0;
    FileMode:=1;
    ReWrite(PeakFile,SizeOf(tPeakStruct));
    IF IOResult=0 THEN
    BEGIN
      FOR i:=0 TO GetCount-2 DO
      BEGIN
        {GetListItem(i,PeakArray^[1]);
        GetListItem(i+1,PeakArray^[2]);{ }
        COPYITEM(GetMemSel,Longint(i)*Longint(ItemLen),HiWord(Longint(PeakArray)),Word(PeakArray),2*ItemLen);{ }
        IF SortProc(PeakArray,0)<>0 THEN BlockWrite(PeakFile,PeakArray^,1);
      END;
      BlockWrite(PeakFile,PeakArray^[2],1);
      Close(PeakFile);
      CopyFile(PeakFile,LocalPeakFile);
    END;
  END;
  GlobalFreePtr(PeakArray);
  Dispose(MemList,Done);
  {$ENDIF MCI}
END;

FUNCTION FindPeak(aFile:pChar;FNamesOEM:Bool;VAR PreAmp:Byte):Boolean;
VAR PeakStruct:tPeakStruct;
BEGIN
  FindPeak:=False;
  GetPeakStruct(aFile,FNamesOEM,PeakStruct);
  IF FindPeakStruct(PeakFile,PeakStruct)THEN
  BEGIN
    PreAmp:=(PreAmp AND $80)OR(PeakStruct.PreAmp AND $7F);
    FindPeak:=True;
  END;
END;

PROCEDURE StorePeak(aFile:pChar;FNamesOEM:Bool;PreAmp:Byte);
VAR PS:tPeakStruct;
BEGIN
  GetPeakStruct(aFile,FNamesOEM,PS);
  PS.PreAmp:=PreAmp;
  StorePeakStruct(PeakFile,PS,True);
  {$IFNDEF MCI}
  CopyFile(PeakFile,LocalPeakFile);
  {$ENDIF MCI}
END;

CONST Stopping:BOOLEAN=FALSE;
PROCEDURE stop;EXPORT;
BEGIN
  IF Stopping THEN Exit;
  Stopping:=True;
  FreeSavedChannels;
  DeActivateSound;
  MI.Playing    :=false;
  Pause         :=false;
  MI.CurrBPM    :=0;
  MI.CurrSPD    :=0;
  IF NOT PeakHold THEN MI.CPUUsage   :=0;
  {$IFNDEF MCI}
  MI.CurrTime   :=0;
  {$ENDIF}
  MI.BufTimeLen :=0;
  MI.CurrPttrn  :=0;
  MI.CurrLine   :=0;
  FirstTrack    :=0;
  FillChar(MI.CurrEffects,SizeOf(Effectstate),0);
  FillChar(MI.CurrInstr,SizeOf(tByteState),0);
  FillChar(MI.CurrVols,SizeOf(tByteState),0);
  FillChar(MI.CurrNotes,SizeOf(tByteState),0);
  {$IFDEF WAVE}
  IF RecordFile<>INVALID_HANDLE_VALUE THEN
  WITH SoundSettings DO CloseWaveFile(RecordFile, RecFile32Bit, Samplerate, Bits, Stereo, NIL);
  {$ENDIF}
  RecordFile:=INVALID_HANDLE_VALUE;
  FreeSongMem;
  FreePlayMem;
  IF FT2Instruments<>NIL THEN
  BEGIN
    GlobalPageUnlock(SelectorOf(FT2Instruments));
    GlobalFreePtr(FT2Instruments);
  END;
  IF FT2Samples<>NIL THEN
  BEGIN
    GlobalPageUnlock(SelectorOf(FT2Samples));
    GlobalFreePtr(FT2Samples);
  END;
  FT2Instruments:=NIL;
  FT2Samples:=NIL;
  UseLinearTune:=False;
  Stopping:=False;
  PeakHold:=GetProfileInt('Player32','PeakHold',0)<>0;
  PeakSearch:=GetProfileInt('Player32','PeakSearch',1)<>0;
  {$IFDEF MCI}
  ShowPeakSearch:=GetProfileInt('Player32','ShowPeakSearch',0)<>0;
  {$ELSE MCI}
  ShowPeakSearch:=PeakSearch;
  {$ENDIF MCI}
  {$IFDEF HUGEMEM}
  CleanUpHugeMem;
  {$ENDIF}
END;

{################################# SpielZeit berechnen#########################################}
PROCEDURE GetTotalTime;
LABEL JMP, S3M_Inc,NewPattern;
BEGIN
  {$IFDEF PROFILE_TIME} StartDebugTimer(2); {$ENDIF PROFILE_TIME}
  {Tatsächlich höchstes, erreichbares Pattern ermitteln}
  FillChar(RowPlayed,SizeOf(RowPlayed),0);
  FillChar(PatternTimes,SizeOf(PatternTimes),$FF);
  FOR k:=1 TO Mi.NumTracks DO Channels[k].PatternLoops:=0;
  i:=0;
  ForceBPM:=True;
  DoSetSpeed(InitBpmSpeed);
  ForceBPM:=False;
  DoSetSpeed(InitSpeed);
  IF MI.FileType=fFar THEN DoFineFarTempo(0);
  FarVibDepth := 4;
  PlayTime:=0;
  if not (MI.FileType in [f669, UNIS, fFAR, FT2]) then
  BEGIN
    MI.LoopTo:=-1;
    MI.LoopFrom:=-1;
  END;
  LoopFromTime:=-1;
  PatternDelay:=0;
  breakto:=0;
  GlobVol:=InitGlobVol;
  REPEAT
    if MI.FileType = S3M THEN case PArrangement[i] of
      $FE: goto S3M_Inc;
      $FF: break;
    end;
    GetPattern(i,256);
    PatternTimes[i]:=PlayTime;
    FOR k:=1 TO Mi.NumTracks DO Channels[k].PatternLoopst:=0;
    IF PtrnTempi[i]>0 THEN DoSetSpeed(PtrnTempi[i]);
    j:=breakto;
    REPEAT
      breakto:=0;
NewPattern:
      FOR k:=1 TO MI.NumTracks DO WITH Pattern^[k][j] DO
      BEGIN
        CASE effekt OF
         NoEffekt       :;
         SetGlobVol     :GlobVol:=Operands;
         SetSpeed       :DoSetSpeed(Operands);
         SetBPMSpeed    :IF Operands >= 32 THEN BEGIN ForceBPM:=True; DoSetSpeed(Operands); ForceBPM:=False; END;
         FineTempoUp    :DoFineFarTempo (Operands);
         FineTempoDwn   :DoFineFarTempo (-Operands);
         PattrnDelay    :PatternDelay:=Operands;
        END;
      END;
      ASM
        XOR ax,ax
        XOR cx,cx
        mov al,PatternDelay
        inc ax
        mul Speed
        mov cx,ax
      @ls:
        call inctime
        loop @ls
        mov patterndelay,0
      END;
      Include (RowPlayed[i], j);
      IF PattrnLoop IN Mi.EffectsUsedSet THEN
      FOR k:=MI.NumTracks DOWNTO 1 DO WITH Pattern^[k][j],Channels[k] DO
      IF Effekt=PattrnLoop THEN
      BEGIN
        IF Operands=0 THEN PatternLoopst:=j  ELSE
        BEGIN
          IF PatternLoops=0 THEN IF PatternLoopst<=j THEN PatternLoops:=Operands+1 ELSE ELSE
          IF PatternLoops=1 THEN BEGIN PatternLoops:=0; PatternLoopst:=j+1; END;
          IF PatternLoops>1 THEN
          BEGIN
            GetPattern(i,j);
            j:=patternloopst;
            Dec(patternloops);
            GOTO NewPattern;
          END;
        END;
      END;{ }
      pjump:=false;
      pbreak:=false;
      FOR k:=1 TO MI.NumTracks DO WITH Pattern^[k][j] DO
      CASE effekt OF
        PositionJump   :IF (Operands<MI.NumPatterns) THEN
                        BEGIN
                          pjump:=true;
                          jumpto:=Operands;
                        END;
        PatternBreak   :BEGIN
                          pbreak:=true;
                          IF UsePattBrkOps THEN breakto:=10*(Operands SHR 4)+(Operands AND $0F);
                        END;
      END;
      IF pjump THEN
      BEGIN
        IF PtrnBreaks[jumpto]<breakto THEN breakto:=0;
        IF (NOT OneChannelLoops)
        AND(breakto IN RowPlayed[jumpto])
        AND(MI.LoopFrom=-1) THEN
        BEGIN
          MI.LoopFrom:=i+1;
          MI.LoopTo:=jumpto+1;
          LoopFromTime:=PlayTime;
        END;
        i:=jumpto;
        GOTO JMP
      END ELSE IF pbreak THEN GOTO S3M_Inc;
      Inc(J);
    UNTIL J>PtrnBreaks[i];
  S3M_Inc:
    inc(i);
    IF i>=MI.numpatterns THEN i:=0;
  JMP:
    IF PtrnBreaks[i]<breakto THEN breakto:=0;
  UNTIL (NOT OneChannelLoops)AND(breakto IN RowPlayed[i]);
  MI.TotalTime:=PlayTime;
  IF LoopFromTime=-1 THEN LoopFromTime:=MI.TotalTime ELSE MI.TotalTime:=LoopFromTime;
  IF MI.LoopTo>-1 THEN LoopToTime:=PatternTimes[MI.LoopTo-1] ELSE LoopToTime:=0;
  FOR i:=1 TO MI.NumTracks DO IF(DefPanning[i]<>0)AND(DefPanning[i]<>255)THEN MI.PanningOff:=False;
  IF (InitGlobVol=0)AND(GlobVol=0) THEN InitGlobVol:=64;
  {$IFDEF PROFILE_TIME} OutputDebugTime(2,'PLAYER32: GetTotalTime '); {$ENDIF PROFILE_TIME}
END;

FUNCTION Play(hPWindow:hWnd; Filename, SaveName: pChar; FNamesOEM: Bool; Settings:pSettings; PauseIt,
              _RepeatSong:Boolean): Word; EXPORT;
VAR  OldCursor : hCursor;
     OldMode   : Word;
     FHandle   : Handle32;
     FIs32     : WordBool;
     _Error    : Word;
LABEL OUTCLOSE;

BEGIN
  {$IFDEF PROFILE_TIME} StartDebugTimer(1); {$ENDIF PROFILE_TIME}
  IF (FileName=NIL)OR(FileName[0]=#0)THEN BEGIN Play:=ID_NoModFile; EXIT; END;
  IF Settings<>NIL THEN
  IF (hpWindow>hwnd (-1))
  AND(Settings^.DeviceID = dev_HDD)
  AND((SaveName=NIL)OR(SaveName[0]=#0))THEN
  BEGIN
    Play:=id_NoWriteFile;
    EXIT;
  END;
  LastFilePos :=  0;
  IF (GetActiveWindow=hWindow)AND(NOT IsIconic(hWindow)) THEN OldCursor := SetCursor(LoadCursor(0,IDC_WAIT));
  OldMode     := SetErrorMode(SEM_NoOpenFileErrorBox OR SEM_FailCriticalErrors);
  STOP;
  Play        :=  0;
  _Error      :=  0;
  InitGlobVol := 64;
  FastSlides  := False;
  IF MI.Comment<>NIL THEN
  BEGIN
    GlobalFreePtr(MI.Comment);
    MI.Comment:=NIL;
  END;
  FillChar(MI.FileType,SizeOf(tModulInfo),0);
  Mi.PanningOff:=True;
  Scontrol:=False;
  {************************MODFILE IN SPEICHER EINLESEN**************************************************}
  FHandle     := _lOpen32(FileName, FNamesOEM, GENERIC_READ, FILE_SHARE_READ OR FILE_SHARE_WRITE, @FIs32);
  IF DosError=0 THEN
  BEGIN
    GetAllDOSMem;
    _Error:=GetInfo(FHandle, FIs32, FileName,False);
    IF _Error<>0 THEN
    BEGIN
      _Error := 0;
      _Error := GetInfo(FHandle, FIs32, FileName, True);
    END;
    IF _Error<>0 THEN GOTO OutClose;
    { get pattern data and calculate total time only }
    IF hpWindow = hwnd (-1) THEN
    begin
      IF NOT(GetSongMem(65536))THEN BEGIN _Error:=id_nomem; GOTO OutClose; end;
      LoadSamples := false;
      IF Mi.FileType=FT2 THEN DefChVol:=$00 ELSE DefChVol:=$FF;
      CASE MI.FileType OF
        NoiseTr,ProTr,FTrk,StarTr,
        WOW      :MOD_GetFileData (_Error);
        MTM      :MTM_GetFileData (_Error);
        f669,UNIS:f669_GetFileData(_Error);
        stm      :stm_GetFileData (_Error);
        OKTALYZ  :okt_GetFileData (_Error);
        fFAR     :Far_GetFileData (_Error);
        S3M      :S3M_GetFileData (_Error);
        FT2      :FT2_GetFileData (_Error);
        ELSE BEGIN Stop; _Error:=Id_InvModFile; GOTO OutClose; END;
      END;
      if _Error = 0 THEN GetTotalTime;
      Stop;  { release SongMem }
      goto OutClose;
    end;
    { now load the patterns again together with the instruments }
    IF (hpWindow>0)AND(Settings<>NIL) THEN
    BEGIN
      IF PeakSearch THEN PeakSearch:=NOT FindPeak(FileName,FNamesOEM,Settings^.PreAmp);
      LoadSamples := true;
      HWindow:=hPWindow;
      soundsettings:=settings^;
      IF NOT GetSongMem(65536)
      THEN BEGIN _Error:=id_nomem; GOTO OutClose; END ELSE;
      IF NOT GetPlayMem(Longint(MI.FileSiz)*Longint(1024))
      THEN BEGIN _Error:=id_nomem; GOTO OutClose; END ELSE;
      IF Mi.FileType=FT2 THEN DefChVol:=$00 ELSE DefChVol:=$FF;
      { progress box }
      if LoadProgress and (SoundSettings.DeviceID < dev_HDD) then
      begin
        InitWaitBox (hWindow, 'Loading'#0);
        SetPercent (0);
      end;
      { end progress box }
      CASE MI.FileType OF
        NoiseTr,StarTr,ProTr,FTrk,
        WOW      :MOD_GetFileData (_Error);
        MTM      :MTM_GetFileData (_Error);
        f669,UNIS:f669_GetFileData(_Error);
        stm      :stm_GetFileData (_Error);
        OKTALYZ  :okt_GetFileData (_Error);
        fFAR     :Far_GetFileData (_Error);
        S3M      :S3M_GetFileData (_Error);
        FT2      :FT2_GetFileData (_Error);
        ELSE BEGIN Stop; _Error:=Id_InvModFile; GOTO OutClose; END;
      END;
      if LoadProgress and (SoundSettings.DeviceID < dev_HDD) THEN DoneWaitBox;
      IF (_Error<>0) THEN
      BEGIN
        Stop;
        GOTO OutClose;
      END;
      IF NOT (FitSongMem(SongMemOfs) AND FitPlayMem(PlayMemOfs)) THEN
      BEGIN
        Stop;
        _Error:=ID_NoMem;
        GOTO OutClose;
      END; { }
      GetTotalTime;
      IF SoundSettings.DeviceID=DEV_HDD THEN
      BEGIN
        PeakSearch:=True;
        ShowPeakSearch:=True;
        SoundSettings.OverSamp:=3;
      END;
      {********************* Start PeakSearch *********************}
      IF PeakSearch THEN
      BEGIN
        Dis_BAK:=Disabled;
        FillChar(DisAbled,SizeOf(DisAbled),0);
        Settings^:=SoundSettings;
        IF SoundSettings.DeviceID<>DEV_HDD THEN
        BEGIN
          IF GetProfileInt('Player32','ExactPeakSearch',1)<>0 THEN
          BEGIN
            SoundSettings.SampleRate:=4000;
            SoundSettings.PreAmp:=$80+40;
          END ELSE
          BEGIN
            SoundSettings.SampleRate:=1000;
            SoundSettings.PreAmp:=$80+20;
          END;
          SoundSettings.Oversamp:=0;
          SoundSettings.Surround:=false;
          SoundSettings.stereo:=2;
        END ELSE SoundSettings.PreAmp:=$80+40;
        SoundSettings.Bits:=16;
        SoundSettings.Panning:=True;
        SoundSettings.nBuffers:=3;
        SoundSettings.DeviceID:=255;
        SoundSettings.TMode:=MMTask;
        activatesound(_Error);
        KanalInit;
        TimerOn;
        RepeatSong:=False;
        Search:=true;
        SearchTime:=Mi.TotalTime;
        IF ShowPeakSearch THEN InitWaitBox(hWindow, 'Peak Search'#0);
        MaxLevel:=0;
        play_the_module; {Spielen im SEARCH-Modus}
        Disabled:=Dis_BAK;
        IF ShowPeakSearch THEN DoneWaitBox;
        IF SoundSettings.SampleRate>1000
        THEN Settings^.PreAmp:=Settings^.PreAmp AND $80 OR SoundSettings.PreAmp AND $7F;
        DeactivateSound;
        PeakSearch:=False;
        Search:=false;
        IF SoundSettings.SampleRate=1000 THEN
        BEGIN
          SoundSettings:=Settings^;
          MaxLevel:=Trunc(20.0*Lg(32767.0/MaxLevel))+20;
          IF MaxLevel<0 THEN MaxLevel:=0 ELSE IF MaxLevel>40 THEN MaxLevel:=40;
          SoundSettings.PreAmp:=(Settings^.PreAmp AND $80)OR MaxLevel;
          Settings^.PreAmp:=SoundSettings.PreAmp;
          StorePeak(FileName,FNamesOEM,SoundSettings.PreAmp);
        END ELSE
        IF SoundSettings.SampleRate=4000 THEN
        BEGIN
          SoundSettings:=Settings^;
          StorePeak(FileName,FNamesOEM,SoundSettings.PreAmp);
        END ELSE SoundSettings:=Settings^;
        IF SoundSettings.DeviceID<DEV_HDD THEN FitPlayMem(0);
        MaxLevel:=0;
      END;
      {********************* END PeakSearch *********************}
      repeatsong:=_RepeatSong;
      pause:=PauseIt;
      activatesound(_Error);
      IF _Error<>0 THEN
      BEGIN
        Stop;
        GOTO OutClose;
      END;
      kanalinit;
      pause:=PauseIt;
      MI.Playing:=true;
      {$IFDEF WAVE}
      IF SoundSettings.DeviceID = dev_HDD THEN
      BEGIN
        WITH SoundSettings DO i:=Longint(SizeOf(tWaveFileHeader))+(MI.TotalTime DIV 1000)
                                 *Stereo*(Bits DIV 8)*Samplerate+192000;
        RecordFile:=CreateWaveFile(SaveName , FNamesOEM, I, RecFile32Bit);
        IF RecordFile<>INVALID_HANDLE_VALUE THEN
        BEGIN
          SoundSettings.tMode := MMTask;
          RepeatSong          := False;
        END ELSE
        IF SoundSettings.DeviceID<-1 THEN
        BEGIN _Error:=id_NoWriteFile; STOP; GOTO OutClose; END;
      END;
      {$ENDIF}
      StartTask;
      IF SoundSettings.DeviceID<DEV_HDD THEN IF NOT Pause THEN TimerOn ELSE ELSE TimerOn;
    END;
OUTCLOSE:
    IF NOT(Mi.FileType IN [STM,S3M]) THEN LastFilePos:=_lPos32(FHandle, FIs32);
    _lClose32(FHandle, FIs32);
    FreeAllDOSMem;
  END ELSE _Error:=ID_FileNotFound;
  Play:=_Error;
  IF (GetActiveWindow=hWindow)AND(NOT IsIconic(hWindow)) THEN SetCursor(OldCursor);
  SetErrorMode(OldMode);
  {$IFDEF PROFILE_TIME} OutputDebugTime(1,'PLAYER32: Play '); {$ENDIF PROFILE_TIME}
END;

FUNCTION GetTimePattern(Time:Longint):Byte;EXPORT;ASSEMBLER;
{Liefert das Pattern zur Zeit...}
ASM
  mov cx,256
  mov si,offset PatternTimes
  DB 66h; XOR dx,dx
  XOR bx,bx
  DB 66h; mov di,word Ptr Time
@Loop:
  DB 66h; mov ax,word Ptr [si]
  DB 66h; cmp ax,di
  ja @Above
  je @GotIt
  DB 66h; cmp ax,dx
  jbe @Above
  mov bx,cx
  DB 66h; mov dx,ax
@Above:
  add si,4
  loop @Loop
  mov cx,bx
@GotIt:
  mov ax,256
  sub ax,cx
  inc ax
END;

PROCEDURE search_time(Time:Longint); {Sucht nach einer Spielposition im MOD-File}
VAR Reps:Boolean;
LABEL JMP, S3M_Inc;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  IF MI.Playing THEN
  BEGIN
    IF(Time<MI.TotalTime)OR(SearchPattern)THEN
    BEGIN
      IF((SearchPattern)AND(SearchP+SearchL>0))OR(Time>200)THEN
      BEGIN
        KanalInit;
        IF NOT(SearchPattern) THEN
        BEGIN
          IF Time>10000
          THEN CurrPattern:=GetTimePattern(Time-10000)
          ELSE CurrPattern:=0;
        END ELSE
        BEGIN
          CurrPattern:=SearchP+1;
          IF CurrPattern>2 THEN Dec(CurrPattern,2) ELSE CurrPattern:=0;
        END;
        WHILE PatternTimes[CurrPattern]<0 DO Dec(CurrPattern);
        PlayTime:=PatternTimes[CurrPattern];
        breakto:=0;
        i:=0;
        IF CurrPattern>0 THEN
        REPEAT
          if MI.FileType = S3M THEN case PArrangement[i] of
            $FE: goto S3M_Inc;
            $FF: break;
          end;
          GetPattern(i,256);
          FOR j:=0+breakto TO PtrnBreaks[i] DO
          BEGIN
            Include (RowPlayed[i], j);
            breakto:=0;
            pjump:=false;
            pbreak:=false;
            FOR k:=1 TO MI.NumTracks DO WITH Pattern^[k][j],Channels[k] DO
            CASE effekt OF
              NoEffekt     :;
              PositionJump :IF (Operands<MI.NumPatterns)THEN BEGIN pjump:=true; jumpto:=Operands; END;
              PatternBreak :BEGIN pbreak:=true; IF UsePattBrkOps THEN breakto:=10*(Operands SHR 4)+(Operands AND $0F); END;
              SetSpeed     :DoSetSpeed(Operands);
              SetBPMSpeed  :IF Operands >= 32 THEN BEGIN ForceBPM:=True; DoSetSpeed(Operands); ForceBPM:=False; END;
              FineTempoUp  :DoFineFarTempo (Operands);
              FineTempoDwn :DoFineFarTempo (-Operands);
              {
              Panning      :BEGIN
                              IF Mi.FileType=FT2
                              THEN PanVal:=Operands
                              ELSE IF Operands<128 THEN PanVal:=Operands SHL 1 ELSE
                                   IF Operands>128 THEN PanVal:=128 ELSE PanVal:=255;
                              VolAdjust(k);
                            END;
              FinePanLeft  :IF PanVal>17     THEN dec (PanVal,17) ELSE PanVal:=0;
              FinePanRight :IF PanVal<255-17 THEN inc (PanVal,17) ELSE PanVal:=255;
              EPanning   :PanVal:=Operands*17;
              { }
              StereoControl:SCont:=Succ(Operands);
              SetGlobVol   :BEGIN GlobVol:=Operands; IF GlobVol>64 THEN GlobVol:=64; END;
              VibratoDepth :FarVibDepth:=Operands;
              VibraWave: BEGIN
                  CASE (Operands AND $03) OF
                    0: VTable:=Ofs(sintable);
                    1: VTable:=Ofs(rampUptable);
                    2: VTable:=Ofs(sqrtable);
                  END;
                  IF Operands AND $04 = 0 THEN Channels[k].vtini:=0;
                END;
              TremWave: BEGIN
                  CASE (Operands AND $03) OF
                    0: TTable:=Ofs(sintable);
                    1: TTable:=Ofs(rampUptable);
                    2: TTable:=Ofs(sqrtable);
                  END;
                  IF Operands AND $04 = 0 THEN Channels[k].vtini:=0;
                END;
              GlissdCtrl :GlissandoOn:=Operands <> 0;
            END;
            IF pjump THEN BEGIN i:=jumpto; GOTO JMP END ELSE IF pbreak THEN GOTO S3M_Inc;
          END;
        S3M_Inc:
          inc(i);
          IF i>=MI.numpatterns THEN i:=0;
        JMP:
          IF PtrnBreaks[i]<breakto THEN breakto:=0;
        UNTIL i=CurrPattern;
        FOR k:=1 TO Mi.NumTracks DO VolAdjust(k); { }
        Reps:=RepeatSong;
        RepeatSong:=False;
        Search:=true;
        SearchTime:=Time;
        PeakSearch:=False;
        play_the_module; {Spielen im SEARCH-Modus}
        IF SoundSettings.DeviceID<DEV_HDD THEN
        FOR I:=1 TO 32 DO WITH Channels[i] DO IF Note<>0 THEN MODEvent.Offset:=Note-IOffset;
        pm_is_working:=true;
        Search:=false;
        RepeatSong:=Reps;
        MI.CurrPttrn:=CurrPattern+1;
        MI.CurrTime:=PlayTime;
        FOR i:=0 TO MaxPlayBuffer DO WITH PlayTimes^[i] DO
        BEGIN
          Pattrn    :=CurrPattern;
          Time      :=PlayTime;
          Effects   :=MI.CurrEffects;
          PlayInstr :=MI.CurrInstr;
        END;
      END ELSE KanalInit;
      IF SoundSettings.DeviceID<DEV_HDD THEN IF NOT Pause THEN TimerOn ELSE ELSE TimerOn;
    END ELSE Stop;
  END;
END;

PROCEDURE beginpause(Proceed:Boolean);
VAR BKWaveHdr:pMyWaveHdrArr;
    i,j:Longint;
LABEL Wait;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  IF MI.Playing THEN
  BEGIN
    pause:=true;
    TimerOff;
    { temp }
    IF WaveHandle<>0 THEN IF Proceed THEN Search_Time(Mi.CurrTime) ELSE
    BEGIN
      waveoutreset(WaveHandle);
      FOR i:=0 TO WaveBuffers-1 DO
      BEGIN
        WaveHeader[i].wh.dwFlags:=0;
        WaveHeader[i].BytePos:=0;
      END;
      CurrBuffer:=0;
    END;
    { temp }
    IF SoundSettings.DeviceID<DEV_HDD THEN MI.CpuUsage:=0;
  END;
END;

PROCEDURE endpause(Proceed:Boolean);
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  TimerOff;
  IF MI.Playing THEN
  BEGIN
    pause:=false;
    IF SoundSettings.DeviceID<DEV_HDD
    THEN Search_Time (MI.CurrTime)
    ELSE IF WaveHandle <> 0
      THEN IF Proceed
        THEN TimerOn
        ELSE Search_Time (MI.CurrTime)
      ELSE TimerOn;
  END;
END;

PROCEDURE SetTimermode(Mode:word);
BEGIN
  IF SoundSettings.DeviceID<=DEV_HDD THEN Exit;
  CASE Mode OF
    WMTimer,MMTimer,MMTask:
     BEGIN
       ende:=true;
       IF pm_is_working THEN
       BEGIN
         TimerOff;
         SoundSettings.Tmode:=Mode;
         TimerOn;
       END ELSE SoundSettings.Tmode:=Mode;
     END;
  END;
END;

PROCEDURE PatternSearch(NewPattern:Word); EXPORT;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD)
   OR(NOT Mi.Playing)
   OR(Mi.NumPatterns<NewPattern)
   OR(PatternTimes[NewPattern-1]<0) THEN Exit;
  SearchPattern:=True;
  SearchP := NewPattern-1;
  SearchL := -1;
  TimerOff;
  Search_Time (Mi.TotalTime);
  SearchPattern:=False;
END;

PROCEDURE settingschanged(VAR settings:tsettings; VAR _Error:word); EXPORT;
VAR p:Pointer;
    SoundChanged:Boolean;
BEGIN
  _Error:=0;
  IF NOT(Mi.Playing) THEN Exit;
  IF (soundsettings.DeviceID = dev_HDD)
  OR (settings.DeviceID      = dev_HDD)THEN
  BEGIN
    _Error := id_SyncDev;
    Exit;
  END;
  { temp }
  IF (settings.DeviceID < dev_HDD)THEN
  IF soundsettings.DeviceID = settings.DeviceID THEN
  BEGIN
    { only PreAmp, Surround and NoBPM can be changed in direct hardware mode }
    IF Settings.PreAmp <> SoundSettings.PreAmp THEN
    BEGIN
      SoundSettings.PreAmp := Settings.PreAmp;
      CalcVolMaster;
      IF pm_is_working THEN FOR i := 1 TO MI.NumTracks DO VolAdjust (i);
    END;
    IF (Settings.NoBPM <> SoundSettings.NoBPM) AND (MI.CurrPttrn <> 0) AND (MI.CurrLine <> 0) THEN
    BEGIN
      SearchPattern:=True;
      SearchP := Pred (MI.CurrPttrn);
      SearchL := Pred (MI.CurrLine);
      Soundsettings.NoBPM := Settings.NoBPM;
      TimerOff;
      GetTotalTime;
      Search_Time (SearchTime);
      SearchPattern:=False;
    END;
    IF settings.Surround<>soundsettings.Surround THEN
    BEGIN
      SoundSettings.Surround:=Settings.Surround;
      UseSurround:=Settings.Surround;
    END;
    IF Mi.Playing THEN Settings.Samplerate:=SoundSettings.SampleRate;
    Exit;
  END ELSE
  BEGIN
    Stop;
    Exit;
  END;
  IF soundsettings.DeviceID < dev_HDD THEN BEGIN Stop; Exit; END;
  { end temp }
  SoundChanged:=(settings.samplerate<>soundsettings.samplerate) OR
                (settings.bits      <>soundsettings.bits)       OR
                (settings.nbuffers  <>soundsettings.nbuffers)   OR
                (settings.DeviceID  <>soundsettings.DeviceID)   OR
                (settings.stereo    <>soundsettings.stereo);
  SetTimerMode(Settings.Tmode);
  IF (pm_is_working) AND
     (SoundChanged                                     OR
     (settings.Surround   <> soundsettings.Surround)   OR
     (settings.NoBPM      <> soundsettings.NoBPM)      OR
     (settings.OverSamp   <> soundsettings.OverSamp)   OR
     (settings.Panning    <> soundsettings.Panning)    OR
     (settings.PreAmp     <> soundsettings.PreAmp))THEN
  BEGIN
    IF (SoundSettings.DeviceID=DEV_HDD) THEN
    BEGIN
      STOP;
      Exit;
    END;
    IF (settings.NoBPM<>soundsettings.NoBPM)AND(MI.CurrPttrn<>0)AND(MI.CurrLine<>0)THEN
    BEGIN
      SearchPattern:=True;
      Searchp:=MI.CurrPttrn-1;
      SearchL:=MI.CurrLine-1;
    END ELSE SearchTime:=MI.CurrTime;
    IF(Settings.Preamp<>SoundSettings.Preamp)THEN BEGIN Soundsettings.PreAmp:=Settings.PreAmp; SetVolTable; END;
    IF Settings.Panning<>SoundSettings.Panning THEN
    BEGIN
      SoundSettings.Panning:=Settings.Panning;
      FOR i:=1 TO MI.NumTracks DO VolAdjust(i);
    END;
    UseSurround:=Settings.Surround;
    IF NOT SoundChanged AND(NOT SearchPattern)AND(Mi.BufTimeLen<800)THEN
    BEGIN
      IF(settings.Surround<>soundsettings.Surround)AND(Settings.Surround)THEN WITH Settings DO
      BEGIN
        p:=long2ptr(Playmemsel+SelectorInc,0);
        FillChar(p^,SurrBufferLen,0);
      END;
      IF NOT UseSurround THEN
      BEGIN
        FOR i:=1 TO MI.NumTracks DO channels[i].MixBuf:=MixBufferOfs;
        RSurFadeOut:=0;
        LSurFadeOut:=0;
      END;
      IF(settings.OverSamp<>soundsettings.OverSamp)THEN
      BEGIN
        CASE Settings.OverSamp OF
          1:   FOR i:=1 TO MI.NumTracks DO WITH Channels[i] DO JmpFlags:=(JmpFlags OR fOverSamp) AND (NOT fCPUCubic);
          2:   FOR i:=1 TO MI.NumTracks DO WITH Channels[i] DO JmpFlags:=(JmpFlags AND (NOT fOverSamp))OR fCPUCubic;
          3:   FOR i:=1 TO MI.NumTracks DO WITH Channels[i] DO JmpFlags:=JmpFlags OR fFPUCubic;
          ELSE FOR i:=1 TO MI.NumTracks DO WITH Channels[i] DO JmpFlags:=JmpFlags AND (NOT (fOverSamp OR fCPUCubic));
        END;
        SoundSettings.OverSamp:=Settings.OverSamp;
        SetVolTable;
        FOR i:=1 TO MI.NumTracks DO VolAdjust(i);
      END;
      soundsettings:=settings;
    END ELSE
    BEGIN
      GetAllDOSMem;
      IF SoundChanged THEN deactivatesound ELSE KanalInit;
      soundsettings:=settings;
      IF Searchpattern THEN GetTotalTime;
      IF SoundChanged THEN activatesound(_Error) ELSE SetVolTable;
      IF _Error=0 THEN search_Time(SearchTime) ELSE stop;
      SearchPattern:=False;
      FreeAllDOSMem;
    END;
  END;
END;

PROCEDURE EnableChn(Chn:Byte; How:Boolean); EXPORT;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  IF(Chn<33)AND(Chn>0)THEN
  BEGIN
    IF (pm_is_working)AND(Mi.BufTimeLen>800)THEN search_Time(MI.CurrTime);
    DisAbled[Chn]:=NOT(How);
  END;
END;

PROCEDURE EnableAllChn(How:Boolean); EXPORT;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  IF (pm_is_working)AND(Mi.BufTimeLen>800)THEN search_Time(MI.CurrTime);
  FillChar(DisAbled,SizeOf(DisAbled),NOT(How));
END;

FUNCTION IsEnabled(Chn:Byte):Boolean; EXPORT;
BEGIN
  IF (Chn<33)AND(Chn>0)THEN IsEnabled:=NOT DisAbled[Chn];
END;

PROCEDURE handlerepeat(rep:boolean); EXPORT;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  repeatsong:=rep;
END;

PROCEDURE RepeatPattern(How:Boolean); EXPORT;
BEGIN
  IF (SoundSettings.DeviceID=DEV_HDD) THEN Exit;
  IF(How<>OnlyOnePattern)THEN
  BEGIN
    OnlyOnePattern:=How;
    IF (pm_is_working)AND(Mi.BufTimeLen>800)THEN search_Time(MI.CurrTime);
  END;
END;

FUNCTION GetDataPtr:Pointer; EXPORT;
BEGIN
  GetDataPtr:=@MI;
END;

FUNCTION canplay(VAR what:tsettings; OvrRateChng: boolean):boolean; EXPORT;
VAR hwave:word;
    waveform:tPCMwaveformat;
    wavecaps:TWaveOutCaps;
LABEL Caps;
BEGIN
  canplay:=false;
  WITH What DO
  BEGIN
    IF samplerate=11000 THEN Samplerate:=11025 ELSE
    IF samplerate=22000 THEN Samplerate:=22050 ELSE
    IF samplerate=44000 THEN Samplerate:=44100;
    IF NOT(OverSamp IN [0..3]) THEN OverSamp:=1;
  END;
  CASE what.DeviceID OF
    dev_HDD: WITH what DO
      CanPlay:=(Samplerate>=1000)
            AND(Samplerate<=96000)
            AND((Stereo=2)OR(Stereo=1))
            AND((Bits=16)OR(Bits=8));
    dev_DAC..dev_DAC + 127:
      IF waveOutGetNumDevs>0 THEN
      BEGIN
        waveOutGetDevCaps(what.DeviceID,@wavecaps,SizeOf(TWaveOutCaps));
        IF (NOT OvrRateChng)AND(wavecaps.dwSupport AND Wavecaps_Playbackrate<>Wavecaps_Playbackrate) THEN
    CAPS:
        WITH Wavecaps DO WITH what DO BEGIN
          IF samplerate=11025 THEN hwave:=1 ELSE
          IF samplerate=22050 THEN hwave:=16 ELSE
          IF samplerate=44100 THEN hwave:=256 ELSE Exit;
          IF stereo=2 THEN BEGIN IF wChannels=1 THEN Exit; hwave:=hwave SHL 1; END;
          IF bits=16  THEN hwave:=hwave SHL 2;
          canplay:=(dwFormats AND hwave)=hwave;
        END ELSE
        BEGIN
          WITH waveform,wf,what DO
          BEGIN
            wformattag:=wave_format_pcm;
            nchannels:=stereo;
            nsamplespersec:=samplerate;
            navgbytespersec:=longint(samplerate)*stereo*(bits DIV 8);
            nblockalign:=stereo*(bits DIV 8);
            wbitspersample:=bits;
          END;
          IF waveoutopen(@hwave,what.deviceid,@waveform,0,0,wave_format_query OR wave_allowsync)=0
          THEN canplay:=true
          ELSE GOTO CAPS;
        END;
      END;
  END;
END;

FUNCTION GetVersion: word; export;
BEGIN GetVersion := ProductVersion; END;

{****************************** MCI *************************************}

{$IFDEF MCI}

CONST
   MCI_CONFIGURE              = $087A;
   MCI_OPEN_DRIVER            = $0801;
   MCI_CLOSE_DRIVER           = $0802;
   MCI_RESOURCE_RETURNED      = $00010000;
   MCI_FALSE                  =(MCI_STRING_OFFSET + 19);
   MCI_TRUE                   =(MCI_STRING_OFFSET + 20);
   MCI_FORMAT_MILLISECONDS_S  =(MCI_STRING_OFFSET + 21);
   MCI_FORMAT_HMS_S           =(MCI_STRING_OFFSET + 22);
   MCI_FORMAT_MSF_S           =(MCI_STRING_OFFSET + 23);
   MCI_FORMAT_FRAMES_S        =(MCI_STRING_OFFSET + 24);
   MCI_FORMAT_SMPTE_24_S      =(MCI_STRING_OFFSET + 25);
   MCI_FORMAT_SMPTE_25_S      =(MCI_STRING_OFFSET + 26);
   MCI_FORMAT_SMPTE_30_S      =(MCI_STRING_OFFSET + 27);
   MCI_FORMAT_SMPTE_30DROP_S  =(MCI_STRING_OFFSET + 28);
   MCI_FORMAT_BYTES_S         =(MCI_STRING_OFFSET + 29);
   MCI_FORMAT_SAMPLES_S       =(MCI_STRING_OFFSET + 30);
   MCI_FORMAT_TMSF_S          =(MCI_STRING_OFFSET + 31);

FUNCTION mciDriverNotify(hCallback:Word; uDeviceID:Word; uStatus:Word):WordBool; FAR; EXTERNAL 'MMSYSTEM.DLL';

FUNCTION mciDriverYield(uDeviceID:Word):WordBool; FAR; EXTERNAL 'MMSYSTEM.DLL';

FUNCTION MAKEMCIRESOURCE(wReturn, wResource:Word):Longint;ASSEMBLER;
ASM
  mov ax,wReturn
  mov dx,wResource
END;

TYPE pMCI_OPEN_DRIVER_PARMS=^tMCI_OPEN_DRIVER_PARMS;
     tMCI_OPEN_DRIVER_PARMS=RECORD
       wDeviceID           : Word;
       lpstrParams         : PChar;
       wCustomCommandTable : Word;
       wType               : Word;
     END;

TYPE pMCI_OPEN_PARAMS=^tMCI_OPEN_PARAMS;
     tMCI_OPEN_PARAMS=RECORD
       dwCallback       : Longint;
       wDeviceID        : Word;
       wReserved        : Word;
       lpstrDeviceType  : PChar;
       lpstrElementName : PChar;
       lpstrAlias       : PChar;
     END;

TYPE pMCI_GENERIC_PARAMS=^tMCI_GENERIC_PARAMS;
     tMCI_GENERIC_PARAMS=RECORD
       dwCallback       : Longint;
     END;

TYPE pMCI_INFO_PARMS=^tMCI_INFO_PARMS;
     tMCI_INFO_PARMS=RECORD
       dwCallback  : Longint;
       lpstrReturn : pChar;
       dwRetSize   : Longint;
     END;
TYPE pMCI_SET_PARMS=^tMCI_SET_PARMS;
     tMCI_SET_PARMS=RECORD
       dwCallback   : Longint;
       dwTimeFormat : Longint;
       dwAudio      : Longint;
     END;

TYPE pMCI_STATUS_PARMS=^tMCI_STATUS_PARMS;
     tMCI_STATUS_PARMS=RECORD
       dwCallback : Longint;
       dwReturn   : Longint;
       dwItem     : Longint;
       dwTrack    : Longint;
     END;

TYPE pMCI_Seek_PARMS=^tMCI_Seek_PARMS;
     tMCI_Seek_PARMS=RECORD
       dwCallback : Longint;
       dwTo       : Longint;
     END;

TYPE pMCI_LOAD_PARMS=^tMCI_LOAD_PARMS;
     tMCI_LOAD_PARMS=RECORD
       dwCallback : Longint;
       lpfilename : PChar;
     END;

TYPE PMCI_Window_Parms = ^TMCI_Window_Parms;
     TMCI_Window_Parms = RECORD
       dwCallback : Longint;
       Wnd        : Longint;
       nCmdShow   : Longint;
       lpstrText  : PChar;
     end;

TYPE pDRVCONFIGINFO=^tDRVCONFIGINFO;
     tDRVCONFIGINFO=RECORD
       dwDCISize:Longint;
       lpszDCISectionName:PChar;
       lpszDCIAliasName:PChar;
     END;

CONST UsageCount    : Word                 = 0;
      TimeFormat    : Longint              = MCI_FORMAT_MILLISECONDS;
      IsPause       : Boolean              = False;
      FileName      : ARRAY[0..260]OF Char = #0;
      FirstPlayed   : Boolean              = True;
      DlgWnd        : hWnd                 = 0;
VAR   Settings      : tSettings;
      MCIPeakSearch    : Boolean;
      I             : Longint;

CONST TaskCallback:Longint=0;
      TaskDriverID:Longint=0;
      MCITID:Word=0;

CONST INISection:PChar='M4W_MCI.DRV'#0;
      INIFile:PChar='SYSTEM.INI'#0;

FUNCTION DialogProc(hWnd,message,wParam:word;lParam:longint):Longint;EXPORT;
VAR i,j:Integer;
    S:String;
BEGIN
  DialogProc:=1;
  CASE Message OF
    WM_Command:CASE wParam OF
     1:BEGIN
         IF IsDlgButtonChecked(hWnd,101)<>0
         THEN WritePrivateProfileString(INISection,'SampleRate'#0,'1'#0,INIFile)
         ELSE IF IsDlgButtonChecked(hWnd,102)<>0
         THEN WritePrivateProfileString(INISection,'SampleRate'#0,'2'#0,INIFile)
         ELSE WritePrivateProfileString(INISection,'SampleRate'#0,'3'#0,INIFile);
         IF IsDlgButtonChecked(hWnd,120)<>0
         THEN WritePrivateProfileString(INISection,'Driver'#0,'0'#0,INIFile)
         ELSE IF IsDlgButtonChecked(hWnd,121)<>0
         THEN WritePrivateProfileString(INISection,'Driver'#0,'1'#0,INIFile)
         ELSE IF IsDlgButtonChecked(hWnd,122)<>0
         THEN WritePrivateProfileString(INISection,'Driver'#0,'2'#0,INIFile)
         ELSE WritePrivateProfileString(INISection,'Driver'#0,'3'#0,INIFile);
         IF IsDlgButtonChecked(hWnd,106)<>0
         THEN WritePrivateProfileString(INISection,'Bits'#0,'8'#0,INIFile)
         ELSE IF IsDlgButtonChecked(hWnd,107)<>0
         THEN WritePrivateProfileString(INISection,'Bits'#0,'16'#0,INIFile);
         {PeakSearch}
         STR(IsDlgButtonChecked(hWnd,110),s);s:=s+#0;
         WritePrivateProfileString(INISection,'PeakSearch'#0,@S[1],INIFile);
         {IDO}
         STR(IsDlgButtonChecked(hWnd,111)*3,s);s:=s+#0;
         WritePrivateProfileString(INISection,'IDO'#0,@S[1],INIFile);
         {Surround}
         STR(IsDlgButtonChecked(hWnd,112),s);s:=s+#0;
         WritePrivateProfileString(INISection,'Surround'#0,@S[1],INIFile);
         EndDialog(hWnd,1);
       END;
     2:EndDialog(hWnd,2);
     ELSE DialogProc:=0;
    END;
    WM_InitDialog:
    BEGIN
      CASE GetPrivateProfileInt(INISection,'SampleRate'#0,3,INIFile) OF
        1:CheckDlgButton(hWnd,101,1);
        2:CheckDlgButton(hWnd,102,1);
        ELSE CheckDlgButton(hWnd,103,1);
      END;
      CASE GetPrivateProfileInt(INISection,'Bits'#0,16,INIFile) OF
        16:CheckDlgButton(hWnd,107,1);
        8:CheckDlgButton(hWnd,106,1);
        ELSE CheckDlgButton(hWnd,107,1);
      END;
      CheckDlgButton(hWnd,110,GetPrivateProfileInt(INISection,'PeakSearch'#0,1,INIFile));
      CheckDlgButton(hWnd,111,GetPrivateProfileInt(INISection,'IDO'#0,1,INIFile));
      CheckDlgButton(hWnd,112,GetPrivateProfileInt(INISection,'Surround'#0,1,INIFile));
      SetWindowLong(GetDLGItem(hWnd,121),gwl_style,ws_disabled OR GetWindowLong(GetDLGItem(hWnd,121),GWL_Style));
      CASE GetPrivateProfileInt(INISection,'Driver'#0,3,INIFile) OF
        1:{IF MOD_CanPlay(DefAWESetting,True) THEN CheckDlgButton(hWnd,121,1) ELSE}CheckDlgButton(hWnd,120,1);
        ELSE CheckDlgButton(hWnd,120,1);
      END;
    END;
    ELSE DialogProc:=0;
  END;
END;

FUNCTION DialogProc1(hWnd,message,wParam:word;lParam:longint):Longint;EXPORT;
BEGIN
  DialogProc1:=1;
  CASE Message OF
    WM_Command:CASE wParam OF
     1:BEGIN
         DlgWnd:=0;
         EndDialog(hWnd,1);
       END;
     ELSE DialogProc1:=0;
    END;
    ELSE DialogProc1:=0;
  END;
END;

PROCEDURE MCITimerproc(hwind:hwnd; msg:word; idTimer:word; Time:Longint); EXPORT;
BEGIN
  IF (NOT MI.Playing)AND(MCITID<>0) THEN
  BEGIN
    KillTimer(0,MCITID);
    MCITID:=0;
    Stop;
    {$IFDEF MCI_Debug}
    IF TaskCallback<>0 THEN OutputDebugString('M4W_MCI.DRV: Play_End_Notify'#13#10' '#13#10);
    {$ENDIF MCI_Debug}
    IF TaskCallback<>0 THEN
    BEGIN
      mciDriverNotify(TaskCallback, TaskDriverID, MCI_NOTIFY_SUCCESSFUL);
      TaskCallback:=0;
    END;
  END;
END;

PROCEDURE StopMCITimer;
BEGIN
  IF MCITID<>0 THEN
  BEGIN
    KillTimer(0,MCITID);
    MCITID:=0;
    Stop;
    IF TaskCallback<>0 THEN
    BEGIN
      {$IFDEF MCI_Debug}
      OutputDebugString('M4W_MCI.DRV: StopMCITimer'#13#10' '#13#10);
      {$ENDIF MCI_Debug}
      mciDriverNotify(TaskCallback, TaskDriverID, MCI_NOTIFY_ABORTED);
      TaskCallback:=0;
    END;
  END;
END;


PROCEDURE GetSettings;
VAR I:Word;
    OldPreAmp:Byte;
    J:Longint;
BEGIN
  OldPreAmp:=Settings.PreAmp;
  CASE GetPrivateProfileInt(INISection,'Driver'#0,3,INIFile) OF
    1:{IF MOD_CanPlay(DefAWESetting,True) THEN CheckDlgButton(hWnd,121,1) ELSE} Settings:=DefDACSetting;
    ELSE Settings:=DefDACSetting;
  END;
  Settings.PreAmp:=OldPreAmp;
  IF Settings.DeviceID=DefDACSetting.DeviceID THEN
  BEGIN
    CASE GetPrivateProfileInt(INISection,'SampleRate'#0,3,INIFile) OF
      1:Settings.SampleRate:=11025;
      2:Settings.SampleRate:=22050;
      ELSE Settings.SampleRate:=44100;
    END;
    CASE GetPrivateProfileInt(INISection,'Bits'#0,16,INIFile) OF
      16:Settings.Bits:=16;
      8:Settings.Bits:=8;
      ELSE Settings.Bits:=8;
    END;
    Settings.OverSamp:=GetPrivateProfileInt(INISection,'IDO'#0,1,INIFile);
    IF NOT(Settings.OverSamp IN [0..3]) THEN Settings.OverSamp:=3;
  END;
  Settings.Surround:=GetPrivateProfileInt(INISection,'Surround'#0,1,INIFile)<>0;
  MCIPeakSearch:=GetPrivateProfileInt(INISection,'PeakSearch'#0,1,INIFile)<>0;
  {TEMP!}
  IF MCIPeakSearch THEN
  BEGIN
    WriteProfileString('Player32','PeakSearch','1');
    WriteProfileString('Player32','ShowPeakSearch','1')
  END ELSE
  BEGIN
    WriteProfileString('Player32','PeakSearch','0');
  END;
  IF MI.Playing THEN
  BEGIN
    IF MCITID<>0 THEN KillTimer(0,MCITID);
    J:=MI.CurrTime;
    SettingsChanged(Settings,i);
    IF (NOT MI.Playing)AND(Play(1,FileName,NIL,False,@Settings,True,False)=0) THEN
    BEGIN
      MI.CurrTime:=J;
      IF NOT IsPause THEN EndPause(False);
    END;
    IF MCITID<>0 THEN
    BEGIN
      MCITID:=SetTimer(0,4444,55,@MCITimerProc);
      {$IFDEF MCI_Debug}
      OutputDebugString('M4W_MCI.DRV: GetSettings Timer on...'#13#10' '#13#10);
      {$ENDIF MCI_Debug}
    END;
  END;
END;

{$IFDEF MCI_Debug}
PROCEDURE AddFlags(PC:PChar;VAR VALUE:Longint;Flag:Longint;Description:PChar);
BEGIN
  IF Value AND Flag<>0 THEN
  BEGIN
    Value:=Value AND (NOT Flag);
    StrCat(PC,Description);
    IF Value<>0 THEN StrCat(PC,' | ');
  END;
END;

PROCEDURE DebugOut(dwDriverID:Longint;hDriver,wMsg:Word; lParam1,lParam2:Longint);
VAR Args:ARRAY[0..15]OF Longint;
    PC:ARRAY[0..260]OF Char;
BEGIN
  pChar(Args[0]):='M4W_MCI.DRV: '#0;
  Args[1]:=wMsg;
  Args[2]:=dwDriverID;
  Args[3]:=hDriver;
  Args[4]:=lParam1;
  Args[5]:=lParam2;
  CASE wMsg OF
    DRV_LOAD:
      BEGIN
        pChar(Args[1]):='DRV_Load'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_FREE:
      BEGIN
        pChar(Args[1]):='DRV_Free'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_OPEN:
      BEGIN
      { the DRV_OPEN message is received once for each MCI device open }
      {lParam1=Command Line String; lParam2=pMCI_OPEN_DRIVER_PARMS}
        pChar(Args[1]):='DRV_Open'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Command Line: "%s",pParams= 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF pMCI_OPEN_DRIVER_PARMS(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='DRV_OPEN_PARMS'#0;
          Args[2]:=pMCI_OPEN_DRIVER_PARMS(lParam2)^.wDeviceID;
          pChar(Args[3]):=pMCI_OPEN_DRIVER_PARMS(lParam2)^.lpstrParams;
          Args[4]:=pMCI_OPEN_DRIVER_PARMS(lParam2)^.wCustomCommandTable;
          Args[5]:=pMCI_OPEN_DRIVER_PARMS(lParam2)^.wType;
          WVSPrintF(PC,'%s %s(wDeviceID= 0x%00004lX, lpStrParams: "%s",wCommandTable= 0x%00004lX, '+
                       'wType=0x%00004lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    DRV_CLOSE:
      BEGIN
        pChar(Args[1]):='DRV_Close'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_QUERYCONFIGURE:
      BEGIN
        pChar(Args[1]):='DRV_QUERYCONFIGURE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    MCI_CONFIGURE:
      BEGIN
        pChar(Args[1]):='UNDOCUMENTED: MCI_CONFIGURE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_CONFIGURE:
      BEGIN
        pChar(Args[1]):='DRV_CONFIGURE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_ENABLE:
      BEGIN
        pChar(Args[1]):='DRV_ENABLE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_DISABLE:
      BEGIN
        pChar(Args[1]):='DRV_DISABLE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_INSTALL:
      BEGIN
        pChar(Args[1]):='DRV_INSTALL'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_REMOVE:
      BEGIN
        pChar(Args[1]):='DRV_REMOVE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    DRV_ExitApplication:
      BEGIN
        pChar(Args[1]):='DRV_ExitApplication'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
    MCI_Open,
    MCI_OPEN_DRIVER:
      BEGIN
        pChar(Args[1]):='MCI_OPEN_DRIVER'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'OPEN_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_OPEN_DRIVER Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_OPEN_ALIAS,'MCI_OPEN_ALIAS');
          AddFlags(PC,lParam1,MCI_OPEN_SHAREABLE,'MCI_OPEN_SHAREABLE');
          AddFlags(PC,lParam1,MCI_OPEN_TYPE,'MCI_OPEN_TYPE');
          AddFlags(PC,lParam1,MCI_OPEN_TYPE_ID,'MCI_OPEN_TYPE_ID');
          AddFlags(PC,lParam1,MCI_OPEN_ELEMENT,'MCI_OPEN_ELEMENT');
          AddFlags(PC,lParam1,MCI_OPEN_ELEMENT_ID,'MCI_OPEN_ELEMENT_ID');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_OPEN_PARAMS(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_OPEN_PARMS'#0;
          Args[2]:=pMCI_OPEN_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_OPEN_PARMS(lParam2)^.wDeviceID;
          pChar(Args[4]):=pMCI_OPEN_PARMS(lParam2)^.lpstrDeviceType;
          pChar(Args[5]):=pMCI_OPEN_PARMS(lParam2)^.lpstrElementName;
          pChar(Args[6]):=pMCI_OPEN_PARMS(lParam2)^.lpstrAlias;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, wDeviceID= 0x%00004lX, lpStrType: "%s",'+
                       ' lpStrElement: "%s", lpStrAlias: "%s")'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Close,
    MCI_CLOSE_DRIVER:
      BEGIN
        pChar(Args[1]):='MCI_CLOSE_DRIVER'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_CLOSE_DRIVER Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_STOP:
      BEGIN
        pChar(Args[1]):='MCI_STOP'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_STOP Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_PAUSE:
      BEGIN
        pChar(Args[1]):='MCI_PAUSE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_PAUSE Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_RESUME:
      BEGIN
        pChar(Args[1]):='MCI_RESUME'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_RESUME Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_CUE:
      BEGIN
        pChar(Args[1]):='MCI_CUE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_CUE Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_SEEK:
      BEGIN
        pChar(Args[1]):='MCI_SEEK'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'SEEK_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_SEEK Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_TO,'MCI_TO');
          AddFlags(PC,lParam1,MCI_SEEK_TO_START,'MCI_SEEK_TO_START');
          AddFlags(PC,lParam1,MCI_SEEK_TO_END,'MCI_SEEK_TO_END');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Seek_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_SEEK_PARMS'#0;
          Args[2]:=pMCI_Seek_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_Seek_PARMS(lParam2)^.dwTo;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, dwTo= %li)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_PLAY:
      BEGIN
        pChar(Args[1]):='MCI_PLAY'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'PLAY_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_PLAY Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_FROM,'MCI_FROM');
          AddFlags(PC,lParam1,MCI_TO,'MCI_TO');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Play_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_PLAY_PARMS'#0;
          Args[2]:=pMCI_Play_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_Play_PARMS(lParam2)^.dwFrom;
          Args[4]:=pMCI_Play_PARMS(lParam2)^.dwTo;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, dwFrom= %li, dwTo= %li)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_RECORD:
      BEGIN
        pChar(Args[1]):='MCI_RECORD'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'RECORD_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_RECORD Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_FROM,'MCI_FROM');
          AddFlags(PC,lParam1,MCI_TO,'MCI_TO');
          AddFlags(PC,lParam1,MCI_RECORD_INSERT,'MCI_RECORD_INSERT');
          AddFlags(PC,lParam1,MCI_RECORD_OVERWRITE,'MCI_RECORD_OVERWRITE');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_RECORD_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_RECORD_PARMS'#0;
          Args[2]:=pMCI_RECORD_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_RECORD_PARMS(lParam2)^.dwFrom;
          Args[4]:=pMCI_RECORD_PARMS(lParam2)^.dwTo;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, dwFrom= %li, dwTo= %li)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_INFO:
      BEGIN
        pChar(Args[1]):='MCI_INFO'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'INFO_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_INFO Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_INFO_PRODUCT,'MCI_INFO_PRODUCT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_INFO_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_INFO_PARMS'#0;
          Args[2]:=pMCI_INFO_PARMS(lParam2)^.dwCallback;
          pChar(Args[3]):=pMCI_INFO_PARMS(lParam2)^.lpStrReturn;
          Args[4]:=pMCI_INFO_PARMS(lParam2)^.dwRetSize;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, lpstrReturn= "%s", dwRetSize= %li)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_STATUS:
      BEGIN
        { }
        pChar(Args[1]):='MCI_STATUS'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'STATUS_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_STATUS Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_STATUS_ITEM,'MCI_STATUS_ITEM');
          AddFlags(PC,lParam1,MCI_STATUS_START,'MCI_STATUS_START');
          AddFlags(PC,lParam1,MCI_TRACK,'MCI_TRACK');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_STATUS_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_STATUS_PARMS'#0;
          Args[2]:=pMCI_STATUS_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_STATUS_PARMS(lParam2)^.dwReturn;
          CASE pMCI_STATUS_PARMS(lParam2)^.dwItem OF
            MCI_STATUS_CURRENT_TRACK    : pChar(Args[4]):='MCI_STATUS_CURRENT_TRACK';
            MCI_STATUS_LENGTH           : pChar(Args[4]):='MCI_STATUS_LENGTH';
            MCI_STATUS_MODE             : pChar(Args[4]):='MCI_STATUS_MODE';
            MCI_STATUS_NUMBER_OF_TRACKS : pChar(Args[4]):='MCI_STATUS_NUMBER_OF_TRACKS';
            MCI_STATUS_POSITION         : pChar(Args[4]):='MCI_STATUS_POSITION';
            MCI_STATUS_READY            : pChar(Args[4]):='MCI_STATUS_READY';
            MCI_STATUS_TIME_FORMAT      : pChar(Args[4]):='MCI_STATUS_TIME_FORMAT';
            ELSE                          pChar(Args[4]):='MCI_STATUS_UNKNOWN';
          END;
          Args[5]:=pMCI_STATUS_PARMS(lParam2)^.dwTrack;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, dwReturn= 0x%000000008lX, '+
                       'dwItem= "%s", dwTrack= 0x%000000008lX,)'#13#10,Args);
          OutputDebugString(PC);
        END;
        { }
        Exit;
      END;
    MCI_LOAD:
      BEGIN
        pChar(Args[1]):='MCI_LOAD'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'LOAD_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_LOAD Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_LOAD_FILE,'MCI_LOAD_FILE');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_LOAD_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_LOAD_PARMS'#0;
          Args[2]:=pMCI_LOAD_PARMS(lParam2)^.dwCallback;
          pChar(Args[3]):=pMCI_LOAD_PARMS(lParam2)^.lpFileName;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, lpFileName= "%s")'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_SAVE:
      BEGIN
        pChar(Args[1]):='MCI_SAVE'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'SAVE_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_SAVE Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_SAVE_FILE,'MCI_SAVE_FILE');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_SAVE_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_SAVE_PARMS'#0;
          Args[2]:=pMCI_SAVE_PARMS(lParam2)^.dwCallback;
          pChar(Args[3]):=pMCI_SAVE_PARMS(lParam2)^.lpFileName;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, lpFileName= "%s")'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_SET:
      BEGIN
        pChar(Args[1]):='MCI_SET'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'SET_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_SET Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_SET_AUDIO,'MCI_SET_AUDIO');
          AddFlags(PC,lParam1,MCI_SET_DOOR_CLOSED,'MCI_SET_DOOR_CLOSED');
          AddFlags(PC,lParam1,MCI_SET_DOOR_OPEN,'MCI_SET_DOOR_OPEN');
          AddFlags(PC,lParam1,MCI_SET_TIME_FORMAT,'MCI_SET_TIME_FORMAT');
          AddFlags(PC,lParam1,MCI_SET_VIDEO,'MCI_SET_VIDEO');
          AddFlags(PC,lParam1,MCI_SET_OFF,'MCI_SET_OFF');
          AddFlags(PC,lParam1,MCI_SET_ON,'MCI_SET_ON');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_SET_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_SET_PARMS'#0;
          Args[2]:=pMCI_SET_PARMS(lParam2)^.dwCallback;
          CASE pMCI_SET_PARMS(lParam2)^.dwTimeFormat OF
            mci_Format_Milliseconds : pChar(Args[3]):='mci_Format_Milliseconds'#0;
            mci_Format_HMS          : pChar(Args[3]):='mci_Format_HMS'#0;
            mci_Format_MSF          : pChar(Args[3]):='mci_Format_MSF'#0;
            mci_Format_Frames       : pChar(Args[3]):='mci_Format_Frames'#0;
            mci_Format_SMPTE_24     : pChar(Args[3]):='mci_Format_SMPTE_24'#0;
            mci_Format_SMPTE_25     : pChar(Args[3]):='mci_Format_SMPTE_25'#0;
            mci_Format_SMPTE_30     : pChar(Args[3]):='mci_Format_SMPTE_30'#0;
            mci_Format_SMPTE_30Drop : pChar(Args[3]):='mci_Format_SMPTE_30Drop'#0;
            mci_Format_Bytes        : pChar(Args[3]):='mci_Format_Bytes'#0;
            mci_Format_Samples      : pChar(Args[3]):='mci_Format_Samples'#0;
            mci_Format_TMSF         : pChar(Args[3]):='mci_Format_TMSF'#0;
            ELSE                      pChar(Args[3]):='mci_Format_UNKNOWN'#0;
          END;
          CASE pMCI_SET_PARMS(lParam2)^.dwAudio OF
            mci_Set_Audio_All       : pChar(Args[4]):='mci_Set_Audio_All'#0;
            mci_Set_Audio_Left      : pChar(Args[4]):='mci_Set_Audio_Left'#0;
            mci_Set_Audio_Right     : pChar(Args[4]):='mci_Set_Audio_Right'#0;
            ELSE                      pChar(Args[4]):='mci_Set_Audio_UNKNOWN'#0;
          END;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, "%s", "%s")'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_GETDEVCAPS:
      BEGIN
        pChar(Args[1]):='MCI_GETDEVCAPS'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'GETDEVCAPS_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_GETDEVCAPS Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,MCI_GETDEVCAPS_ITEM,'MCI_GETDEVCAPS_ITEM');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_GETDEVCAPS_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GETDEVCAPS_PARMS'#0;
          Args[2]:=pMCI_GETDEVCAPS_PARMS(lParam2)^.dwCallback;
          Args[3]:=pMCI_GETDEVCAPS_PARMS(lParam2)^.dwReturn;
          CASE pMCI_GETDEVCAPS_PARMS(lParam2)^.dwItem OF
            MCI_GETDEVCAPS_CAN_EJECT       : pChar(Args[4]):='CAN_EJECT';
            MCI_GETDEVCAPS_CAN_PLAY        : pChar(Args[4]):='CAN_PLAY';
            MCI_GETDEVCAPS_CAN_RECORD      : pChar(Args[4]):='CAN_RECORD';
            MCI_GETDEVCAPS_CAN_SAVE        : pChar(Args[4]):='CAN_SAVE';
            MCI_GETDEVCAPS_COMPOUND_DEVICE : pChar(Args[4]):='COMPOUND_DEVICE';
            MCI_GETDEVCAPS_DEVICE_TYPE     : pChar(Args[4]):='DEVICE_TYPE';
            MCI_GETDEVCAPS_HAS_AUDIO       : pChar(Args[4]):='HAS_AUDIO';
            MCI_GETDEVCAPS_HAS_VIDEO       : pChar(Args[4]):='HAS_VIDEO';
            MCI_GETDEVCAPS_USES_FILES      : pChar(Args[4]):='USES_FILES';
            ELSE                             pChar(Args[4]):='Unknown_Caps';
          END;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX, dwReturn= 0x%000000008lX, CAPS "%s")'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Window:
      BEGIN
        pChar(Args[1]):='MCI_WINDOW'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX, Flags= 0x%000000008lX, '+
                     'lpWindow= 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_WINDOW Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          AddFlags(PC,lParam1,mci_Anim_Window_Enable_Stretch,'MCI_Window_Enable_Stretch');
          AddFlags(PC,lParam1,mci_Anim_Window_Disable_Stretch,'MCI_Window_Disable_Stretch');
          AddFlags(PC,lParam1,MCI_ANIM_WINDOW_HWND,'MCI_WINDOW_HWND');
          AddFlags(PC,lParam1,MCI_ANIM_WINDOW_STATE,'MCI_WINDOW_STATE');
          AddFlags(PC,lParam1,MCI_ANIM_WINDOW_TEXT,'MCI_WINDOW_TEXT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF PMCI_Window_Parms(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[0]):='MCI_WINDOW_PARMS'#0;
          Args[1]:=PMCI_Window_Parms(lParam2)^.dwCallback;
          Args[2]:=PMCI_Window_Parms(lParam2)^.Wnd;
          Args[3]:=PMCI_Window_Parms(lParam2)^.nCmdShow;
          pChar(Args[4]):=PMCI_Window_Parms(lParam2)^.lpstrText;
          WVSPrintF(PC,'%s: dwCallback: 0x%000000008lX; hWnd: 0x%000000008lX; cmdShow: 0x%000000008lX; Text: "%s"'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Put:
      BEGIN
        pChar(Args[1]):='MCI_PUT'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_PUT Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Cut:
      BEGIN
        pChar(Args[1]):='MCI_CUT'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_CUT Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Copy:
      BEGIN
        pChar(Args[1]):='MCI_COPY'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_COPY Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Paste:
      BEGIN
        pChar(Args[1]):='MCI_Paste'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_Paste Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Delete:
      BEGIN
        pChar(Args[1]):='MCI_Delete'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'Generic_PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_Delete Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
        IF pMCI_Generic_Params(lParam2)<>NIL THEN
        BEGIN
          pChar(Args[1]):='MCI_GENERIC_PARMS'#0;
          Args[2]:=pMCI_GENERIC_PARMS(lParam2)^.dwCallback;
          WVSPrintF(PC,'%s %s(dwCallback= 0x%000000008lX)'#13#10,Args);
          OutputDebugString(PC);
        END;
      END;
    MCI_Sound:
      BEGIN
        pChar(Args[1]):='MCI_Sound'#0;
        WVSPrintF(PC,'%s %s(ID= 0x%000000008lX, hDriver= 0x%00004lX,Flags= 0x%000000008lX,'+
                     'PARAMS: 0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
        IF lParam1<>0 THEN
        BEGIN
          StrCopy(PC,pChar(Args[0]));
          StrCat(PC,' MCI_Sound Flags are '#0);
          AddFlags(PC,lParam1,MCI_NOTIFY,'MCI_NOTIFY');
          AddFlags(PC,lParam1,MCI_WAIT,'MCI_WAIT');
          IF lParam1<>0 THEN
          BEGIN
            OutputDebugString(PC);
            WVSPrintF(PC,'0x%000000008lX'#13#10,lParam1);
          END ELSE StrCat(PC,#13#10);
          OutputDebugString(PC);
        END;
      END;
    ELSE
      BEGIN
        Args[1]:=wMsg;
        WVSPrintF(PC,'%s !!UNKNOWN!! 0x%00004lx(ID= 0x%000000008lX, hDriver= 0x%00004lX,lParams= 0x%000000008lX, '+
                     '0x%000000008lX)'#13#10,Args);
        OutputDebugString(PC);
      END;
  END;
  OutputDebugString(' '#13#10);
END;
{$ENDIF MCI_Debug}

Function Min(V1,V2:Integer):integer;
BEGIN
  if V1<V2 THEN Min:=V1 ELSE Min:=V2;
END;

FUNCTION DriverProc(dwDriverID:Longint;hDriver,wMsg:Word; lParam1,lParam2:Longint):Longint;EXPORT;
VAR SeekToTime:Longint;
LABEL Config,DoEndPause;
BEGIN
  DriverProc:=0;
  {$IFDEF MCI_Debug}
  DebugOut(dwDriverID,hDriver,wMsg,lParam1,lParam2);
  {$ENDIF MCI_Debug}
  CASE wMsg OF
{*****************************************************************************}
{*                                                                           *}
{*   Driver Specific Stuff                                                   *}
{*                                                                           *}
{*****************************************************************************}
    DRV_LOAD:
      BEGIN
        { the DRV_LOAD message is received once, when the driver is }
        { first loaded - any one-time initialization code goes here }
        DriverProc:=1;
      END;
    DRV_FREE:
      { the DRV_FREE message is received once when the driver is }
      { unloaded - any final shut down code goes here }
      BEGIN
        DriverProc:=1;
      END;
    DRV_OPEN:
      { the DRV_OPEN message is received once for each MCI device open }
      {lParam1=Command Line String; lParam2=pMCI_OPEN_DRIVER_PARMS}
      IF pMCI_OPEN_DRIVER_PARMS(lParam2)<>NIL THEN
      BEGIN
        pMCI_OPEN_DRIVER_PARMS(lParam2)^.wType:=mci_DevType_Sequencer;
        pMCI_OPEN_DRIVER_PARMS(lParam2)^.wCustomCommandTable:=$FFFF;
        IF UsageCount=0 THEN DriverProc:=$1234 ELSE DriverProc:=1;
      END ELSE DriverProc:=1;
    DRV_CLOSE:
      { this message is received once for each MCI device close }
      BEGIN
        DriverProc:=1;
      END;
    DRV_QUERYCONFIGURE:
      { the DRV_QUERYCONFIGURE message is used to determine if the }
      { DRV_CONCIGURE message is supported - return 1 to indicate }
      { configuration is supported }
      DriverProc:=1;
    MCI_CONFIGURE:
      IF lParam1=0 THEN
      BEGIN
        DriverProc:=DrvCnf_OK;
        GOTO Config;{ }
      END;
    DRV_CONFIGURE:
Config:
      BEGIN
        IF DialogBox(hInstance,'SETUPDLG',0,@DialogProc)=1 THEN
        BEGIN
          DriverProc:=DrvCnf_OK;
          IF MI.Playing THEN
          IF dwDriverID<>$1234
          THEN MessageBox(0,'The changes will take effect'#10+
                            'next time you start playback!'
                           ,'Information',MB_IconInformation)
          ELSE { }GetSettings;
        END ELSE DriverProc:=DrvCnf_Cancel;
      END;
{*****************************************************************************}
{*                                                                           *}
{*   MCI Specific Stuff                                                      *}
{*                                                                           *}
{*****************************************************************************}
    MCI_OPEN_DRIVER:
      {dwParam1=Flags;dwParam2=pMCI_OPEN_PARAMS}
      IF (lParam1 AND MCI_OPEN_SHAREABLE<>0)THEN DriverProc:=mcierr_Flags_Not_Compatible ELSE
      IF (lParam1 AND MCI_OPEN_ELEMENT<>0) THEN
      BEGIN
        IF (UsageCount=0) THEN
        BEGIN
          Inc(UsageCount);
          IF Pointer(lParam2)<>NIL THEN StrCopy(FileName,pMCI_OPEN_PARAMS(lParam2)^.lpstrElementName);
          Play($FFFF,FileName,NIL,False,@Settings,False,False);
          {$IFDEF MCI_Debug}
          OutputDebugString('M4W_MCI:'#0);
          OutputDebugString(FileName);
          OutputDebugString(#13#10);
          {$ENDIF MCI_Debug}
          IsPause:=False;
        END ELSE DriverProc:=mcierr_Device_Open;
      END;
    MCI_CLOSE_DRIVER:IF (dwDriverID=$1234)THEN
      BEGIN
        IF TaskCallback<>0 THEN lParam1:=lParam1 AND (NOT MCI_NOTIFY);
        IF MCITID=0 THEN Stop ELSE StopMCITimer;
        UsageCount:=0;
      END;
    MCI_STOP:IF (dwDriverID<>$1234) THEN DriverProc:=mcierr_NONAPPLICABLE_Function ELSE
      BEGIN
        IsPause:=False;
        IF MCITID=0 THEN Stop ELSE
        BEGIN
          IF TaskCallback<>0 THEN lParam1:=lParam1 AND (NOT MCI_NOTIFY);
          StopMCITimer;
          MI.CurrTime:=0;
        END;
      END;
    MCI_PAUSE:IF (dwDriverID<>$1234) THEN DriverProc:=mcierr_NONAPPLICABLE_Function ELSE
      BEGIN
        BeginPause(False);
        IsPause:=True;
      END;
    MCI_RESUME: IF (dwDriverID<>$1234) THEN DriverProc:=mcierr_NONAPPLICABLE_Function ELSE
      BEGIN
DoEndPause:
        EndPause(False);
        IsPause:=False;
      END;
    MCI_SEEK:IF (dwDriverID<>$1234) THEN DriverProc:=mcierr_NONAPPLICABLE_Function ELSE
      BEGIN
        IF MI.Playing AND NOT(IsPause) THEN BeginPause(False);
        IF (lParam1 AND MCI_TO<>0)AND(Pointer(lParam2)<>NIL) THEN
        BEGIN
          MI.CurrTime:=pMCI_Seek_PARMS(lParam2)^.dwTo;
          IF MI.CurrTime>MI.TotalTime-200 THEN MI.CurrTime:=MI.TotalTime-200 ELSE
          IF MI.CurrTime<0                THEN MI.CurrTime:=0;
        END ELSE
        IF (lParam1 AND MCI_Seek_To_Start<>0) THEN MI.CurrTime:=0 ELSE
        IF (lParam1 AND MCI_Seek_To_End<>0)   THEN MI.CurrTime:=MI.TotalTime-200 ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
        IF MI.Playing AND NOT(IsPause) THEN EndPause(False);
      END;
    MCI_PLAY:IF (dwDriverID<>$1234) THEN DriverProc:=mcierr_NONAPPLICABLE_Function ELSE
      BEGIN
        IF MI.Playing THEN BeginPause(False);
        SeekToTime:=Mi.CurrTime;
        PlayEndTime:=Mi.TotalTime;
        IF Pointer(lParam2)<>NIL THEN
        BEGIN
          IF lParam1 AND MCI_FROM<>0 THEN SeekToTime:=pMCI_PLAY_PARMS(lParam2)^.dwFrom;
          IF lParam1 AND MCI_TO<>0   THEN PlayEndTime:=pMCI_PLAY_PARMS(lParam2)^.dwTo
        END;
        i:=0;
        IF MI.Playing THEN
        BEGIN
          Mi.CurrTime:=SeekToTime;
          EndPause(False);
        END ELSE
        BEGIN
          GetSettings;
          i:=Play(1,FileName,NIL,False,@Settings,True,False);
        END;
        CASE i OF
          0:BEGIN
              IsPause:=False;
              Mi.CurrTime:=SeekToTime;
              EndPause(False);
              IF (lParam1 AND MCI_WAIT<>0) THEN
              BEGIN
                WHILE MI.Playing DO IF mciDriverYield(dwDriverID) THEN
                BEGIN
                  IF (lParam1 AND MCI_NOTIFY<>0)AND(Pointer(lParam2)<>NIL)
                  THEN mciDriverNotify(pMCI_GENERIC_PARAMS(lParam2)^.dwCallback, dwDriverID, MCI_NOTIFY_ABORTED);
                  lParam1:=lParam1 AND (NOT MCI_NOTIFY);
                  Break;
                END;
              END ELSE
              IF (lParam1 AND MCI_NOTIFY<>0)AND(Pointer(lParam2)<>NIL) THEN
              BEGIN
                IF MCITID<>0 THEN KillTimer(0,MCITID);
                MCITID:=SetTimer(0,4444,55,@MCITimerProc);
                TaskDriverID:=dwDriverID;
                TaskCallback:=pMCI_GENERIC_PARAMS(lParam2)^.dwCallBack;
                lParam1:=lParam1 AND (NOT MCI_NOTIFY);
              END;
              {$IFDEF DEMO}
              IF DlgWnd<>0 THEN
              BEGIN
                EndDialog(DlgWnd,0);
                DlgWnd:=0;
                FirstPlayed:=True;
              END;
              IF FirstPlayed AND (GlobalFindAtom('M4W_MCI_NOT_NAGGED'#0)=0)THEN
              BEGIN
                DlgWnd:=CreateDialog(hInstance,'ABOUT',0,@DialogProc1);
                IF DlgWnd=0 THEN Halt(0);
                FirstPlayed:=False;
              END;
              {$ENDIF}
            END;
          10000..11000     :DriverProc:=MCIERR_Wave_OutputsInUse;
          id_NoMem         :DriverProc:=MCIERR_Out_Of_Memory;
          id_FileNotFound  :DriverProc:=MCIERR_File_Not_Found;
          id_NoModFile     :DriverProc:=MCIERR_Invalid_File;
          id_FileCorrupted :DriverProc:=MCIERR_Invalid_File;
          id_NixFile       :DriverProc:=MCIERR_File_Not_Found;
          id_InvModFile    :DriverProc:=MCIERR_Invalid_File;
          id_FileUnsupport :DriverProc:=MCIERR_Invalid_File;
          ELSE              DriverProc:=MCIERR_Device_Not_Ready;
        END;
      END;
    MCI_GETDEVCAPS:
      IF lParam1 AND MCI_GETDEVCAPS_ITEM<>0 THEN
      BEGIN
        DriverProc:=MCI_RESOURCE_RETURNED;
        IF Pointer(lParam2)<>NIL THEN
        CASE PMCI_GetDevCaps_Parms(lParam2)^.dwItem OF
          MCI_GETDEVCAPS_COMPOUND_DEVICE,
          MCI_GETDEVCAPS_USES_FILES,
          MCI_GETDEVCAPS_HAS_AUDIO,
          MCI_GETDEVCAPS_CAN_PLAY    :
            PMCI_GetDevCaps_Parms(lParam2)^.dwReturn:=MakeMCIResource(1,MCI_True);
          MCI_GETDEVCAPS_DEVICE_TYPE :
            PMCI_GetDevCaps_Parms(lParam2)^.dwReturn:=MakeMCIResource(mci_DevType_Sequencer,mci_DevType_Sequencer);
          MCI_GETDEVCAPS_CAN_RECORD,
          MCI_GETDEVCAPS_CAN_SAVE,
          MCI_GETDEVCAPS_CAN_EJECT,
          MCI_GETDEVCAPS_HAS_VIDEO:
            PMCI_GetDevCaps_Parms(lParam2)^.dwReturn:=MakeMCIResource(0,MCI_FALSE);
          ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
        END ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
      END ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
    MCI_INFO:
      BEGIN
        IF (pMCI_INFO_PARMS(lParam2)<>NIL)
        AND(pMCI_INFO_PARMS(lParam2)^.lpstrReturn<>NIL)
        AND(pMCI_INFO_PARMS(lParam2)^.dwRetSize>0)THEN
        BEGIN
          
          IF (lParam1 AND MCI_INFO_File<>0)
          THEN StrLCopy(pMCI_INFO_PARMS(lParam2)^.lpstrReturn,pChar(@Mi.MODName[1]), 
                        Min(Length(Mi.MODName),pMCI_INFO_PARMS(lParam2)^.dwRetSize-1))
          ELSE IF (lParam1 AND MCI_INFO_PRODUCT<>0)
            {$IFDEF RJM4W}
            THEN StrLCopy(pMCI_INFO_PARMS(lParam2)^.lpstrReturn,'RJs MOD4WIN MCI Driver'#0,
                          pMCI_INFO_PARMS(lParam2)^.dwRetSize-1)
            {$ELSE RJM4W}
            THEN StrLCopy(pMCI_INFO_PARMS(lParam2)^.lpstrReturn,'MOD4WIN MCI Driver'#0,
                          pMCI_INFO_PARMS(lParam2)^.dwRetSize-1)
            {$ENDIF RJM4W}
            ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
        END ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
      END;
    MCI_SET:
      BEGIN
        IF lParam1 AND MCI_SET_TIME_FORMAT<>0 THEN
        BEGIN
          IF (Pointer(lParam2)=NIL)OR(pMCI_SET_PARMS(lParam2)^.dwTimeFormat<>MCI_FORMAT_MILLISECONDS)
          THEN DriverProc:=mcierr_Flags_Not_Compatible;
        END ELSE
        IF (lParam1 AND MCI_SET_AUDIO<>0)
        OR (lParam1 AND MCI_SET_DOOR_CLOSED<>0)
        OR (lParam1 AND MCI_SET_DOOR_OPEN<>0)
        OR (lParam1 AND MCI_SET_VIDEO<>0)
        OR (lParam1 AND MCI_SET_ON<>0)
        OR (lParam1 AND MCI_SET_OFF<>0)
        THEN DriverProc:=MCIERR_UNSUPPORTED_FUNCTION
        ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
      END;
    MCI_STATUS:
      IF lParam1 AND MCI_STATUS_ITEM<>0 THEN
      BEGIN
        IF Pointer(lParam2)<>NIL THEN
        CASE pMCI_STATUS_PARMS(lParam2)^.dwItem OF
          MCI_STATUS_POSITION:
            IF (lParam1 AND MCI_TRACK<>0)
            THEN IF (pMCI_STATUS_PARMS(lParam2)^.dwTrack=1)
                 THEN pMCI_STATUS_PARMS(lParam2)^.dwReturn:=0
                 ELSE DriverProc:=MCIERR_OUTOFRANGE
            ELSE IF lParam1 AND MCI_STATUS_START<>0
                 THEN pMCI_STATUS_PARMS(lParam2)^.dwReturn:=0
                 ELSE pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MI.CurrTime;
          MCI_STATUS_LENGTH:
            IF (lParam1 AND MCI_TRACK<>0)AND(pMCI_STATUS_PARMS(lParam2)^.dwTrack<>1)
            THEN DriverProc:=MCIERR_OUTOFRANGE
            ELSE pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MI.TotalTime;
          MCI_STATUS_CURRENT_TRACK,
          MCI_STATUS_NUMBER_OF_TRACKS: pMCI_STATUS_PARMS(lParam2)^.dwReturn:=1;
          MCI_STATUS_READY: pMCI_STATUS_PARMS(lParam2)^.dwReturn:=1;
          MCI_STATUS_MEDIA_PRESENT:
            IF MI.CurrTime<>0
            THEN pMCI_STATUS_PARMS(lParam2)^.dwReturn:=1
            ELSE pMCI_STATUS_PARMS(lParam2)^.dwReturn:=0;
          MCI_STATUS_MODE:
            BEGIN
              DriverProc:=MCI_RESOURCE_RETURNED;
              IF NOT MI.Playing
              THEN pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MakeMCIResource(MCI_MODE_STOP,MCI_MODE_STOP)
              ELSE IF IsPause
                THEN pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MakeMCIResource(MCI_MODE_PAUSE,MCI_MODE_PAUSE)
                ELSE pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MakeMCIResource(MCI_MODE_PLAY,MCI_MODE_PLAY);
            END;
          MCI_STATUS_TIME_FORMAT:
            BEGIN
              pMCI_STATUS_PARMS(lParam2)^.dwReturn:=MakeMCIResource(MCI_FORMAT_MILLISECONDS,MCI_FORMAT_MILLISECONDS_S);
            END;
          ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
        END ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
      END ELSE DriverProc:=MCIERR_MISSING_PARAMETER;
    mci_Where,
    mci_Cue,
    MCI_Window,
    MCI_LOAD,
    MCI_RECORD,
    MCI_SAVE:   DriverProc:=MCIERR_UNSUPPORTED_FUNCTION;
    ELSE
    BEGIN
      DriverProc:=DefDriverProc(dwDriverID, hDriver, wMsg, lParam1, lParam2);
    END;
  END;
  IF (wMsg>=DRV_MCI_First)AND(wMsg<=DRV_MCI_Last)AND((lParam1 AND MCI_NOTIFY)<>0)AND(Pointer(lParam2)<>NIL)
  THEN mciDriverNotify(pMCI_GENERIC_PARAMS(lParam2)^.dwCallback, dwDriverID, MCI_NOTIFY_SUCCESSFUL);
END;

(**
FUNCTION DriverProc(dwDriverID:Longint;hDriver,wMsg:Word;lParam1,lParam2:Longint):Longint;EXPORT;ASSEMBLER;
VAR Stack:Pointer;
ASM
  push gptr
  mov ax,1
  DB 66h; shl ax,16
  DB 66h; push ax
  call GlobAlallocPtr
  mov word ptr stack+0,ax
  mov word ptr stack+2,dx
  DB 66h; mov ax,word ptr dwDriverID
  DB 66h; mov bx,word ptr wMsg
  DB 66h; mov cx,word ptr lParam1
  DB 66h; mov dx,word ptr lParam2
  mov di,sp
  mov si,ss
  mov ss,word ptr Stack+2
  mov sp,65532
  push si
  push di{ }
  DB 66h; push ax
  DB 66h; push bx
  DB 66h; push cx
  DB 66h; push dx
  call _DriverProc
  pop bx
  pop ss
  mov sp,bx{ }
  DB 66h; push word ptr Stack
  mov word ptr stack+0,ax
  mov word ptr stack+2,dx
  call GlobalFreePtr
  mov ax,word ptr stack+0
  mov dx,word ptr stack+2
END;
(**)

{$ENDIF}

{****************************** END OF MCI *************************************}

{$IFOPT S+} {$DEFINE StackCheck} {$S-} {$ENDIF}
VAR OrgExitProc:Pointer;
PROCEDURE ExitDll; FAR;
BEGIN
  ExitProc:=OrgExitProc;
  IF NOT(Ende)THEN STOP;
  {$IFDEF MCI}
  IF DlgWnd<>0 THEN
  BEGIN
    EndDialog(DlgWnd,0);
    DlgWnd:=0;
  END;
  StopMCITimer;
  {$ENDIF}
  IF WriteTID<>0 THEN TimeKillEvent(WriteTID);
  timeEndPeriod (1);
  FreeProcInstance(VxDTimerProc);
  IF CodeSel<>0 THEN
  BEGIN
    GlobalPageUnLock(CodeSel);
    FreeSelector(CodeSel);
  END;
  IF CodeSel1<>0 THEN
  BEGIN
    GlobalPageUnLock(CodeSel1);
    FreeSelector(CodeSel1);
  END;
  CodeSel:=0;
  { }
  ASM
    push ds
    call GlobalPageUnLock
  END;
  { }
END;

FUNCTION CriticalExit:Pointer; EXPORT;
BEGIN
  Ende:=True;
  IF InCritical THEN CriticalExit:=@CatchBuf ELSE
  BEGIN
    CriticalExit:=NIL;
    ExitProc:=OrgExitProc;
    IF WriteTID<>0 THEN TimeKillEvent(WriteTID);
    WriteTID:=0;
    timeEndPeriod (1);
    STOP;
    {$IFDEF MCI}
    IF DlgWnd<>0 THEN
    BEGIN
      EndDialog(DlgWnd,0);
      DlgWnd:=0;
    END;
    StopMCITimer;
    {$ENDIF}
    FreeProcInstance(VxDTimerProc);
    IF CodeSel<>0 THEN
    BEGIN
      GlobalPageUnLock(CodeSel);
      FreeSelector(CodeSel);
    END;
    IF CodeSel1<>0 THEN
    BEGIN
      GlobalPageUnLock(CodeSel1);
      FreeSelector(CodeSel1);
    END;
    CodeSel:=0;
    { }
    ASM
      push ds
      call GlobalPageUnLock
    END;
    { }
  END;
END;

{$IFDEF StackCheck} {$S+} {$ENDIF}

EXPORTS
   Stop              index 1,
   Play              index 2,
   BeginPause        index 3,
   EndPause          index 4,
   GetTimePattern    index 5,
   SettingsChanged   index 6,
   HandleRepeat      index 7,
   GetDataPtr        index 8,
   EnableChn         index 9,
   IsEnabled         index 10,
   CanPlay           index 11,
   PatternSearch     index 12,
   RepeatPattern     index 13,
   GetVersion        index 14,
   EnableAllChn      index 15,
   CriticalExit      index 16,
   AppTaskProc       index 99,
   {$IFDEF MCI}
   MCITimerproc      index 102,
   DriverProc        index 103,
   DialogProc        index 104,
   {$ENDIF}
   WaveOutCallBack   index 10000,
   MMTimerproc       index 10001;


BEGIN
  {$IFDEF MCI}
  Settings:=DefDACSetting;
  {$ENDIF}
  ASM
    push ds
    call GlobalPageLock
  END;
  AssignPeakFile;
  VxDTimerProc :=MakeProcInstance(@VVxDTimerProc,hInstance);
  PlayTimes    :=NIL;
  OrgExitProc  :=ExitProc;
  ExitProc     :=@ExitDLL;
  InCritical   :=False;
  PeakSearch   :=GetProfileInt('Player32','PeakSearch',1)<>0;
  {$IFDEF MCI}
  LoadProgress := GetProfileInt ('Player32', 'LoadProgress', 0) <> 0;
  {$ELSE MCI}
  LoadProgress := True;
  {$ENDIF MCI}
  SearchPattern:=False;
  PitchTable[0]:=0;
  Ende         :=true;
  PlayMemSel   :=0;
  SongMemSel   :=0;
  initspeed    :=6;
  initbpmspeed :=125;
  FillChar(MI,SizeOf(tModulInfo),0);
  Randomize;
  FOR I:=1   TO 145 DO PitchTable[i]:=Round(54784.0*Exp(ln(2)*((1.0-i)/12)));
  FOR i:=0   TO 255 DO sintable[i] :=Trunc(-128*Sin(Pi*i/128)-0.5);
  FOR i:=0   TO 255 DO RampUptable[i]:=127-i;
  FOR i:=0   TO 255 DO RampDntable[i]:=i-128;
  FOR i:=0   TO 127 DO Sqrtable[i] :=-128;
  FOR i:=128 TO 255 DO Sqrtable[i] :=127;
  FillChar(DisAbled,SizeOf(DisAbled),0);
  FirstFilePos:=0;
  First:=NIL;
  FT2Instruments:=NIL;
  FT2Samples:=NIL;
  RecordFile:=INVALID_HANDLE_VALUE;
  timeBeginPeriod (1);
  MergePeakFiles;
END.
