{$N+,E-}
{ $DEFINE MCI}
{$DEFINE HUGEMEM}

UNIT PlayDefs;
{$C Fixed Preload Permanent}
INTERFACE
USES WinTypes,WinProcs,MODC,MMSystem,W32DEFS,Win31{$IFDEF HUGEMEM},HugeMem,GetOS{$ENDIF};

CONST SurrBufferLen          = 8192;
      SurrLen                = 15;
      fPAL                   = 7093789.2;
      fNTSC                  = 7159090.5;
      DefMODTuning:Longint   = Trunc(fNTSC/2/428);
      UsePattBrkOps:Bool     = True;
      MaxPlayBuffer          = 162;
      NumEffTracks:Word      = 0;
      MaxLevel:Longint       = 0;
      PeakSearch:Boolean     = False;
      PeakHold:Boolean       = False;
      ShowPeakSearch:Boolean = False;
      LoadProgress:Boolean   = False;
      OnlyOnePattern:Boolean = False;
      OneBy1024              :single=1.0/1024.0;
      OneBy65536             :single=1.0/65536.0;
      OneBy4                 :single=1.0/4.0;
      {$IFDEF MCI}
      PlayEndTime:Longint    = 0;
      {$ENDIF MCI}
VAR   PeakFile:FILE;
      {$IFNDEF MCI}
      LocalPeakFile:FILE;
      {$ENDIF MCI}
var IWFound: boolean;


{*********************Typen zum Abspielen von MOD-Files*****************************}
TYPE tVolTable=ARRAY[0..64,0..255]OF Integer;

TYPE tDIS=ARRAY[1..32]OF BOOLEAN;

VAR  PitchTable:ARRAY[0..145]OF Word;
     DisAbled  :tDIS;
     Dis_BAK   :tDIS;

CONST
   TuneTable:ARRAY[0..15] OF Longint=
    (Trunc(65536.0*(436.4/436.4)),
     Trunc(65536.0*(436.4/439.0)),
     Trunc(65536.0*(436.4/441.6)),
     Trunc(65536.0*(436.4/445.1)),
     Trunc(65536.0*(436.4/447.8)),
     Trunc(65536.0*(436.4/451.5)),
     Trunc(65536.0*(436.4/455.2)),
     Trunc(65536.0*(436.4/457.0)),
     Trunc(65536.0*(436.4/412.0)),
     Trunc(65536.0*(436.4/414.4)),
     Trunc(65536.0*(436.4/416.7)),
     Trunc(65536.0*(436.4/419.9)),
     Trunc(65536.0*(436.4/423.1)),
     Trunc(65536.0*(436.4/426.3)),
     Trunc(65536.0*(436.4/429.6)),
     Trunc(65536.0*(436.4/432.1)));{}

TYPE
   TPanArray = ARRAY[1..32]OF Byte;

   TNote       = RECORD
                   Instrument:Byte;
                   Ton       :Word;
                   Effekt    :Byte;
                   Operands  :Byte;
                   ChnVol    :Byte;
                 END;

   pTrack      = ^tTrack;
   TTrack      = ARRAY[0..255] OF tnote;

   pPattern    = ^tPattern;
   TPattern    = ARRAY[1..32] OF TTrack;

   tOrderData  = ARRAY[1..32] OF Longint;
   pTrackSeq   = ^TTrackSeq;
   TTrackSeq   = ARRAY[0..255]OF tOrderData;

   pMixChannel = ^tMixChannel;
   tMixChannel = RECORD
                   note   :longint; { Aktueller Sampleoffset                     }
                   iend   :longint; { Länge der Instrumente in Bytes             }
                   LLn    :longint; { Loopstart der Instrumente in Bytes         }
                   lend   :longint; { Looplänge der Instrumente in Bytes         }
                   Rest   :longint; { Übertrag der Nachkommastellen              }
                   Plo    :word;    { Nachkommastellen für Sampleincrement       }
                   Phi    :longint; { Vorkommastellen für Sampleincrement        }
                   MixBuf :Longint; { Wohin soll gemixt werden?                  }
                   lVol   :byte;    { Linke Lautstärke der Instrumente           }
                   rVol   :byte;    { rechte Lautstärke der Instrumente          }
                   avgVol :byte;    { mittlere Lautstärke der Instrumente        }
                   JmpFlags:Word;   {Für Mixing...}
                   Dummy:Longint;
                 END;

   TMODEvent   = RECORD Inst:word; Offset:Longint; Pitch:Longint; lVolume,rVolume:Word; END;
   pChannel    = ^tChannel;
   tChannel    = RECORD
                   note   :longint; { Aktueller Sampleoffset                     }
                   iend   :longint; { Länge der Instrumente in Bytes             }
                   LLn    :longint; { Loopstart der Instrumente in Bytes         }
                   lend   :longint; { Looplänge der Instrumente in Bytes         }
                   Rest   :longint; { Übertrag der Nachkommastellen              }
                   Plo    :word;    { Nachkommastellen für Sampleincrement       }
                   Phi    :longint; { Vorkommastellen für Sampleincrement        }
                   MixBuf :Longint; { Wohin soll gemixt werden?                  }
                   lVol   :byte;    { Linke Lautstärke der Instrumente           }
                   rVol   :byte;    { rechte Lautstärke der Instrumente          }
                   avgVol :byte;    { mittlere Lautstärke der Instrumente        }
                   JmpFlags:Word;   {Für Mixing...}
                   Vol    :byte;    { Lautstärke der Instrumente                 }
                   PanVal :byte;
                   Tonh   :word;
                   NewTonh:Word;
                   NewFineTune:Byte;
                   NoteVal:Byte;
                   Effkt  :byte;
                   Opnd   :byte;
                   Inst   :byte;
                   NxtInst:byte;
                   tpton  :word;
                   TPGlTon:word;
                   Volsld :byte;
                   VTini  :byte;
                   Acnt   :Byte;
                   {more instrument stuff}
                   SmpNum :Word;
                   ITune  :Longint;
                   IFinTun:Byte;
                   IOffset:Longint;
                   { effect parameters }
                   VOps   :byte; {merker für Vibrato}
                   TOps   :byte; {merker für Tremolo}
                   PrtOps :byte; {merker für Portamento Up/Down}
                   PrtNOps:byte; {merker für Portamento to Note}
                   OfsOps :Byte; {merker für SetOffset}
                   ArpOps :Byte; {merker für Arpeggio}
                   VSldOps:Byte; {merker für Volume Slide}
                   {Pro...}
                   fVuOps :Byte; {merker für fine Volume up}
                   fVdOps :Byte; {merker für fine Volume down}
                   PdelOps:Byte; {merker für Pattern delay}
                   RetrOps:Byte; {merker für Retrigger}
                   patternloopst :byte;
                   patternloops  :byte;
                   { local variables }
                   VTable,TTable :Word;
                   GlissandoOn   :Boolean;
                   {Oktalyzer}
                   Arp1Ops:Byte;  {merker für Oktalyzer Arpeggio 1}
                   Arp2Ops:Byte;  {merker für Oktalyzer Arpeggio 2}
                   OktSuOps :Byte; {merker für Oktalyzer Slide up}
                   OktSu1Ops:Byte; {merker für Oktalyzer Slide up once}
                   OktSdOps :Byte; {merker für Oktalyzer Slide down}
                   OktSd1Ops:Byte; {merker für Oktalyzer Slide down once}
                   OldVolume:Byte; {merker für Oktalyzer old volume}
                   {Tremor}
                   TVol     :Byte;
                   TremorOps:Byte; {merker für Tremor}
                   {VolumePortamento}
                   VolPrt   :Byte;
                   VPVBlanc :Byte;
                   {FarPortamento}
                   FPVBlanc :Byte;
                   FPTonh   :Word;
                   {StereoControl}
                   Scont    :Byte;
                   {FT2 Stuff}
                   OldChnVol:Byte;
                   gVSldOps :Byte; {merker für Globalvolume Slide}
                   VSldUpOps:Byte; {Volume Slide Up}
                   VSldDnOps:Byte; {Volume Slide Down}
                   fVSldUpOps:Byte; {fine Volume Slide Up}
                   fVSldDnOps:Byte; {fine Volume Slide Down}
                   PSldOps  :Byte; {merker für Panning Slide}
                   PSldLOps  :Byte; {merker für Panning Slide Left}
                   PSldROps  :Byte; {merker für Panning Slide Right}
                   {Envelopes}
                   EnvVolPos:Byte; {Current Envelope Position}
                   EnvVolTck:Word;
                   EnvVol   :Byte;
                   EnvPanPos:Byte; {Current Envelope Position}
                   EnvPanTck:Word;
                   EnvPan   :Byte;
                   VibTck   :Word;
                   VibPtr   :Byte;
                   VibTbl   :Word;
                   FadeVol  :Longint;
                   DoFadeOut:Boolean;
                   { hardware stuff }
                   BackEChg,
                   EventChg :SET OF (EInst, EPitch, EVol, EReset);
                   PlayChannel : shortint;
                   BackChannel : shortint;
                   MODEvent    : tMODEvent;
                   BackEvent   : tMODEvent;
                 END;

   pChannels   = ^tChannels;
   tChannels   = ARRAY [1..32] OF tChannel;

   pMixChannels= ^tMixChannels;
   tMixChannels= ARRAY [1..32] OF tMixChannel;

   pPlayTime   = ^tPlaytime;
   tPlayTime   = RECORD
                    Time       :Longint;       { 4}
                    BytePos    :Longint;       {+4}
                    Pattrn     :Word;          {+2}
                    Effects    :EffectState;   {+288 (32 Channels*Int(68 FX / 8 Bits/Byte))}
                    Volumes    :tByteState;    {+32}
                    Notes      :tByteState;    {+32}
                    PlayInstr  :tByteState;    {+32}
                    BPM        :Word;          {+2}
                    SPD        :Word;          {+2}
                    PlayLine   :Word;          {+2}
                  END;                         { =400 Bytes}
   pPlayTimes  = ^tPlaytimes;
   tPlayTimes  = ARRAY[0..MaxPlayBuffer]OF tPlayTime;
TYPE
   TChannelMap = SET OF 1..32;

{****************Typen zum lesen von MOD-FIles*************************************}
   TFileNote   = RECORD Byte1,Byte2,Byte3,Byte4:BYTE; END; { Note im MOD-File }

   TArrangement= ARRAY[0..127] OF Byte;

   TFileInstrument = RECORD
                  SampName : ARRAY[0..21] OF CHAR;
                  SampLen  : WORD;
                  SampTune : Shortint;
                  SampVol  : BYTE;
                  SampRepS : WORD;
                  SampRepL : WORD;
                END;

   pMODHeader = ^tMODHeader;
   TMODHeader = RECORD
                  MODName  : ARRAY[0..19] OF CHAR;
                  InstrBank: ARRAY[1..31] OF TFileInstrument;
                  ArrangeSize:BYTE;
                  CIAASpeed: BYTE;
                  Arrangement:TArrangement;
                  MODSign  : ARRAY[1..4] OF CHAR;
                END;

   p15MODHeader = ^t15MODHeader;
   T15MODHeader = RECORD
                  MODName     : ARRAY[0..19] OF CHAR;
                  InstrBank   : ARRAY[1..15] OF TFileInstrument;
                  ArrangeSize : BYTE;
                  CIAASpeed   : BYTE;
                  Arrangement : TArrangement;
                END;
{*********MTM**********************************************************************}
   TMTMFileNote = RECORD Byte1,Byte2,Byte3:BYTE; END; { Note im MOD-File }

   pMTMHeader = ^tMTMHeader;
   TMTMHeader  = RECORD
                  Sign        : ARRAY[0..2]OF Char;
                  Version     : Byte;
                  MTMName     : ARRAY[0..19]OF Char;
                  SavedTracks : Word;
                  lastPattern : Byte;
                  lastToPlay  : Byte;
                  CommentLen  : Word;
                  NumSamples  : Byte;
                  Attribute   : Byte; {=0}
                  BeatPerTrck : Byte; {=64}
                  NTracks     : Byte; {1..32}
                  PanPostns   : ARRAY[1..32]OF Byte;
                END;

   tMTMInstrument = RECORD
                      SampName : ARRAY[0..21] OF CHAR;
                      SampLen  : Longint;
                      SampRepS : Longint;
                      SampRepE : Longint;
                      SampTune : Shortint;
                      SampVol  : BYTE;
                      SampAttr : Byte; {Bit0=0..8Bit; Bit0=1..16Bit}
                    END;
   pMTMInstrBank = ^tMTMInstrBank;
   tMTMInstrBank = ARRAY[1..256]OF tMTMInstrument;

   tMTMOrderData  = ARRAY[1..32] OF Word;
   pMTMTrackSeq   = ^TMTMTrackSeq;
   TMTMTrackSeq   = ARRAY[0..255]OF tMTMOrderData;

   tMTMArrangement = ARRAY[0..127] OF byte;
{***********************STM*******************************************************}
 TYPE stminstrument=RECORD{32 Bytes}
       sampname    :ARRAY[0..11] OF char;
       STMInstSign,
       STMInstDisk :byte;
       SMEMOFS     :word; {*16=Filepos}
       SampLen     :word;
       sampreps    :word;
       samprepend  :word;
       Sampvol,
       reserved    :byte;
       NAdjust     :word;
       fill2       :ARRAY[0..5] OF byte;
     END;

TYPE stmarrangement=ARRAY[0..127] OF Byte;

TYPE PSTMHeader=^TSTMHeader;
     Tstmheader=RECORD
       {   0d   0h} StmName    :ARRAY[0..19] OF char;
       {  20d  14h} StmSign    :ARRAY[0..7]  OF char;
                    STMSign2   :char;
                    FileType,
                    VerMajor,
                    VerMinor   :byte;
       {  32d  20h} songtempo  :Byte;
       {  33d  21h} numpatts   :Byte;
       {  34d  22h} SongVol    :Byte;
       {  35d  23h} fill2      :ARRAY[0..12]OF Byte;
       {  48d  30h} instrBank  :ARRAY[1..31]OF stminstrument;
       {1040d 410h} arrangement:stmarrangement;
       {1. Pattern: 1168(490h)...2192(890h) usw.}
     END;

VAR stmsamplememofs:ARRAY[1..31] OF word;

{*****************669 Format******************************************************}
{669 hat fixe Pattern-Größe --> 8 Ch. * 64 Zeilen}
TYPE   P669Header=^t669Header;
       T669Header =RECORD
         f669sign    : WORD;
         Comment     : ARRAY[1..3, 1..36] OF CHAR; {108 CHARS}
         NInstruments: BYTE;                       {0..64 'SAVED'}
         numpatts    : BYTE;                       {0..128 'SAVED'}
         RepStart    : BYTE;                       {LoopStartPattern}
         arrangement : ARRAY[0..127] OF BYTE;
         Tempos      : ARRAY[0..127] OF BYTE;     {Pattern Tempos}
         Lengths     : ARRAY[0..127] OF BYTE;     {Break Location List for Patterns}
       END;
TYPE   T669Instrument =                           {middle C bei 8740 Hz}
       RECORD
         sampName  : ARRAY[1..13] OF CHAR;
         Samplen   : LONGINT;
         sampreps  : LONGINT;
         samprepend: LONGINT;
       END;

TYPE P669insarray=^t669insarray;
     t669insarray=RECORD
       instr       :ARRAY[1..1] OF t669instrument;
     END;

TYPE t669Note=RECORD
       byte1,byte2,byte3 : BYTE;
     END;

{******************************* FAR format specs by Jensi *******************************}
TYPE PFarHeader = ^TFarHeader;
     TFarHeader = RECORD
       FarSign   : ARRAY[0.. 3] OF char;
       FarName   : ARRAY[0..39] OF char;
       FarSign2  : ARRAY[0.. 2] OF char;
       HeaderLen : word;
       Version   : byte;
       ChannelMap: ARRAY[1..16] OF boolean;
       editdata1 : ARRAY[1.. 9] OF byte;
       DefTempo  : byte;
       PanningMap: ARRAY[1..16] OF byte;
       editdata2 : ARRAY[1.. 4] OF byte;
       CommentLen: word;
     END;

     PFarHeader2 = ^TFarHeader2;
     TFarHeader2 = RECORD
       Orders    : ARRAY[1..256] OF byte;
       NumPatts,
       OrdLength,
       LoopToLoc : byte;
       PatLength : ARRAY[0..255] OF word;
     END;

     TFarNote = RECORD b1, b2, b3, b4: byte END;
     PFarPatternData = ^TFarPatternData;
     TFarPatternData = RECORD
       BreakLoc,
       PtrnTempo : byte;
       PtrnData  : ARRAY [0..255, 1..16] OF TFarNote;
     END;

     TFarSampleMap = SET OF 0..63;

     PFarInstrument = ^TFarInstrument;
     TFarInstrument = RECORD
       Name      : ARRAY[0..31] OF char;
       Length    : longint;
       FineTune,
       Volume    : byte;
       RepeatStart,
       RepeatEnd : longint;
       SampleType,
       LoopMode  : byte;
     END;

VAR  FarSampleMap : TFarSampleMap;
     HeaderLength : word;

{******************************* S3M format specs by Jensi *******************************}
TYPE PS3MHeader=^TS3MHeader;
     TS3MHeader=RECORD
       S3MName   : ARRAY[0..27] OF char;
       S3MSign1  : char;
       FileType  : byte;
       unused1   : ARRAY[1..2] OF byte;
       OrdNum,
       InsNum,
       PatNum,
       Flags,
       Cwt_V,
       Ffv       : word;
       S3MSign2  : ARRAY[0..3] OF char;
       glob_vol,
       init_speed,
       init_tempo,
       mast_vol,
       ult_click,
       def_panpos: byte;
       unused2   : ARRAY[1..8] OF byte;
       special   : word;
       channels  : ARRAY[1..32] OF byte;
     END;

     POrders = ^TOrders;
     TOrders = ARRAY [1..1] OF byte;

     PInstruments = ^TInstruments;
     TInstruments = ARRAY [1..1] OF word;

     PPatterns = ^TPatterns;
     TPatterns = ARRAY [0..0] OF word;

     PPanPos = ^TPanPos;
     TPanPos = ARRAY [1..1] OF byte;

     TS3MNote = RECORD b0, b1, b2, b3, b4, b5: byte END;
     PS3MPatternData = ^TS3MPatternData;
     TS3MPatternData = RECORD
       Length    : word;
       PackedData: ARRAY [1..32 * 64 * SizeOf(TS3MNote)] OF byte;
     END;

TYPE TSampleType = (unused, sample, amel, abd, asnare, atom, acym, ahihat);
     PS3MSample = ^TS3MSample;
     TS3MSample = RECORD
       SampleType: TSampleType;
       DOSName   : ARRAY[0..11] OF char;
       MemSeg    : RECORD b1: byte; w1: word END;
       Length,
       LoopBeg,
       LoopEnd   : longint;
       Volume,
       unused1,
       Pack,
       Flags     : byte;
       C2_Speed,
       unused2   : longint;
       internal1,
       internal2 : word;
       internal3 : longint;
       SampleName: ARRAY[0..27] OF char;
       SampleSign: ARRAY[0..3] OF char;
     END;

VAR  S3MSample:     PS3MSample;

{******************************* ULT format specs by Jensi *******************************}
TYPE PULTHeader = ^TULTHeader;
     TULTHeader = RECORD
       ULTSign1:   ARRAY[0..11] OF char;
       ULTSign2:   ARRAY[0..2]  OF char;
       ULTName:    ARRAY[0..31] OF char;
       CommentLen: byte;                      { v. 1.4 and up }
     END;

     PSampleStruc = ^TSampleStruc;
     TSampleStruc = RECORD
       SampleName: ARRAY[0..31] OF char;
       DOSName   : ARRAY[0..11] OF char;
       LoopStart,
       LoopEnd,
       SizeStart,
       SizeEnd   : longint;
       Volume,
       BidiLoop  : byte;
       FineTune  : word;
     END;

     PSampleStruc2 = ^TSampleStruc2;
     TSampleStruc2 = RECORD
       SampleName: ARRAY[0..31] OF char;
       DOSName   : ARRAY[0..11] OF char;
       LoopStart,
       LoopEnd,
       SizeStart,
       SizeEnd   : longint;
       Volume,
       BidiLoop  : byte;
       FineTune  : word;
       C2_Frequ:   word;                      { v. 1.6 and up }
     END;

     PULTHeader2 = ^TULTHeader2;
     TULTHeader2 = RECORD
       {NumSamples: byte;}
       CASE byte OF
       1: (Samples: ARRAY[1..1] OF TSampleStruc);
       2: (Sampls2: ARRAY[1..1] OF TSampleStruc2);
     END;

     PULTHeader3 = ^TULTHeader3;
     TULTHeader3 = RECORD
       PatternSequence: ARRAY[1..256] OF byte;
       NumChannels:     byte;
       NumPatterns:     byte;
       {PanPositions:    array[1..1] of byte;}  { v. 1.5 and up }
     END;

VAR  FormatVersion: byte;

{******************************* DMF format specs by Jensi *******************************}
TYPE PDMFHeader = ^TDMFHeader;
     TDMFHeader = RECORD
       DMFSign: ARRAY[0..3] OF char;
       Version: byte;
       Tracker: ARRAY[0..7] OF char;
       DMFName: ARRAY[0..29] OF char;
       CompNam: ARRAY[0..19] OF char;
       Date:    ARRAY[1..3] OF byte;
     END;

     PDMFCompMessage = ^TDMFCompMessage;
     TDMFCompMessage = RECORD
       Sign:    ARRAY[0..3] OF char;
       MsgSize: longint;
     END;

{******************************* FT2 format specs by Jensi *******************************}
TYPE PFT2Header = ^TFT2Header;
     TFT2Header = RECORD
       FT2Sign:    ARRAY[0..16] OF char;
       FT2Name:    ARRAY[0..19] OF char;
       FT2Sign2:   char;
       TrackNam:   ARRAY[0..19] OF char;
       Version:    word;
       HeaderLen:  longint;
       SongLen,
       RestartPos,
       NumChans,
       NumPatts,
       NumInstrs,
       Flags,
       DefTempo,
       DefBPM:     word;
       OrderTable: ARRAY[0..255] OF byte;
     END;

     PFT2Pattern = ^TFT2Pattern;
     TFT2Pattern = RECORD
       HeaderLen:  longint;
       PackType:   byte;
       NumRows:    word;
       PackedSize: word;
     END;

     PFT2Instrument = ^TFT2Instrument;
     TFT2Instrument = RECORD
       InstrSize:  longint;
       InstrName:  ARRAY[0..21] OF char;
       InstrType:  byte;
       NumSamples: word;
     END;

     tEnvPts=ARRAY [1..12] OF RECORD Tick,Value:Word; END;

     PFT2Instrument2 = ^TFT2Instrument2;
     TFT2Instrument2 = RECORD
       SampleHead: longint;
       SampleNums: ARRAY[1..96] OF byte;
       VolEnvPts,
       PanEnvPts:  tEnvPts;
       NumVolPts,
       NumPanPts,
       VolSustPt,
       VolLoopSt,
       VolLoopEnd,
       PanSustPt,
       PanLoopSt,
       PanLoopEnd,
       VolType,
       PanType,
       VibrType,
       VibrSweep,
       VibrDepth,
       VibrRate:   byte;
       VolFadeOut,
       Reserved:   word;
     END;

     PFT2Sample = ^TFT2Sample;
     TFT2Sample = RECORD
       SampleLen,
       LoopStart,
       LoopLength: longint;
       Volume,
       FineTune,
       SampleType,
       Panning:    Byte;
       RelNote:    Shortint;
       Reserved:   byte;
       SampleName: ARRAY[0..21] OF char;
     END;

CONST MaxFT2Insts=255;
      MaxFT2Samples=1024;
TYPE pFT2Instruments=^tFT2Instruments;
     tFT2Instruments=ARRAY[0..MaxFT2Insts]OF RECORD
       InstrSize:  {Just for File...}longint;
       InstrName:  ARRAY[0..21] OF char;
       InstrType:  {?????}byte;
       NumSamples: {Number of Subsamples for Instrument}word;
       SampleHead: {length of the Sampleheader (Just to read it directly from file)}longint;
       SampleNos: {zero based relative SampleNumber to play on Note index}ARRAY[1..96] OF Byte;
       VolEnvPts,  {ARRAY[1..12]OF (Tick from KeyOn,Volume)}
       PanEnvPts:  {ARRAY[1..12]OF (Tick from KeyOn,NewPanning)}tEnvPts;
       NumVolPts,  {Number of Points in the VolEnvPts}
       NumPanPts,  {Number of Points in the PanEnvPts}
       VolSustPt,  {Index in VolEnvPts to leave the Volume as it is until KeyOff happens and then go on}
       VolLoopSt,  {Index in VolEnvPts to begin Volume Loop}
       VolLoopEnd, {Index in VolEnvPts to loop to begin of Volume Loop}
       PanSustPt,  {Index in PanEnvPts to leave the Panning as it is until KeyOff happens and then go on}
       PanLoopSt,  {Index in PanEnvPts to begin Panning Loop}
       PanLoopEnd, {Index in PanEnvPts to loop to begin of Panning Loop}
       VolType,    {Bit 0: Envelope on; 1:Sustain On; 2: Loop On}
       PanType,    {Bit 0: Envelope on; 1:Sustain On; 2: Loop On}
       VibrType,   {Sine=?, Square=?, RampUp=?, RampDn=?}
       VibrSweep,  {Slide VibrDepth from 0 to VibrDepth in VibrSweep}
       VibrDepth,  {from normal Vibrato effect}
       VibrRate:   {from normal Vibrato effect}byte;
       VolFadeOut, {IF KeyOff THEN IF VolFadeOut<>0
                    THEN Number of ticks from to silence = 32768/VolFadeOut
                    ELSE Continue Volume Envelope if sustained and do !NOT! KeyOff
                    ELSE (doesn't matter)}
       Reserved:   Word;
       SampleSt:   Word; {internal offset for absolute sample start}
     END;

     pFT2Samples=^tFT2Samples;
     tFT2Samples=ARRAY[0..MaxFT2Samples]OF RECORD {39*255}
       SampleLen,
       LoopStart,
       LoopEnd:    longint;
       Volume,
       FineTune,
       SampleType,
       Panning:    Byte;
       RelNote:    Shortint;
       Reserved:   byte;
       SampleName: ARRAY[0..21] OF char;
       SampOffset: Longint;
       FileOffset: Longint;
     END;

{**************************Oktalyzer***********************************************}
VAR  RelatedChn:ARRAY[1..8]OF Byte;
TYPE pOKTInstrument = ^tOKTInstrument;
     TOktInstrument = RECORD
      SampName   : ARRAY [0..19] OF CHAR;
      SampLen    : LONGINT;
      SampRepS   : WORD;
      SampRepL   : WORD;
      fill1      : BYTE;
      DefVol     : BYTE;                  { Default volume.                        }
      SampRes    : WORD;                  {meistens=1 8bit Sample >1 =7 bit Sample}
    END;

TYPE POktNote = ^TOktNote;
     TOktNote = RECORD
         byte1,
         byte2,
         byte3,
         byte4 : BYTE;
     END;

TYPE  pOktSign=^tOktSign;
      toktsign=RECORD
      CASE Integer OF
       1:(fullsign  :ARRAY[0..7]OF char);
       2:(B4sign    :ARRAY[0..3]OF char;
          Structsize:longint);
     END;

TYPE pMyWaveHdr=^tMyWaveHdr;
     tMyWaveHdr=RECORD
       WH           : TWaveHdr;
       BytePos      : Longint;
       Filled       : WordBool;
       Playing      : WordBool;
       DoUnPrepare  : WordBool;
       Repeating    : WordBool;      {+2}
     END;
     pMyWaveHdrARR=^tMyWaveHdrARR;
     tMyWaveHdrARR=ARRAY[0..98]OF tMyWaveHdr;

{****************Globale Variablen*************************************************}
VAR
   {MODUL-Infos}
   MI:tMODulInfo;
   FirstFilePos,LastFilePos:Longint;
   PatternTimes  :ARRAY[0..255]OF Longint;
   i,j,k:longint;   {Allgemein zum Zählen}
   ActiveChannelMap: TChannelMap;
   {Link zu Parent}
   HWindow       :hWnd;
   {SpeicherHandling}
CONST
   CodeSel       :Word=0;
   CodeSel1      :Word=0;
VAR
   PlayMemSel    :Word;
   PlayMemOfs    :Longint;
   PlayMemTop    :Longint;
{Aufbau Playmem:
   -VolumeTable     : ARRAY[-128..127][0..64] OF Word                 == 65*256*2= 33280 Bytes
   -32Bit MixBuffer : ARRAY[0..3999+1]OF RECORD Left,Right:Longint; END == 32008 Bytes
   ---------------------------------- 256 Bytes Temp for mixing --------------------------------
   AsmChn
   OldSmpL
   OldSmpR
   IncrR
   IncrL
   HH
   _TMP
   VolMaster
   ---------------------------------- 256 Bytes nix -------------------------------->64K Grenze
   -32 Bit SurroundBuffer (mit SelectorInc als Selector zu erreichen) == SurrBufferLen Bytes
   -Samples
   -SampleBuffers}
CONST
   MaxMixBufSamples= 2000;
   MaxSurBufSamples=MaxMixBufSamples;
   MixBufferOfs=33280;
   SurBufferOfs=MixBufferOfs+(MaxMixBufSamples+1)*8;
   oAsmChn     =SurBufferOfs+(MaxSurBufSamples+1)*8;
   oOldSmpL    =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel);
   oOldSmpR    =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+4;
   oIncrR      =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+8;
   oIncrL      =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+12;
   oHH         =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+16;
   o_TMP       =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+20;
   oVolMaster  =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+24;
   o_d         =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+28;
   o_c         =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+32;
   oOneBy1024  =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+36; {single!}
   oOneBy65536 =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+44; {single!}
   oOneBy4     =SurBufferOfs+(MaxSurBufSamples+1)*8+SizeOf(tMixChannel)+48; {single!}

VAR
   SongMemSel    :Word;
   SongMemOfs    :Longint;
{Aufbau Songmem:
     Tracks     ...[1..32][0..NumPatterns-1]OF Longint
     Pattern    ...Statisches Pattern zum Spielen [1..NumTracks][0..MaxPattLen-1(63)]OF TNote
     TrackData  ...[0..Lasttrack]OF tTrack}
   SongMemTop    :Longint;
   InstrMemSize  :Longint;
   FirstTrack    :LongInt;
   {WaveOut-VAR's}
   SoundSettings :Tsettings;
   NoteStretch   :Double;
   WaveHandle    :hWaveout;
   WaveFormat    :TPCMWaveFormat;
   WaveHeader    :tMyWaveHdrArr;
   WaveOfs       :Longint;
   WaveEnd       :Longint;
   WaveBuffers   :Byte;
   LastWaveBuffer:Byte;
   CurrBuffer    :Byte;
   LastBuffer    :Integer;
   BufferSize    :Longint;
   PlaytimeBuffer:Byte;
   PlayBufferLen :Longint;
   WaveBytePos   :Longint;

   {Zeitgeber-VAR's}
   Ende          :Boolean;
   CatchBuf      :tCatchBuf;
   InCritical    :Boolean;

   {SpielStatus}
   Tracks        :pTrackSeq;
   LastTrack     :Word;                  {Letzte gespielte Tracknummer}
   Pattern       :pPattern;              {enthält aktuelle Daten zum abspielen}
   PtrnTempi     :ARRAY[0..255]OF shortint;
   PtrnBreaks    :ARRAY[0..255]OF Byte;
   LastPattern   :Byte;                  {Letzte gespielte Patternnummer}
   RowPlayed     :ARRAY[0..255]OF SET OF 0..255;

   CurrPattern   :Word;                  {Aktuelle Patternnummer}
   CurrNote      :Word;                  {Notenzeile im Pattern}
   CurrVBlanc    :Byte;                  {VBlanc-Takt}

   Speed         :Byte;
   BPMSpeed      :Byte;
   NumSamples    :Word;                  {Anzahl der Samples für 1/50 sec}

   PM_Is_Working :Boolean;
   Repeatsong    :Boolean;
   PlayTime      :Longint;               {Zeit des aktuellen Songs in ms}
   LoopFromTime  :Longint;               {Zeit für Looping}
   LoopToTime    :Longint;
   Timestep      :Longint;
   Timesteprest  :Word;
   PlayTimes     :pPlayTimes;
   SearchTime    :Longint;               {Zu Suchende Position beim Spulen}
   Search        :Boolean;               {Nicht spielen, sondern spulen}
   SearchPattern :Boolean;
   SearchP       :Byte;
   SearchL       :Integer;
   StartTime     :Longint;               {Systemzeit bei Eintritt in Play_the_Module}
   Pause         :Boolean;               {Pause aktiv}
   NextWritePos  :Longint;
   DefChVol      :Byte;                  {Für TrackWrite}

CONST
   RecordFile    : Handle32 = -1;        {INVALID_HANDLE_VALUE}
VAR
   RecFile32Bit  : Bool;

VAR
  {Kanäle}
   SinTable,RampUpTable,RampDnTable,Sqrtable:ARRAY[0..255] OF shortint;    {Sinustabelle für Vibrato,Tremolo}

   delayPattern  :Boolean;
   patterndelay  :byte;
   DefPanning    :TPanArray;
   Channels      :tChannels;
   FT2Instruments:pFT2Instruments;
   FT2Samples    :pFT2Samples;
   initspeed     :byte;
   initbpmspeed  :byte;
   BufStart,
   BufEnd        :word;
   FarVibDepth   :Byte;

   UseSurround :Boolean;
   Scontrol    :Boolean;
   FastSlides  :Boolean;
   NoteMin     :Byte;
   NoteMax     :Byte;

CONST
  UseLinearTune:Boolean=False;

VAR
  LastOffs: byte;

TYPE pPointer=^Pointer;
VAR First:pPointer;
    FirstSeg:Longint ABSOLUTE First;

CONST
   MMTask         =0;
   WMTimer        =1;
   MMTimer        =2;
   WriteTID:WORD  =0;
   MMTID:Longint  =0;
   VxDTID:Longint =0;
   HalfNote       =$f1a2;      { 65536*2^(-1/12),zur Berechnung von Halbtonschritten }
   HalfNote1:Longint=$10f39;
   {Effekte}
   Arpeggio       =0;
   Portamento_up  =1;
   Portamento_down=2;
   Tone_portamento=3;
   Vibrato        =4;
   PortVolslide   =5;
   VibraVolslid   =6;
   Tremolo        =7;
   Panning        =8;
   PlayOffset     =9;
   VolumeSlide    =$A;
   PositionJump   =$B;
   SetVolume      =$C;
   PatternBreak   =$D;
   Enhanced       =$E;
   SetSpeed       =$F;
   {Protracker-E-Effekte}
   SetFilter      =$10;
   FSldUp         =$11;
   FSldDwn        =$12;
   GlissdCtrl     =$13;
   VibraWave      =$14;
   FineTune       =$15;
   PattrnLoop     =$16;
   TremWave       =$17;
   EPanning       =$18;
   RetrigNote     =$19;
   FineVolUp      =$1A;
   FineVolDwn     =$1B;
   NoteCut        =$1C;
   NoteDelay      =$1D;
   PattrnDelay    =$1E;
   InvLoop        =$1F;
   { OKT }
   OktArpg1       =$20;
   OktArpg2       =$21;
   OktSldUp       =$22;
   OktSldDwn      =$23;
   OktSld1Up      =$24;
   OktSld1Dwn     =$25;
   OktOldVol      =$26;
   { FAR }
   FarSpecial     =$27; { $00 }
   FarSldUp       =$28; { $01 }
   FarSldDwn      =$29; { $02 }
   FarPortamento  =$2a; { $03 }
   FarRetrig      =$2b; { $04 }
   VibratoDepth   =$2c; { $05 }
   FarVibrato     =$2d; { $06 }
   FarVolUp       =$2e; { $07 }
   FarVolDwn      =$2f; { $08 }
   VibratoSust    =$30; { $09 }
   VolumePort     =$31; { $0A }
   FineTempoDwn   =$32; { $0D }
   FineTempoUp    =$33; { $0E }
   { STM }
   Tremor         =$34;
   XtraFSldUp     =$35;
   XtraFSldDwn    =$36;
   { S3M }
   RetrigVolSlide =$37;
   StereoControl  =$38;
   SetGlobVol     =$39;
   SetBPMSpeed    =$3A;
   FineVibrato    =$3B;
   { UNIS/669 }
   FrequAdjust    =$3C;
   FinePanLeft    =$3D;
   FinePanRight   =$3E;
   SlotRetrigger  =$3F;
   {FT2}
   GlobVolSld     =$40;
   SetEnvelopePos =$41;
   PanSld         =$42;
   KeyOff         =$43; { for FT2 }
   {Dummy-Effekte...Nur Konstanten...Effekte werden während GetTotalTime gesetzt}
   NoEffekt       =$FF;


VAR
  MasterVol,
  GlobVol,
  InitGlobVol:Word;
  VolMaster:Longint;

VAR
   aResult :Word;
   reached :Boolean;
   pBuffer :pByte;
   BufSize :Word;
   UniLong :Longint;

   pArrangement:ARRAY[0..255] OF Byte;

{*************************** DirectSound Interface ***************************}

PROCEDURE MemWrite(SrcSeg:Word; SrcOfs:Longint; DstSeg:Word; DstOfs:Longint; Count:Longint; ARGS:Word);

FUNCTION GetSongMem(MEM:Longint):BOOLEAN;
FUNCTION FitSongMem(NewSize:Longint):BOOLEAN;
PROCEDURE FreeSongMem;

FUNCTION GetPlayMem(MEM:Longint):BOOLEAN;
FUNCTION FitPlayMem(NewSize:Longint):BOOLEAN;
PROCEDURE FreePlayMem;

FUNCTION FitGlobalMem(VAR Sel:word;VAR NewSize:Longint):Boolean;

PROCEDURE IncTime;FAR;

PROCEDURE OutputHugeMem(P:PChar);

IMPLEMENTATION

PROCEDURE MemWrite(SrcSeg:Word; SrcOfs:Longint; DstSeg:Word; DstOfs:Longint; Count:Longint; ARGS:Word); ASSEMBLER;
{
ARGS:
14...16Bit Limit to S[0]-s[-1]<32768 Difference
13... 8Bit Limit to S[0]-s[-1]<128 Difference
12...Fill with zero!
11...Reverse Count Words from Src to Dst
10...Reverse Count Bytes from Src to Dst
 9...16 Bit delta to 8 Bit
 8...16 Bit delta
 7... 8 Bit delta
 6...16 Bit to 8 Bit flip
 5...16 Bit to 8 Bit
 4... 7 Bit to 8 Bit shl
 3...16 Bit flip
 2...16 Bit
 1... 8 Bit flip
 0... 8 Bit
}
ASM
  cmp DstSeg,0
  je @out
  cmp SrcSeg,0
  je @out
  DB 66h; mov cx,word Ptr Count
  DB 66h; OR cx,cx
  je @out
  mov es,SrcSeg
  DB 66h; mov si,word Ptr SrcOfs
  mov ax,DstSeg
 {mov fs,ax}  DB 8eh,0E0h
  DB 66h; mov di,word Ptr DstOfs
  mov bx,args
  cmp bx,32
  ja @Out
  add bx,bx
  {mov gs,}DB 8Eh,2Eh; DW Offset CodeSel1
  DB 65h; jmp word ptr [offset @MemJmpTable+BX]

@MemJmpTable:
  DW Offset @STRTSB
  DW Offset @STRTUB
  DW Offset @STRTSW
  DW Offset @STRTUW
  DW Offset @STRT7Bit
  DW Offset @STRT16BitS0
  DW Offset @STRT16BitU0
  DW Offset @STRT08Delta
  DW Offset @STRT16Delta
  DW Offset @STRT16to8Delta
  DW Offset @STRTReverse
  DW Offset @STRTReverse16
  DW Offset @FillZero
  DW Offset @STRTLimit8
  DW Offset @STRTLimit16
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
  DW Offset @Out
{8Bit signed->8Bit signed with |max Delta|=128}
@STRTLimit8:
  mov dx,128  {Upper Limit}
  mov bx,-128 {Lower limit}
@STRTLimit80:
 {movsx ax,byte ptr [es:esi]}  DB 26h,67h,0Fh,0BEh,06h
  cmp ax,dx   {Upper Limit}
  jl @STRTLimit81
  mov ax,dx
  jmp @STRTLimit82
@STRTLimit81:
  cmp ax,bx   {Lower limit}
  jg @STRTLimit82
  mov ax,bx
@STRTLimit82:
  mov dx,128  {Upper Limit}
  mov bx,-128 {Lower limit}
  add bx,ax
  add dx,ax
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; inc si
  DB 66h; dec cx
  jnz @STRTLimit80
  jmp @END
{16Bit signed->16Bit signed with |max Delta|=32768}
@STRTLimit16:
  DB 66h; SHR cx,1
  DB 66h; mov dx,8000h; DW $0000 {Upper Limit}
  DB 66h; mov bx,8000h; DW $FFFF {Lower limit}
@STRTLimit160:
 {movsx eax,word ptr es:[esi]}  DB 26h,67h,66h,0Fh,0BFh,06h
  DB 66h; cmp ax,dx   {Upper Limit}
  jl @STRTLimit161
  DB 66h; mov ax,dx
  jmp @STRTLimit162
@STRTLimit161:
  DB 66h; cmp ax,bx   {Lower limit}
  jg @STRTLimit162
  DB 66h; mov ax,bx
@STRTLimit162:
  DB 66h; mov dx,8000h; DW $0000 {Upper Limit}
  DB 66h; mov bx,8000h; DW $FFFF {Lower limit}
  DB 66h; add bx,ax
  DB 66h; add dx,ax
 {mov fs:[edi],ax}  DB 64h,67h,89h,07h
  DB 66h; add di,2
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRTLimit160
  jmp @END
{8Bit->8Bit}
@STRTSB:
 {mov al,es:[esi]}  DB 26h,67h,8ah,06h
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; inc si
  DB 66h; dec cx
  jnz @STRTSB
  jmp @END
{7Bit->8Bit}
@STRT7Bit:
 {mov al,es:[esi]}  DB 26h,67h,8ah,06h
  SHL al,1
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; inc si
  DB 66h; dec cx
  jnz @STRT7Bit
  jmp @END
{8Bit->8Bit with sign inversion}
@STRTUB:
 {mov al,es:[esi]}  DB 26h,67h,8ah,06h
  XOR al,80h
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; inc si
  DB 66h; dec cx
  jnz @STRTUB
  jmp @END
{16Bit->16Bit}
@STRTSW:
  DB 66h; SHR cx,1
@STRTSW0:
 {mov ax,es:[esi]}  DB 26h,67h,8bh,06h
 {mov fs:[edi],ax}  DB 64h,67h,89h,07h
  DB 66h; add di,2
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRTSW0
  jmp @END
{16Bit->16Bit with sign inversion}
@STRTUW:
  DB 66h; SHR cx,1
@STRTUW0:
 {mov ax,es:[esi]}  DB 26h,67h,8bh,06h
 XOR ax,8000h
 {mov fs:[edi],ax}  DB 64h,67h,89h,07h
  DB 66h; add di,2
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRTUW0
  jmp @END
{16Bit->8Bit with sign inversion}
@STRT16BitU0:
  DB 66h; SHR cx,1
@STRT16BitU:
 {mov ax,es:[esi]}  DB 26h,67h,8bh,06h
 XOR ax,8000h
 {mov fs:[edi],ah}  DB 64h,67h,88h,27h
  DB 66h; inc di
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRT16BitU
  jmp @END
{16Bit->8Bit}
@STRT16BitS0:
  DB 66h; SHR cx,1
@STRT16BitS:
 {mov ax,es:[esi]}  DB 26h,67h,8bh,06h
 {mov fs:[edi],ah}  DB 64h,67h,88h,27h
  DB 66h; inc di
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRT16BitS
  jmp @END
{8Bit signed delta -> 8Bit signed}
@STRT08delta:
  XOR ax,ax
@STRT08delta1:
 {add al,es:[esi]}  DB 26h,67h,02h,06h
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; inc si
  DB 66h; dec cx
  jnz @STRT08delta1
  jmp @END
{16Bit signed delta -> 16Bit signed}
@STRT16delta:
  XOR ax,ax
  DB 66h; SHR cx,1
@STRT16delta1:
 {add ax,es:[esi]}  DB 26h,67h,03h,06h
 {mov fs:[edi],ax}  DB 64h,67h,89h,07h
  DB 66h; add di,2
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRT16delta1
  jmp @END
{16Bit signed delta -> 8Bit signed}
@STRT16To8delta:
  XOR ax,ax
  DB 66h; SHR cx,1
@STRT16To8delta1:
 {add ax,es:[esi]}  DB 26h,67h,03h,06h
 {mov fs:[edi],ah}  DB 64h,67h,88h,27h
  DB 66h; inc di
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @STRT16To8delta1
  jmp @END
{8Bit -> 8Bit reverse (BiDi-Loop)}
@STRTReverse:
  DB 66h; add di,cx
@LoopReverse:
  DB 66h; dec di
 {mov al,es:[esi]}  DB 26h,67h,8ah,06h
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc si
  DB 66h; dec cx
  jnz @LoopReverse
  jmp @END
{16Bit -> 16Bit reverse}
@STRTReverse16:
  DB 66h; add di,cx
  DB 66h; SHR cx,1
@LoopReverse16:
  DB 66h; sub di,2
 {mov ax,es:[esi]}  DB 26h,67h,8bh,06h
 {mov fs:[edi],ax}  DB 64h,67h,89h,07h
  DB 66h; add si,2
  DB 66h; dec cx
  jnz @LoopReverse16
  jmp @END
{8Bit Fill with zero}
@FillZero:
 xor al,al
@FillZero1:
 {mov fs:[edi],al}  DB 64h,67h,88h,07h
  DB 66h; inc di
  DB 66h; dec cx
  jnz @FillZero
  jmp @END

@END:
@out:
END;

PROCEDURE OutputHugeMem(P:PChar);
VAR L:Longint;
    P1:pChar;
    PC:ARRAY[0..40]OF CHar;
BEGIN
  P1:=P;
  L:={$IFDEF HUGEMEM}GetFreeHugeMem DIV 1024{$ELSE}0{$ENDIF HUGEMEM};
  wvsPrintF(PC,'Player32(%s): Free DPMI Mem %ld'#13#10,P1);
  OutputDebugString(PC);
END;

FUNCTION FitGlobalMem(VAR Sel:word;VAR NewSize:Longint):Boolean;
VAR NewSel:Word;
BEGIN
  {$IFDEF HUGEMEM}
  IF OS<>WINDOWS_NT THEN
  BEGIN
    FitGlobalMem:=False;
    NewSel:=0;
    IF Sel<>0 THEN
    BEGIN
      NewSel:=HugeReAlloc(Sel,NewSize);
      IF NewSize=0 THEN
      BEGIN
        Sel:=0;
        FitGlobalMem:=True;
        Exit;
      END;
    END ELSE IF NewSize>0
      THEN NewSel:=HugeAlloc(NewSize)
      ELSE FitGlobalMem:=True;
    IF NewSel=0 THEN Exit;
    Sel:=NewSel;
    NewSize:=HugeSize(Sel);
    FitGlobalMem:=True;
  END ELSE
  {$ENDIF HUGEMEM}
  BEGIN
    FitGlobalMem:=False;
    NewSel:=0;
    IF (Sel<>0)AND(GlobalSize(Sel)<>0) THEN
    BEGIN
      GlobalPageUnLock(Sel);
      GlobalUnlock(Sel);
      NewSel:=0;
      IF NewSize<>0 THEN
      BEGIN
        NewSel:=GlobalReAlloc(Sel,GlobalSize(Sel),gmem_modify OR gmem_moveable);       IF NewSel=0 THEN Exit;
        NewSel:=GlobalReAlloc(NewSel,NewSize,gmem_moveable);                           IF NewSel=0 THEN Exit;
        NewSel:=GlobalReAlloc(NewSel,GlobalSize(NewSel),gmem_share OR gmem_nodiscard);
      END ELSE
      BEGIN
        GlobalFree(Sel);
        NewSize:=0;
        Sel:=0;
        FitGlobalMem:=True;
        Exit;
      END;
    END ELSE IF NewSize>0
      THEN NewSel:=GlobalAlloc(gmem_share OR gmem_nodiscard,NewSize)
      ELSE FitGlobalMem:=True;
    IF NewSel=0 THEN Exit;
    Sel:=NewSel;
    Sel:=SelectorOf(GlobalLock(Sel));
    GlobalPageLock(Sel);
    NewSize:=GlobalSize(Sel);
    FitGlobalMem:=True;
  END;{}
END;

PROCEDURE AdjustSongMem;
BEGIN
  {$IFDEF HUGEMEM}
  SongMemTop:=GetSelectorLimit(SongMemSel)+1;
  {$ELSE HUGEMEM}
  SongMemTop:=GlobalSize(SongMemSel);
  {$ENDIF HUGEMEM}
  Tracks:=Ptr(SongMemSel,0);
  IF Longint(MI.NumPatterns)*SizeOf(tOrderData)+Longint(MI.NumTracks)*SizeOf(tTrack)>65536
  THEN Pattern:=Ptr(SongMemSel+SelectorInc,0)
  ELSE Pattern:=Ptr(SongMemSel,Longint(MI.NumPatterns)*Longint(SizeOf(tOrderData)));
END;

FUNCTION GetSongMem(MEM:Longint):BOOLEAN;
VAR PatternSize:Longint;
BEGIN
  GetSongMem:=False;
  PatternSize:=Longint(MI.NumPatterns)*SizeOf(tOrderData)+Longint(MI.NumTracks)*SizeOf(tTrack);
  IF PatternSize>65536
  THEN MEM:=MEM+65536+MI.NumTracks*SizeOf(tTrack)+256
  ELSE MEM:=MEM+Longint(MI.NumPatterns)*SizeOf(tOrderData)+MI.NumTracks*SizeOf(tTrack)+256;
  IF NOT FitGlobalMem(SongMemSel,MEM) THEN Exit;
  AdjustSongMem;
  FillChar(Tracks^,Longint(MI.NumTracks)*SizeOf(tTrack),0);
  IF PatternSize>65536
  THEN SongMemOfs:=Longint(65536)+MI.NumTracks*SizeOf(tTrack)
  ELSE SongMemOfs:=0+MI.NumPatterns*SizeOf(tOrderData)+MI.NumTracks*SizeOf(tTrack);
  FirstTrack:=SongMemOfs;
  GetSongMem:=True;
END;

FUNCTION FitSongMem(NewSize:Longint):BOOLEAN;
BEGIN
  FitSongMem:=False;
  IF NOT FitGlobalMem(SongMemSel,NewSize) THEN Exit;
  AdjustSongMem;
  MI.SongMem:=SongmemOfs-Firsttrack+PlayMemOfs-SurrBufferLen-65536;
  FitSongMem:=True;
END;

PROCEDURE FreeSongMem;
BEGIN
  SongMemTop:=0;
  IF NOT FitGlobalMem(SongMemSel,SongMemTop) THEN Exit;
  MI.SongMem:=0;
  SongMemOfs:=0;
  Tracks:=NIL;
  Pattern:=NIL;
END;

FUNCTION GetPlayMem(MEM:Longint):BOOLEAN;
BEGIN
  GetPlayMem:=False;
  Mem:=Mem+SurrBufferLen+65536;
  IF NOT FitGlobalMem(PlayMemSel,Mem) THEN Exit;
  PlayMemTop:=Mem;
  PlayMemOfs:=SurrBufferLen+65536;
  single(ptr(PlayMemSel,oOneBy1024)^):=OneBy1024;
  single(ptr(PlayMemSel,oOneBy65536)^):=OneBy65536;
  single(ptr(PlayMemSel,oOneBy4)^):=OneBy4;
  GetPlayMem:=True;
END;

FUNCTION FitPlayMem(NewSize:Longint):BOOLEAN;
BEGIN
  FitPlayMem:=True;
  IF PlayMemTop>NewSize THEN Exit;
  FitPlayMem:=False;
  IF NOT FitGlobalMem(PlayMemSel,NewSize) THEN Exit;
  PlayMemTop:=NewSize;
  FitPlayMem:=True;
END;

PROCEDURE FreePlayMem;
BEGIN
  PlayMemTop:=0;
  IF NOT FitGlobalMem(PlayMemSel,PlayMemTop) THEN Exit;
END;

PROCEDURE IncTime; ASSEMBLER;
ASM
  mov bx,word Ptr TimeStep+2
  mov ax,word Ptr TimeStep
  add TimeStepRest,ax
  adc word Ptr PlayTime,bx
  adc word Ptr PlayTime+2,0
END;

BEGIN
  ASM
    push cs
    call AllocCSToDSAlias
    mov CodeSel1,ax
    push ax
    call GlobalPageLock
  END;
END.
