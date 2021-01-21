{$DEFINE CompressTracks}

UNIT Loaders;
{$C Fixed Preload Permanent}
INTERFACE
USES PlayDefs,WinApi,WinTypes,
     MODC,Strings,WaitBox,Win31,FILE32, W32DEFS, VALCONV,windos;

CONST  LoadSamples: Boolean=True;

FUNCTION GetInfo(_FHandle:Handle32; _FIs32:WordBool; FileName:pChar; UniLoad:Boolean):Word;

PROCEDURE MOD_GetFileData (VAR Error:Word);
PROCEDURE MTM_GetFileData (VAR Error:Word);
PROCEDURE f669_GetFileData(VAR Error:Word);
PROCEDURE Okt_GetFileData (VAR Error:Word);
PROCEDURE FAR_GetFileData (VAR Error:Word);
PROCEDURE STM_GetFileData (VAR Error:Word);
PROCEDURE S3M_GetFileData (VAR Error:Word);
PROCEDURE FT2_GetFileData (VAR Error:Word);

PROCEDURE ReplaceTrackNums;

IMPLEMENTATION

{UNIT local file variables to save call stack}
VAR FHandle : Handle32;
    FIs32   : WordBool;

TYPE
   pTrackTable=^tTrackTable;
   tTrackTable=RECORD
     TrackOfs:ARRAY[0..32*255]OF Longint;
     MaxTrack:Word;
   END;

CONST TrackTable:pTrackTable=NIL;
PROCEDURE ReplaceTrackNums;
VAR I,J:Word;
    K:Word;
BEGIN
  IF TrackTable=NIL THEN Exit;
  WITH TrackTable^ DO
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO
  BEGIN
    k:=Tracks^[i][j]-1;
    IF k<MaxTrack
    THEN Tracks^[i][j]:=Longint(TrackOfs[k])
    ELSE Tracks^[i][j]:=Longint(0);
  END;
  GlobalFreePtr(TrackTable);
  TrackTable:=NIL;
END;

{$IFDEF CompressTracks}
{**********************************************}
{* TRACKWRITE                                 *}
{**********************************************}
{Gepacktes TrackFormat:                                           }
{WORD ...Länge des Tracks IN Byte einschliesslich dieses Words    }
{  +Beliebig oft                                                  }
{       Byte ...Flags:                                            }
{               Bit0...Instr.   <> 00h                            }
{               Bit1...Ton      <> 00h                            }
{               Bit2...Effekt   <> FFh (NoEffekt)                 }
{               Bit3...Operands <> 00h                            }
{               Bit4...ChnVol   <> FFh/00h (DefChVol)             }
{               [Bit 5-7 Anzahl der folgenden LeerZeilen]         }
{      +Byte ...Falls (Flags AND 01h)>0 Instrument                }
{      +Word ...Falls (Flags AND 02h)>0 Ton                       }
{      +Byte ...Falls (Flags AND 04h)>0 Effekt                    }
{      +Byte ...Falls (Flags AND 08h)>0 Operands                  }
{      +Byte ...Falls (Flags AND 10h)>0 ChnVol                    }
FUNCTION TrackWrite(Num: Byte; Max: Word):Boolean;
TYPE pByte=^tByte;
     tByte=ARRAY[0..65520]OF Byte;
VAR  twi       : Integer;
     twj       : Integer;
     Ofs0      : Word;
     Ofs1      : Word;
     OldOfs    : Word;
     Flgs      : Byte;
     EmptyRows : Byte;
     bBuf      : pByte;
BEGIN
  IF TrackTable=NIL THEN TrackTable:=GlobalAllocPtr(gptr,SizeOf(tTrackTable));
  TrackWrite:=True;
  bBuf:=GlobalAllocPtr(gMem_NoDiscard OR gMem_Fixed,2+Max*7);
  IF bBuf=NIL THEN BEGIN TrackWrite:=False; Exit; END;
  FOR twi:=1 TO Num DO
  BEGIN
    Ofs1      := 2;
    OldOfs    := Ofs1;
    EmptyRows := 0;
    IF Max>0 THEN FOR twj:=0 TO Max-1 DO WITH Pattern^[twi][twj] DO
    BEGIN
      { convert ProTracker enhanced effects }
      IF Effekt = Enhanced THEN
      BEGIN
        Effekt   := $10 + Operands SHR 4;
        Operands := Operands AND $0F;
      END;
      {Note Packen!}
      Ofs0:=Ofs1;
      Flgs:=0;
      Inc(Ofs1);
      { load ops only IF Effect <> NoEffect }
      IF Instrument<>0    THEN BEGIN Flgs:=Flgs OR $01; bBuf^[ofs1]:=Instrument      ; Inc(Ofs1);  END;
      IF Ton<>0           THEN BEGIN Flgs:=Flgs OR $02; word(Addr(bBuf^[ofs1])^):=Ton; Inc(Ofs1,2); END;
      IF Effekt<>NoEffekt THEN
      BEGIN
                                     Flgs:=Flgs OR $04; bBuf^[ofs1]:=Effekt          ; Inc(Ofs1);
        IF Operands<>0    THEN BEGIN Flgs:=Flgs OR $08; bBuf^[ofs1]:=Operands        ; Inc(Ofs1);  END;
        CASE effekt OF
         Portamento_up  :IF MI.FileType in [STM, S3M] THEN
                           if Operands >= $F0 THEN Effekt := FSldUp else
                           if Operands >= $E0 THEN Effekt := XtraFSldUp;
         Portamento_down:IF MI.FileType in [STM, S3M] THEN
                           if Operands >= $F0 THEN Effekt := FSldDwn else
                           if Operands >= $E0 THEN Effekt := XtraFSldDwn;
         VolumeSlide    :IF MI.FileType in [STM, S3M] THEN
                           if (Operands and $0F = $0F)and(Operands and $F0 <> 0) THEN Effekt := FineVolUp else
                           if (Operands and $F0 = $F0)and(Operands and $0F <> 0) THEN Effekt := FineVolDwn;
         StereoControl  :Scontrol:=true;
         Panning        :MI.PanningOff:=False;
         EPanning       :MI.PanningOff:=False;
        END;
        Include(MI.EffectsUsedSet,Effekt);
      END;
      IF ChnVol<>DefChVol THEN
      BEGIN
                                     Flgs:=Flgs OR $10; bBuf^[ofs1]:=ChnVol          ; Inc(Ofs1);
        IF Mi.FileType=FT2 THEN
        BEGIN
          CASE ChnVol OF
            $00..$0F:{NIX}                ;
            $10..$50:{Set Volume}         Include(Mi.EffectsUsedSet, SetVolume);
            $60..$6F:{VolSldDn}           Include(Mi.EffectsUsedSet, VolumeSlide);
            $70..$7F:{VolSldUp}           Include(Mi.EffectsUsedSet, VolumeSlide);
            $80..$8F:{FineVolSldDn}       Include(Mi.EffectsUsedSet, FineVolDwn);
            $90..$9F:{FineVolSldUp}       Include(Mi.EffectsUsedSet, FineVolUp);
            $A0..$AF:{Set Vibrato Speed} {Include(Mi.EffectsUsedSet, SetVibrSpd)};
            $B0..$BF:{Vibrato}            Include(Mi.EffectsUsedSet, Vibrato);
            $C0..$CF:{Set Panning}        Include(Mi.EffectsUsedSet, EPanning);
            $D0..$DF:{PanSlide Left}      Include(Mi.EffectsUsedSet, PanSld{Left});
            $E0..$EF:{PanSlide Right}     Include(Mi.EffectsUsedSet, PanSld{Right});
            $F0..$FF:{Tone Portamento}    Include(Mi.EffectsUsedSet, Tone_Portamento);
          END;
        END ELSE Include(Mi.EffectsUsedSet, SetVolume);
      END;
      IF (Flgs=0)AND(EmptyRows<7)AND(twj<>0) THEN
      BEGIN
        Inc(EmptyRows);
        Dec(Ofs1);
      END ELSE
      BEGIN
        bBuf^[OldOfs]:=bBuf^[OldOfs] OR (EmptyRows SHL 5);
        bBuf^[ofs0]:=FLGS;
        EmptyRows:=0;
        OldOfs:=Ofs0;
      END;
    END;
    pword(bBuf)^:=ofs1;
    TrackWrite:=True;
    IF (LongInt(SongMemOfs)+LongInt(Ofs1)>=SongMemTop)
    AND(NOT FitSongMem(LongInt(SongMemOfs)+LongInt(Ofs1)+4096)){} THEN
    BEGIN
      TrackWrite:=False;
      GlobalFreePtr(bBuf);
      Exit;
    END;
    TrackTable^.TrackOfs[TrackTable^.MaxTrack]:=SongMemOfs;
    Inc(TrackTable^.MaxTrack);
    MemWrite(Seg(bBuf^),Ofs(bBuf^),SongMemSel,SongMemOfs,ofs1,0);
    Inc(SongMemOfs,ofs1);
  END;
  GlobalFreePtr(bBuf);
END;

{$ELSE CompressTracks}

{**********************************************}
{* TRACKWRITE                                 *}
{**********************************************}
{Gepacktes TrackFormat:                                           }
{WORD ...Länge des Tracks IN Byte einschliesslich dieses Words    }
{  +Beliebig oft tNote                                            }
FUNCTION TrackWrite(Num: Byte; Max: Word):Boolean;
VAR  twi       : Integer;
     twj       : Integer;
     LastRow   : Integer;
BEGIN
  IF TrackTable=NIL THEN TrackTable:=GlobalAllocPtr(gptr,SizeOf(tTrackTable));
  TrackWrite:=True;
  FOR twi:=1 TO Num DO
  BEGIN
    LastRow:=0;
    IF Max>0 THEN FOR twj:=0 TO Max-1 DO WITH Pattern^[twi][twj] DO
    BEGIN
      { convert ProTracker enhanced effects }
      IF Effekt = Enhanced THEN
      BEGIN
        Effekt   := $10 + Operands SHR 4;
        Operands := Operands AND $0F;
      END;
      IF (Instrument<>0)
      OR (Ton<>0)
      OR (Effekt<>NoEffekt)
      OR (Operands<>0)
      OR (ChnVol<>DefChVol) THEN LastRow:=twj;
      { load ops only IF Effect <> NoEffect }
      IF Effekt<>NoEffekt THEN
      BEGIN
        CASE effekt OF
         Portamento_up  :IF MI.FileType in [STM, S3M] THEN
                           if Operands >= $F0 THEN Effekt := FSldUp else
                           if Operands >= $E0 THEN Effekt := XtraFSldUp;
         Portamento_down:IF MI.FileType in [STM, S3M] THEN
                           if Operands >= $F0 THEN Effekt := FSldDwn else
                           if Operands >= $E0 THEN Effekt := XtraFSldDwn;
         VolumeSlide    :IF MI.FileType in [STM, S3M] THEN
                           if (Operands and $0F = $0F)and(Operands and $F0 <> 0) THEN Effekt := FineVolUp else
                           if (Operands and $F0 = $F0)and(Operands and $0F <> 0) THEN Effekt := FineVolDwn;
         StereoControl  :Scontrol:=true;
         Panning        :MI.PanningOff:=False;
         EPanning       :MI.PanningOff:=False;
        END;
        Include(MI.EffectsUsedSet,Effekt);
      END;
      IF ChnVol<>DefChVol THEN
      BEGIN
        IF Mi.FileType=FT2 THEN
        BEGIN
          CASE ChnVol OF
            $00..$0F:{NIX}                ;
            $10..$50:{Set Volume}         Include(Mi.EffectsUsedSet, SetVolume);
            $60..$6F:{VolSldDn}           Include(Mi.EffectsUsedSet, VolumeSlide);
            $70..$7F:{VolSldUp}           Include(Mi.EffectsUsedSet, VolumeSlide);
            $80..$8F:{FineVolSldDn}       Include(Mi.EffectsUsedSet, FineVolDwn);
            $90..$9F:{FineVolSldUp}       Include(Mi.EffectsUsedSet, FineVolUp);
            $A0..$AF:{Set Vibrato Speed} {Include(Mi.EffectsUsedSet, SetVibrSpd)};
            $B0..$BF:{Vibrato}            Include(Mi.EffectsUsedSet, Vibrato);
            $C0..$CF:{Set Panning}        Include(Mi.EffectsUsedSet, EPanning);
            $D0..$DF:{PanSlide Left}      Include(Mi.EffectsUsedSet, PanSld{Left});
            $E0..$EF:{PanSlide Right}     Include(Mi.EffectsUsedSet, PanSld{Right});
            $F0..$FF:{Tone Portamento}    Include(Mi.EffectsUsedSet, Tone_Portamento);
          END;
        END ELSE Include(Mi.EffectsUsedSet, SetVolume);
      END;
    END;
    TrackWrite:=True;
    twj:=(LastRow+1)*SizeOf(tNote)+2;
    IF (LongInt(SongMemOfs)+twj>=SongMemTop)
    AND(NOT FitSongMem(LongInt(SongMemOfs)+twj+4096)) THEN
    BEGIN
      TrackWrite:=False;
      Exit;
    END;
    TrackTable^.TrackOfs[TrackTable^.MaxTrack]:=SongMemOfs;
    Inc(TrackTable^.MaxTrack);
    MemWrite(Seg(twj),Ofs(twj),SongMemSel,SongMemOfs,2,2);
    MemWrite(Seg(Pattern^[twi]),Ofs(Pattern^[twi]),SongMemSel,SongMemOfs+2,twj-2,2);
    Inc(SongMemOfs,twj);
  END;
END;
{$ENDIF CompressTracks}


{**********************************************}
{* String Convert                             *}
{* converts pChar w/o term. #0 to String and  *}
{* excludes control chars                     *}
{**********************************************}
PROCEDURE sconvert(VAR S:STRING; pc:ARRAY OF char; nmax:byte); ASSEMBLER;
ASM
    mov dx,ds
    cld
    {CX--->Anzahl der Zeichen}
    mov cl,nmax
    mov ah,cl   { save nmax }
    XOR ch,ch
    {DS:SI--->PC[0]}
    lds si,pc
    {ES:DI--->S[1]}
    les di,s
    mov bx,di   { save DI   }
    inc di
@start:
    lodsb
    OR  al,al   { was it 0  }
    jz  @stop   { stop it   }
    cmp al,' '
    {PC[i]>=#32?}
    jae @1
    {nein--->S[i+1]:=32}
    mov al,' '
@1: {Ja --->S[i+1]:=pc[i]}
    stosb
    loop @start
@stop:
    {letzte Position eines Zeichens berechnen}
    sub ah,cl
    {Anzahl der Zeichen IN S[0] schreiben}
    mov al,ah
    mov di,bx   { restore   }
    stosb
    mov ds,dx
END;


{**********************************************}
{* REDUCE STRING -> removes start&end Spaces  *}
{**********************************************}
PROCEDURE ReduceString (VAR AString: STRING);
BEGIN
  WHILE (Length(AString)>0) AND (AString[Length(AString)]=#32) DO DEC(byte(AString[0]));
  WHILE (Length(AString)>0) AND (AString[1]=#32) DO Delete(AString,1,1);
END;


{**********************************************}
{* Allocate Comment memory                    *}
{**********************************************}
PROCEDURE AllocateComment (Size: Word);
BEGIN
  IF Mi.Comment<>NIL THEN GlobalFreePtr(Mi.Comment);
  MI.Comment:=NIL;
  IF Size>4096
    THEN MI.Comment:=GlobalallocPtr(gmem_nodiscard OR gptr,Size)
    ELSE MI.Comment:=GlobalallocPtr(gmem_nodiscard OR gptr,4096);
END;


{**********************************************}
{* Load Instrument                            *}
{**********************************************}
FUNCTION LoadInstrument (VAR Name:STRING; VAR Offset,IEnd,LoopStart,LoopEnd:LongInt;
                         VAR Volume,Bits:Byte; SampNum,InsType: byte; Bidi:Boolean):Boolean;
CONST
  CR             = #$0D#$0A;
VAR
  Buf            : RECORD
                     CASE Byte OF
                       1:(P       :Pointer);
                       2:(Offs,Sel:Word);
                   END;
  RedType        : Byte;
  S              : STRING;
  T              : STRING[10];
  TmpSel         : Word;
  I              : Longint;
BEGIN
  TmpSel:=0;
  LoadInstrument:=True;
  IF NOT LoadSamples THEN EXIT;
  {Read the whole instrument at once                         }
  UniLong:=IEnd+8192;
  IF NOT(FitGlobalMem(TmpSel,UniLong))THEN Exit;
  Buf.P   := Ptr(TmpSel,0);
  UniLong := 0; { how many bytes have been successfully read }
  REPEAT
    IF (IEnd - UniLong)<32768 THEN BufSize := IEnd - Unilong ELSE BufSize := 32768;
    IF Buf.Offs>65536-BufSize THEN BufSize := 65536-Buf.Offs;
    aResult := _lRead32(Fhandle, FIs32, Buf.P, BufSize);
    INC(Buf.Offs,aResult); IF Buf.Offs=0 THEN INC(Buf.Sel,SelectorInc);
    INC(UniLong, aResult);
  UNTIL (UniLong=IEnd)OR(aResult=0);
  {for ripping...}
  IF MI.FileType IN [STM, S3M] THEN IF LastFilePos<_lPos32(FHandle,FIs32) THEN LastFilePos:=_lPos32(FHandle,FIs32);
  {Adjust Instrument...}
  IEnd := UniLong;
  IF IEnd>0 THEN
  BEGIN
    IF Volume>64           THEN Volume    := 64;
    IF LoopEnd>IEnd        THEN LoopEnd   := IEnd;
    IF LoopStart>IEnd      THEN LoopStart := 0;
    IF LoopEnd-Loopstart<3 THEN BEGIN LoopStart:=0; LoopEnd:=0; END;
    { conversions 1st stage, produce linear, signed, little endian data }
    CASE InsType OF
      0, 2: {  8/16 bit signed - ok };
      1, 3, {  8/16 bit unsigned - flip }
         4, {     7 bit signed - shl }
      7, 8: {  8/16 bit delta - convert to linear }
        MemWrite(TmpSel,0,TmpSel,0,IEnd,InsType);
    END;
    IF Bits AND $1F = 16 THEN InsType:=2 ELSE InsType:=0;
    (**)
    {Remove too short loops...}
    IF (LoopEnd>0)AND(LoopEnd<1024) THEN
    BEGIN
      {Remove BiDi in too short loops...}
      IF BiDi THEN
      BEGIN
        I:=LoopEnd-LoopStart;
        MemWrite(TmpSel,LoopStart,TmpSel,LoopEnd,I,9+(Bits AND $1F) DIV 8);
        Inc(IEnd,I);
        Inc(LoopEnd,I);
        BiDi:=False;
      END;
      WHILE LoopEnd<1024 DO
      BEGIN
        I:=LoopEnd-LoopStart;
        MemWrite(TmpSel,LoopStart,TmpSel,LoopEnd,I,0);
        Inc(LoopEnd,I);
        Inc(IEnd,I);
      END;
    END ELSE
    {Fade Out one shot samples}
    IF LoopEnd=0 THEN
    BEGIN

    END;
    (**)
    IF (SoundSettings.deviceID>=dev_HDD)OR(PeakSearch) THEN
    BEGIN
      PlayMemOfs:=(Longint(PlayMemOfs) DIV 4+1)*4;
      IF (PlayMemOfs+IEnd+16>=PlayMemTop)AND(NOT FitPlayMem(PlayMemOfs+IEnd+16)){ } THEN
      BEGIN
        LoadInstrument:=False;
        Exit;
      END;
      {PrePend two Zero's for Cubic Splines}
      MemWrite(PlayMemSel,PlayMemOfs,PlayMemSel,PlayMemOfs,2,12);
      Inc(PlayMemOfs,2);
      {...}
      OFFSET:=PlayMemOfs;
      MemWrite(TmpSel,0,PlayMemSel,PlayMemOfs,IEnd,InsType);
      Inc(PlayMemOfs,IEnd);
      IF BiDi THEN
      BEGIN
        IF (PlayMemOfs+LoopEnd-LoopStart+16>=PlayMemTop)
        AND(NOT FitPlayMem(PlayMemOfs+LoopEnd-LoopStart+16))THEN
        BEGIN
          LoadInstrument:=False;
          Exit;
        END;
        MemWrite(PlayMemSel,Offset+LoopStart,PlayMemSel,Offset+LoopEnd,LoopEnd-LoopStart,9+(Bits AND $1F) DIV 8);
        Inc(PlayMemOfs,LoopEnd-LoopStart);
        Inc(IEnd,LoopEnd-LoopStart);
        Inc(LoopEnd,LoopEnd-LoopStart);
      END;
      {for Oversampling==>Append 4 Bytes!}
      IF (LoopEnd=0)
      THEN MemWrite(PlayMemSel,PlayMemOfs,PlayMemSel,PlayMemOfs,4,12)
      ELSE MemWrite(PlayMemSel,Offset+LoopStart,PlayMemSel,Offset+LoopEnd,4,2);
      Inc(PlayMemOfs,4);
    END ELSE OFFSET:=4;
    INC(IEnd,OFFSET);
    IF LoopEnd>0 THEN
    BEGIN
      INC(LoopStart,OFFSET);
      INC(Loopend,OFFSET);
      IEnd:=LoopEND;
    END;
    IF Bits AND $1F=16 THEN
    BEGIN
      OFFSET:=OFFSET SHR 1;
      LoopStart:=LoopStart SHR 1;
      Loopend:=Loopend SHR 1;
      IEnd:=IEnd SHR 1;
    END;
  END ELSE OFFSET:=0;
  UniLong:=0;
  FitGlobalMem(TmpSel,UniLong);
END;

VAR One_8:Boolean; {FÜR OKTALYZER!!!!!!!!}

FUNCTION SwapLong(l: LongInt) : LongInt; ASSEMBLER;
ASM mov al,byte Ptr l+3; mov ah,byte Ptr l+2; mov dl,byte Ptr l+1; mov dh,byte Ptr l END;



{**********************************************}
{* GET INFO MAIN PROC                         *}
{**********************************************}
FUNCTION GetChannels(aSize:LongInt):Boolean;
TYPE pByteArray=^ByteArray;
     ByteArray =ARRAY[0..3]OF RECORD Dummy,tByte:Byte; END;
BEGIN
   GetChannels  := False;                                     IF aSize<>8       THEN EXIT;
   aResult      := _lRead32(FHandle, FIs32, pBuffer, aSize);  IF aResult<>Asize THEN EXIT;
   MI.NumTracks := 0;
   FOR I:=0 TO 3 DO WITH pByteArray(pBuffer)^[I] DO CASE tByte OF
   0:BEGIN
       Inc(MI.NumTracks,1);
       CASE i OF
       0,3:DefPanning[MI.NumTracks]:=0;
       1,2:DefPanning[MI.NumTracks]:=255;
       END;
       RelatedChn[MI.NumTracks]:=MI.NumTracks;
     END;
   1:BEGIN
       Inc(MI.NumTracks,2);
       CASE i OF
       0,3:BEGIN DefPanning[MI.NumTracks]:=0;  DefPanning[MI.NumTracks-1]:=0;  END;
       1,2:BEGIN DefPanning[MI.NumTracks]:=255; DefPanning[MI.NumTracks-1]:=255; END;
       END;
       RelatedChn[MI.NumTracks-1]:=MI.NumTracks  ;
       RelatedChn[MI.NumTracks  ]:=MI.NumTracks-1;
     END;
    ELSE Exit;
   END;
   getchannels:=true;
END;


FUNCTION GetInstrData(aSize:LongInt):Boolean;
BEGIN
   GetInstrData      :=False;
   One_8             :=False;
   IF (aSize MOD (SizeOf(tOktinstrument)))<>0 THEN EXIT;
   I                 := 1;
   InstrMemSize      := 0;
   MI.NumInstruments := 0;
   REPEAT
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(tOktInstrument));
     IF aResult<>SizeOf(tOktInstrument) THEN EXIT;
     WITH pOktInstrument(pBuffer)^,MI.Instruments[i] DO
     BEGIN
       samplen:=swaplong(samplen);
       IF samplen>0 THEN
       BEGIN
         Offset:=0;
         IEnd:=samplen;
         SampRepS:=Swap(SampRepS) SHL 1;
         SampRepL:=Swap(SampRepL) SHL 1;
         IF SampRepL <= SampLen THEN
         BEGIN
           LoopStart := SampRepS;
           LoopEnd   := SampRepS+SampRepL;
         END;
         IF LoopEnd>SampLen THEN LoopEnd:=Samplen-LoopStart;
         Volume:=DefVol;
         FineTune:=0;
         IF Hi(Sampres)=1 THEN BEGIN Bits:=8; One_8:=True; END ELSE Bits:=7;
         inc(MI.NumInstruments);
         Inc(InstrMemSize,IEnd);
         TUNING:=DefMODTuning;
       END;
       sconvert(Name,sampname,20);
     END;
     INC(i);
     DEC(ASize,SizeOf(tOKTInstrument));
   UNTIL Asize<=0;
   getinstrdata:=true;
END;

FUNCTION GetInitSpeed(aSize:LongInt):Boolean;
TYPE pByteArray=^ByteArray;
     ByteArray=RECORD Dummy,tByte:Byte; END;
BEGIN
  GetInitSpeed := False;                                    IF aSize<>2       THEN EXIT;
  aResult      := _lRead32(FHandle, FIs32, pBuffer, aSize); IF aResult<>aSize THEN EXIT;
  InitSpeed    := pByteArray(pBuffer)^.tByte; IF InitSpeed=0 THEN InitSpeed:=6;
  InitBPMSpeed :=125;
  GetInitSpeed :=True;
END;

FUNCTION GetSLen(aSize:LongInt):Boolean;
BEGIN
  GetSLen     := False;                                         IF aSize<>2       THEN EXIT;
  aResult     := _lRead32(FHandle, FIs32, @LastPattern, aSize); IF aResult<>aSize THEN EXIT;
  LastPattern := Swap(LastPattern);
  GetSLen     := True;
END;

FUNCTION GetArrangeSize(aSize:LongInt):Boolean;
BEGIN
  GetArrangeSize := False;                                            IF aSize<>2       THEN EXIT;
  aResult        := _lRead32(FHandle, FIs32, @MI.NumPatterns, aSize); IF aResult<>aSize THEN EXIT;
  MI.NumPatterns := Swap(MI.NumPatterns);
  GetArrangeSize := True;
END;

FUNCTION GetArrangeMent(aSize:LongInt):Boolean;
BEGIN
  GetArrangeMent := False;                                          IF aSize>256      THEN EXIT;
  aResult        := _lRead32(FHandle, FIs32, @pArrangeMent, aSize); IF aResult<>aSize THEN EXIT;
  FillChar(PtrnTempi ,SizeOf(PtrnTempi) , 0);
  FillChar(PtrnBreaks,SizeOf(PtrnBreaks),63);
  FillChar(RowPlayed ,SizeOf(RowPlayed) , 0);
  MI.DiffPatterns:= 0;
  FOR I:=0 TO MI.NumPatterns-1 DO
  BEGIN
    IF pArrangeMent[I]>LastPattern THEN LastPattern:=pArrangeMent[I];
    IF NOT(1 IN RowPlayed[PArrangement[I]])THEN
    BEGIN
      Include(RowPlayed[PArrangement[I]],1);
      Inc(MI.DiffPatterns);
    END;
  END;
  LastTrack      := Word (Succ (LastPattern)) * MI.NumTracks;
  {TrackMemSize   := LongInt (LastTrack) * (64 * Succ (SizeOf (tNote)) + 2);{}
  GetArrangeMent := True;
END;

FUNCTION GetInfo(_FHandle:Handle32; _FIs32:WordBool; FileName:pChar; UniLoad:Boolean):Word;
VAR Ext: pChar;
LABEL OKTErr;
BEGIN
  GetInfo                := 0;
  PlayDefs.MI.CommentLen := 0;
  FHandle                := _FHandle;
  FIs32                  := _FIs32;
  MI.FileSiz             := _lSize32(FHandle, FIs32) DIV 1024;
  Ext                    := StrRScanCounted(FileName,'.',1);
  MI.FileType            := NoiseTr;
  ActiveChannelMap       := [1..32];
  IF UniLoad THEN
  BEGIN
    _lSeekAbs32(FHandle, FIs32, FirstFilePos);
    pBuffer := GlobalAllocPtr(gmem_nodiscard OR gPtr,2048);
    aResult := _lRead32(FHandle, FIs32, pBuffer, 2048);
    IF pModHeader(pBuffer)^.ModSign='M.K.'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='M!K!'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='M&K!'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='M.K!'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='FLT4'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='FLT8'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='EX04'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='EX08'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='OCTA'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='N.T.'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign='CD81'              THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign[2]
      +pModHeader(pBuffer)^.ModSign[3]
      +pModHeader(pBuffer)^.ModSign[4]='CHN'            THEN MI.FileType:=Noisetr ELSE
    IF pModHeader(pBuffer)^.ModSign[3]
      +pModHeader(pBuffer)^.ModSign[4]='CH'             THEN MI.FileType:=Noisetr ELSE
    IF pSTMHeader(pBuffer)^.StmSign='!Scream!'          THEN MI.FileType:=STM     ELSE
    IF pSTMHeader(pBuffer)^.StmSign='BMOD2STM'          THEN MI.FileType:=STM     ELSE
    IF pSTMHeader(pBuffer)^.StmSign='SWavePro'          THEN MI.FileType:=STM     ELSE
    IF pMTMHeader(pBuffer)^.Sign='MTM'                  THEN MI.FileType:=MTM     ELSE
    IF pFarHeader(pBuffer)^.FarSign='FAR'#$FE           THEN MI.FileType:=fFAR    ELSE
    IF pS3MHeader(pBuffer)^.S3MSign2='SCRM'             THEN MI.FileType:=S3M     ELSE
    IF pULTHeader(pBuffer)^.ULTSign1='MAS_UTrack_V'     THEN MI.FileType:=ULT     ELSE
    IF pDMFHeader(pBuffer)^.DMFSign='DDMF'              THEN MI.FileType:=DMF     ELSE
    IF pFT2Header(pBuffer)^.FT2Sign='Extended Module: ' THEN MI.FileType:=FT2     ELSE
    IF poktsign(pBuffer)^.fullsign='OKTASONG'           THEN MI.FileType:=OKTalyz ELSE
    IF p669Header(pBuffer)^.f669Sign=$6669  {'IF'}      THEN MI.FileType:=f669    ELSE
    IF p669Header(pBuffer)^.f669Sign=$4E4A  {'JN'}      THEN MI.FileType:=f669;
    GlobalFreePtr(pBuffer);
    _lSeekAbs32(FHandle, FIs32, FirstFilePos);
  END ELSE
  BEGIN
    IF(UpStrComp(EXT,'.NST')=0) THEN MI.FileType:=Noisetr ELSE
    IF(UpStrComp(EXT,'.MOD')=0) THEN MI.FileType:=Noisetr ELSE
    IF(UpStrComp(EXT,'.WOW')=0) THEN MI.FileType:=WOW     ELSE
    IF(UpStrComp(EXT,'.MTM')=0) THEN MI.FileType:=MTM     ELSE
    IF(UpStrComp(EXT,'.STM')=0) THEN MI.FileType:=STM     ELSE
    IF(UpStrComp(EXT,'.FAR')=0) THEN MI.FileType:=fFAR    ELSE
    IF(UpStrComp(EXT,'.S3M')=0) THEN MI.FileType:=S3M     ELSE
    IF(UpStrComp(EXT,'.ULT')=0) THEN MI.FileType:=ULT     ELSE
    IF(UpStrComp(EXT,'.DMF')=0) THEN MI.FileType:=DMF     ELSE
    IF(UpStrComp(EXT,'.XM') =0) THEN MI.FileType:=FT2     ELSE
    IF(UpStrComp(EXT,'.OKT')=0) THEN MI.FileType:=OKTALYZ ELSE
    IF(UpStrComp(EXT,'.669')=0) THEN MI.FileType:=f669    ELSE
    BEGIN GetInfo := ID_InvModFile; EXIT; END;
  END;
  CASE MI.FileType OF
{------------------------------------Oktalyzer-------------------------------------------}
   Oktalyz    :
   BEGIN
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,8192);
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(tOktSign));
     IF (aResult<>SizeOf(tOktSign))OR(pOktSign(pBuffer)^.FullSign<>'OKTASONG')THEN BEGIN GetInfo:=ID_NoModFile; EXIT; END;
     PlayDefs.MI.ModName:= StrPas(FileName);
     PlayDefs.MI.ModName:= PrevDir(PlayDefs.MI.ModName);
     PlayDefs.MI.ModName:= Copy(PlayDefs.MI.ModName,1,Pos('.',PlayDefs.MI.ModName)-1);
     REPEAT
       aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(tOktSign));
       WITH pOktSign(pBuffer)^ DO
       BEGIN
         StructSize:=SWAPLONG(StructSize);
         IF b4sign='CMOD' THEN BEGIN IF NOT(getchannels   (structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign='SAMP' THEN BEGIN IF NOT(getInstrData  (structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign='SPEE' THEN BEGIN IF NOT(getinitspeed  (structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign='SLEN' THEN BEGIN IF NOT(getSLEN       (structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign='PLEN' THEN BEGIN IF NOT(getarrangesize(structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign='PATT' THEN BEGIN IF NOT(getarrangement(structsize))THEN GOTO OKTErr; END ELSE
         IF b4sign<>'PBOD' THEN GOTO OKTErr;
       END;
     UNTIL (pOktSign(pBuffer)^.b4sign='PBOD');
     GlobalfreePtr(pBuffer);
     FastSlides := true;
     NoteMin    := 13;
     NoteMax    := 48;
     EXIT;
OKTErr:
     GlobalfreePtr(pBuffer);
     GetInfo:= ID_NoModFile;
   END;
{-----------------------------------------6669------------------------------------------------------------}
   f669       :
   BEGIN
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(T669Header));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(T669Header));
     IF(aResult<>SizeOf(T669Header))THEN BEGIN GetInfo:=ID_NoModFile; GlobalFreePtr(pBuffer); EXIT; END;
     MI.NumTracks:=8;
     WITH p669Header(pBuffer)^ DO
     BEGIN
       {FehlerCheck...}
       IF NOT (numpatts IN [1..128])OR(f669Sign<>$6669)AND(f669Sign<>$4E4A)OR not (ninstruments in [1..64])
         THEN BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
       IF f669Sign = $4E4A THEN MI.FileType := UNIS;
       Move(arrangement,parrangement,128);
       {Höchste Patternnummer finden}
       Lastpattern := numpatts-1;
       MI.DiffPatterns:=0;
       FillChar(RowPlayed,SizeOf(RowPlayed),0);
       MI.NumPatterns:=0;
       FOR i:=0 TO 127 DO
       BEGIN
         IF arrangement[i]<128 THEN MI.NumPatterns:=i+1 ELSE Break;
        {IF arrangement[i]>LastPattern THEN LastPattern:=arrangement[i];}
         IF NOT(1 IN RowPlayed[Arrangement[i]])THEN
         BEGIN
           Include(RowPlayed[Arrangement[i]],1);
           Inc(MI.DiffPatterns);
         END;
       END;
       { find dummy patterns }
       IF MI.DiffPatterns < Succ (LastPattern) THEN MI.DummyPatterns := Succ (LastPattern) - MI.DiffPatterns;
       IF(MI.NumPatterns=0){OR(lastpattern>127)}THEN BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
       LastTrack := word (Succ (LastPattern)) * MI.NumTracks;
       {TrackMemSize := LongInt (LastTrack) * (64 * Succ (SizeOf (tNote)) + 2);{}
       {Speed,Tempi,Patternbreaks}
       InitBPMSpeed:=78;
       InitSpeed:=tempos[arrangement[0]];
       MI.NumInstruments:=ninstruments;
       FOR i:=0 TO MI.NumPatterns-1 DO
       BEGIN
         PtrnTempi[i] :=Tempos [Arrangement[i]];
         PtrnBreaks[i]:=Lengths[Arrangement[i]];
       END;
       {Modname,Comment...}
       FOR i := 1 TO 3 DO
       BEGIN
         SConvert(PlayDefs.MI.ModName,comment[i],36);
         ReduceString(PlayDefs.MI.ModName);
         IF PlayDefs.MI.ModName <> '' THEN Break;
       END;
       MI.LoopTo:=RepStart+1;
       MI.LoopFrom:=-1;
       IF MI.LoopTo>MI.NumPatterns THEN MI.LoopTo:=-1 ELSE MI.LoopFrom:=MI.NumPatterns;
     END;
     MI.CommentLen:=108;
     AllocateComment (PlayDefs.MI.CommentLen);
     IF Mi.Comment=NIL THEN BEGIN GetInfo:=ID_NoMem; globalfreeptr(pbuffer); Exit; END;
     Move(p669Header(pBuffer)^.comment,tCommentBuf(MI.comment^),108);
     globalfreeptr(pbuffer);
     {*******INSTRUMENTE***********}
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(T669instrument)*(MI.NumInstruments));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(T669instrument)*(MI.NumInstruments));
     InstrMemSize:=0;
     FOR I:=MI.NumInstruments DOWNTO 1 DO WITH p669insarray(pbuffer)^.instr[i],MI.Instruments[i] DO
     BEGIN
       IF SampLen=0 THEN Dec(MI.NumInstruments) ELSE
       BEGIN
         Offset:=0;
         IEnd:=samplen;
         IF SampRepend<=SampLen THEN
         BEGIN
           LoopStart := SampRepS;
           LoopEnd   := SampRepEnd;
         END;
         Volume  :=$40; {VOl + FineTune gibts nicht}
         FineTune:=  0;
         Bits    :=  8;
         Inc(InstrMemSize,IEnd);
         IF MI.FileType=UNIS THEN Tuning := 8100 ELSE Tuning:=DefMODTuning{8740};
       END;
       sconvert(Name,sampname,13);
     END;
     IF MI.FileType=UNIS THEN
     FOR i:=1 TO MI.NumTracks DIV 2 DO
     BEGIN
       DefPanning[2*i-1]:=2*34;
       DefPanning[2*i]:=2*95;
     END ELSE
     FOR i:=1 TO MI.NumTracks DIV 2 DO
     BEGIN
       DefPanning[2*i-1]:=0;
       DefPanning[2*i]:=255;
     END;
     globalfreeptr(pbuffer);
     NoteMin    := 13;
     NoteMax    := 72;
   END;
{================================ FAR info routine by Jensi ================================}
   fFAR       :
   BEGIN
     { read first chunk }
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TFarHeader));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TFarHeader));
     IF(aResult<>SizeOf(TFarHeader))THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
     WITH PFarHeader(pBuffer)^ DO
     BEGIN
       { error check }
       IF (FarSign<>'FAR'#$FE)OR(FarSign2<>#13#10#26)THEN
       BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
       IF Version <> $10 THEN BEGIN GetInfo:=id_InvModFile; globalfreeptr(pBuffer); Exit; END;
       { get name }
       SConvert(PlayDefs.MI.ModName,FarName,40);
       ReduceString(PlayDefs.MI.ModName);
       { get channel info }
       MI.NumTracks:=16;
       ActiveChannelMap:=[];
       FOR i:=1 TO 16 DO IF ChannelMap[i] THEN Include (ActiveChannelMap, i);
       FOR i:=1 TO 16 DO DefPanning[i]:=PanningMap[i]*17;
       { set speeds }
       InitSpeed:=DefTempo;
       InitBPMSpeed:=80;
       { get comment }
       PlayDefs.MI.CommentLen:=CommentLen;
       IF CommentLen>0 THEN
       BEGIN
         AllocateComment (PlayDefs.MI.CommentLen);
         IF Mi.Comment=NIL THEN BEGIN GetInfo:=ID_NoMem; globalfreeptr(pbuffer); Exit; END;
         aResult := _lRead32(FHandle, FIs32, MI.comment, CommentLen);
         IF aResult<>CommentLen THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       END;
       unilong:=FirstFilePos+HeaderLen;
       HeaderLength:=HeaderLen;
     END;
     GlobalFreePtr(pBuffer);
     { read second chunk }
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TFarHeader2));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TFarHeader2));
     IF(aResult<>SizeOf(TFarHeader2))THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
     WITH PFarHeader2(pBuffer)^ DO
     BEGIN
       { get pattern/order info }
       Move (Orders,PArrangement,OrdLength);
       MI.NumPatterns:=OrdLength;
       Lastpattern:=NumPatts-1;
       MI.DiffPatterns:=0;
       FillChar(RowPlayed,SizeOf(RowPlayed),0);
       FOR i:=1 TO MI.NumPatterns DO
       BEGIN
         IF Orders[i]>LastPattern THEN LastPattern:=Orders[i];
         IF NOT(1 IN RowPlayed[Orders[i]])THEN
         BEGIN
           Include(RowPlayed[Orders[i]],1);
           Inc(MI.DiffPatterns);
         END;
       END;
       { find dummy patterns }
       IF MI.DiffPatterns<Succ(LastPattern)THEN MI.DummyPatterns:=Succ(LastPattern)-MI.DiffPatterns;
       { find loop IN module }
       MI.LoopTo:=LoopToLoc+1;
       MI.LoopFrom:=-1;
       IF MI.LoopTo>MI.NumPatterns THEN MI.LoopTo:=-1 ELSE MI.LoopFrom:=MI.NumPatterns;
       { skip pattern data AND calculate TrackMemSize }
       {TrackMemSize:=0;{}
       FOR i:=0 TO 255 DO
       BEGIN
         inc(unilong, PatLength[i]);
         {inc(TrackMemSize,PatLength[i] DIV 64);{}
       END;
       {TrackMemSize:=TrackMemSize*MI.NumTracks*Succ(SizeOf(tNote));{}
     END;
     GlobalFreePtr(pBuffer);
     IF _lSeekAbs32(FHandle, FIs32, UniLong)<>UniLong THEN BEGIN GetInfo:=ID_NoModFile; Exit; END;
     { get instrument info }
     aResult := _lRead32(FHandle, FIs32, @FarSampleMap, SizeOf(tFarSampleMap));
     IF(aResult<>SizeOf(TFarSampleMap))THEN BEGIN GetInfo:=id_NoModFile; Exit; END;
     inc(unilong,SizeOf(TFarSampleMap));
     pBuffer:=GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TFarInstrument));
     InstrMemSize:=0;
     FOR i:=1 TO 64 DO WITH MI.Instruments[i] DO IF Pred (i) IN FarSampleMap THEN
     BEGIN
       IF _lSeekAbs32(FHandle, FIs32, UniLong)<>UniLong THEN BEGIN GetInfo:=ID_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TFarInstrument));
       IF(aResult<>SizeOf(TFarInstrument))THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
       WITH PFarInstrument (pBuffer)^ DO
       BEGIN
         sconvert(MI.Instruments[i].Name,Name,32);
         IF Length>0 THEN
         BEGIN
           IEnd:=Length;
           IF LoopMode AND(1 SHL 3)=(1 SHL 3) THEN BEGIN LoopStart:=RepeatStart; LoopEnd:=RepeatEnd; END;
           MI.Instruments[i].FineTune:= FineTune;
           MI.Instruments[i].Volume  := Volume;
           IF SampleType AND 1=1 THEN Bits:=16 ELSE Bits:=8;
           Tuning:=DefMODTuning;
           inc(InstrMemSize,IEnd);
           inc(MI.NumInstruments);
         END;
         inc(unilong,SizeOf(TFarInstrument)+Length);
       END;
     END;
     GlobalFreePtr(pBuffer);
     { set player engine variables }
     LastTrack := word (Succ (LastPattern)) * MI.NumTracks;
     FillChar(PtrnTempi,SizeOf(PtrnTempi),0);    { pattern tempi shall NOT be supported }
     FillChar(PtrnBreaks,SizeOf(PtrnBreaks),0);  { gets overridden later }
     NoteMin    := 1;
     NoteMax    := 72;
   END;
{================================ S3M info routine by Jensi ================================}
   S3M        :
   BEGIN
   { read first chunk }
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TS3MHeader));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TS3MHeader));
     IF (aResult<>SizeOf(TS3MHeader)) THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
     WITH PS3MHeader(pBuffer)^ DO
     BEGIN
       { error check }
       IF NOT(S3MSign1 IN[#0, #$1A])OR(S3MSign2<>'SCRM')THEN
       BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       IF(FileType <> 16) OR NOT (Ffv IN [1, 2]) THEN BEGIN GetInfo:=id_InvModFile; GlobalFreePtr(pBuffer); Exit; END;
       { get name }
       SConvert(PlayDefs.MI.ModName,S3MName,28);
       ReduceString(PlayDefs.MI.ModName);
       { get info data }
       MI.NumTracks:=0;
       ActiveChannelMap:=[];
       FOR i:=1 TO 32 DO IF channels[i] AND $7F IN[0..31] THEN
       BEGIN
         inc (MI.NumTracks);
         CASE channels[i] OF
           00..07:DefPanning[i]:=  0;
           08..15:DefPanning[i]:=255;
           16..31:DefPanning[i]:=128;
         END;
         IF channels[i]AND $80 = 0 THEN Include(ActiveChannelMap,i);
       END ELSE Break;
       MI.NumPatterns:=OrdNum;
       MI.DummyPatterns:=PatNum;
       MI.NumInstruments:=InsNum;
       { take care of Flags AND Cwt_V }
       IF (Flags AND $10 <> 0)                    THEN
       BEGIN
         NoteMin    := 37;
         NoteMax    := 72;
       END ELSE
       BEGIN
         NoteMin    := 1;
         NoteMax    := 96;
       END;
       IF (Flags AND $40 <> 0) OR (Cwt_V = $1300) THEN FastSlides := true;
       { volumes AND speeds }
       InitGlobVol :=glob_vol;         IF InitGlobVol>$40 THEN InitGlobVol:=$40;
       InitSpeed   :=init_speed;
       InitBPMSpeed:=init_tempo;
       MasterVol   :=mast_vol AND $7F; IF MasterVol<$10 THEN MasterVol:=$10;
       { force mono mode IF necessary }
       IF mast_vol AND $80 = 0 THEN FOR i := 1 TO MI.NumTracks DO DefPanning[i] := 128;
       { check for default panning }
       IF (def_panpos = $FC) AND (mast_vol AND $80 <> 0)
         THEN unilong := SizeOf (TS3MHeader) + OrdNum + InsNum * 2 + PatNum * 2
         else unilong := 0;
     END;
     GlobalFreePtr(pBuffer);
   { read second chunk }
     aResult := _lRead32(FHandle, FIs32, @PArrangement, MI.NumPatterns);
     IF aResult<>MI.NumPatterns THEN BEGIN GetInfo:=id_NoModFile; Exit; END;
     { get real pattern number/arrangement }
     FillChar(RowPlayed,SizeOf(RowPlayed),0);
     LastPattern  :=0;
     MI.DiffPatterns :=0;
     FOR i:=0 TO MI.NumPatterns-1 DO IF not (PArrangement[i] in [$FE, $FF]) THEN begin
       IF PArrangement[i]>LastPattern THEN LastPattern:=PArrangement[i];
       IF NOT (1 IN RowPlayed[PArrangement[i]]) THEN
       BEGIN
         Include(RowPlayed[PArrangement[i]],1);
         inc(MI.DiffPatterns);
       END;
     END;
     for i := MI.NumPatterns-1 downto 0 do IF PArrangement[i] = $FF THEN dec (MI.NumPatterns) else break;
     { IF(LastPattern>=MI.DummyPatterns)THEN BEGIN error:=id_NoModFile; exit; END; }
     IF MI.NumPatterns = 0 THEN BEGIN GetInfo := id_NoModFile; Exit; END;
     dec(MI.DummyPatterns,MI.DiffPatterns);
   { read third chunk }
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR GPTR,MI.NumInstruments*2);
     aResult := _lRead32(FHandle, FIs32, pBuffer, MI.NumInstruments*2);
     IF aResult<>MI.NumInstruments*2 THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
     { get instrument info }
     InstrMemSize:=0;
     S3MSample:=GlobalAllocPtr(gmem_nodiscard OR GPTR,SizeOf(TS3MSample));
     FOR i:=1 TO MI.NumInstruments DO WITH MI.Instruments[i] DO
     BEGIN
       _lSeekAbs32(FHandle, FIs32, FirstFilePos+PInstruments(pBuffer)^[i]*LongInt(16));
       IF DosError<>0 THEN
       BEGIN GlobalFreePtr(S3MSample); GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       aResult := _lRead32(FHandle, FIs32, S3MSample, SizeOf(TS3MSample));
       IF aResult<>SizeOf(TS3MSample) THEN
       BEGIN GlobalFreePtr(S3MSample); GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       WITH PS3MSample(S3MSample)^ DO
       BEGIN
         { get name }
         sconvert(Name,SampleName,28);
         { get other data }
         CASE SampleType OF
           unused:dec(MI.NumInstruments);
           sample:
             BEGIN
               IF SampleSign<>'SCRS' THEN
               BEGIN GlobalFreePtr (S3MSample); GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
               Offset:=0;
               IEnd:=Length;
               IF Flags AND $01<>0 THEN BEGIN LoopStart:=LoopBeg; MI.Instruments[i].LoopEnd:=LoopEnd; END;
               MI.Instruments[i].Volume:=Volume;
               Bits:=8+2*(Flags AND $04)+(Flags AND $02)SHL 4+(Pack AND $01)SHL 6;
                         {      16 Bit         Stereo            gepackt          }
               Tuning:=C2_Speed;
               {16Bit Samples...}
               IF Flags AND $04<>0 THEN
               BEGIN
                 Inc(IEnd,IEnd);
                 IF Flags AND $01<>0 THEN
                 BEGIN
                   Inc(LoopStart,LoopStart);
                   Inc(MI.Instruments[i].LoopEnd,MI.Instruments[i].LoopEnd);
                 END;
               END;
               inc(InstrMemSize,IEnd);
             END;
           amel..ahihat:
             BEGIN
               IF SampleSign<>'SCRI' THEN
               BEGIN GlobalFreePtr (S3MSample); GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
               { pack AdLib data into unused variables }
               IEnd:=SwapLong(Length);
               LoopStart:=SwapLong(LoopBeg);
               MI.Instruments[i].LoopEnd:=SwapLong(LoopEnd);
               MI.Instruments[i].Volume :=Volume;
               Bits:=128+Ord(SampleType);
               Tuning:=C2_Speed;
             END;
           ELSE BEGIN GlobalFreePtr (S3MSample); GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
         END;
       END;
     END;
     GlobalFreePtr(S3MSample);
     GlobalFreePtr(pBuffer);
   { read fourth chunk IF one exists }
     IF unilong <> 0 THEN
     BEGIN
       _lSeekAbs32(FHandle, FIs32, FirstFilePos+UniLong);
       pBuffer := GlobalAllocPtr(gmem_nodiscard OR GPTR, MI.NumTracks);
       aResult := _lRead32(FHandle, FIs32, pBuffer, MI.NumTracks);
       IF aResult<>MI.NumTracks THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       FOR i := 1 TO MI.NumTracks DO
       BEGIN
         j := PPanPos (pBuffer)^[i];
         IF j AND $20 <> 0 THEN DefPanning[i] := (j AND $0F)*17;
       END;
       GlobalFreePtr(pBuffer);
     END;
   { set player engine variables }
     {TrackMemSize:=LongInt(MI.DiffPatterns)*MI.NumTracks*(64*Succ(SizeOf(TNote))+2);{}
     LastTrack := word (Succ (LastPattern)) * MI.NumTracks;
     FillChar(PtrnTempi,SizeOf(PtrnTempi),0);
     FillChar(PtrnBreaks,SizeOf(PtrnBreaks),63);
   END;
{================================ ULT info routine by Jensi ================================}
   ULT        :
   BEGIN
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TULTHeader));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TULTHeader));
     IF (aResult<>SizeOf(TULTHeader)) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
     WITH PULTHeader(pBuffer)^ DO
     BEGIN
       { error check }
       IF ULTSign1 <> 'MAS_UTrack_V'
         THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       Move (ULTSign2, EXT[1], 3); EXT[0] := #3;
       Val (EXT, FormatVersion, aResult);
       IF (aResult <> 0) OR (FormatVersion > 4)
         THEN BEGIN GetInfo:=id_InvModFile; GlobalFreePtr(pBuffer); Exit; END;
       { temp }
       Str (FormatVersion, MI.Instruments[255].Name);
       { get name }
       SConvert(PlayDefs.MI.ModName,ULTName,32);
       ReduceString(PlayDefs.MI.ModName);
       { get comment }
       PlayDefs.MI.CommentLen:=32 * word (CommentLen);
       IF PlayDefs.MI.CommentLen>0 THEN
       BEGIN
         AllocateComment (PlayDefs.MI.CommentLen);
         IF Mi.Comment=NIL THEN BEGIN GetInfo:=ID_NoMem; globalfreeptr(pbuffer); Exit; END;
         aResult := _lRead32(FHandle, FIs32, MI.Comment, PlayDefs.MI.CommentLen);
         IF aResult<>PlayDefs.MI.CommentLen THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       END;
     END;
     GlobalFreePtr(pBuffer);
     aResult := _lRead32(FHandle, FIs32, @MI.NumInstruments, 1);
     IF (aResult <> 1) OR (MI.NumInstruments > $7F)
       THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
     IF FormatVersion >= 4 THEN BEGIN
       pBuffer := GlobalAllocPtr (gmem_nodiscard OR gptr, MI.NumInstruments * SizeOf (TSampleStruc2));
       aResult := _lRead32(FHandle, FIs32, pBuffer, MI.NumInstruments * SizeOf (TSampleStruc2));
       IF aResult <> MI.NumInstruments * SizeOf (TSampleStruc2)THEN
       BEGIN
         GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit;
       END;
       FOR i := 1 TO MI.NumInstruments DO WITH PULTHeader2(pBuffer)^.sampls2[i], MI.Instruments[i] DO
       BEGIN
         SConvert (Name, SampleName, 32);
         WHILE (Length(Name)>0) AND (Name[Length(Name)]=#32) DO dec(byte(Name[0]));
       END;
       GlobalFreePtr(pBuffer);
     END ELSE BEGIN
       pBuffer := GlobalAllocPtr (gmem_nodiscard OR gptr, MI.NumInstruments * SizeOf (TSampleStruc));
       aResult := _lRead32(FHandle, FIs32, pBuffer, MI.NumInstruments * SizeOf (TSampleStruc));
       IF aResult <> MI.NumInstruments * SizeOf (TSampleStruc)THEN
       BEGIN
         GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit;
       END;
       FOR i := 1 TO MI.NumInstruments DO WITH PULTHeader2(pBuffer)^.samples[i], MI.Instruments[i] DO
       BEGIN
         sconvert (Name, SampleName, 32);
         WHILE (Length(Name)>0) AND (Name[Length(Name)]=#32) DO dec(byte(Name[0]));
       END;
       GlobalFreePtr(pBuffer);
     END;
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TULTHeader3));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TULTHeader3));
     IF aResult <> SizeOf(TULTHeader3) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
     WITH PULTHeader3(pBuffer)^ DO
     BEGIN
       { PatternSequence here }
       MI.NumTracks := NumChannels;
       MI.DiffPatterns := MI.NumPatterns;
       { PanPositions here }
     END;
     GlobalFreePtr(pBuffer);
   END;
{================================ DMF info routine by Jensi ================================}
   DMF        :
   BEGIN
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TDMFHeader));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TDMFHeader));
     IF aResult<>SizeOf(TDMFHeader) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
     WITH PDMFHeader(pBuffer)^ DO
     BEGIN
       { error check }
       IF DMFSign <> 'DDMF' THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
       { get name }
       SConvert(PlayDefs.MI.ModName,CompNam,20);
       ReduceString(PlayDefs.MI.ModName);
       i := Ord (PlayDefs.MI.ModName[0]);
       Move (PlayDefs.MI.ModName[1], PlayDefs.MI.ModName[31], 20);
       SConvert(PlayDefs.MI.ModName,DMFName,30);
       ReduceString(PlayDefs.MI.ModName);
       j := Ord (PlayDefs.MI.ModName[0]);
       PlayDefs.MI.ModName[0] := Chr (30 + i);
       Delete (PlayDefs.MI.ModName, j + 1, 30 - j);
       Insert ('  -  ', PlayDefs.MI.ModName, j + 1);
     END;
     WITH PDMFCompMessage(pBuffer)^ DO
     BEGIN
       { seek comment block }
       i := 0;
       WHILE Sign <> 'CMSG' DO
       BEGIN
         _lSeek32(FHandle, FIs32, I, FILE_CURRENT);
         aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf (TDMFCompMessage));
         IF aResult<>SizeOf(TDMFCompMessage) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
         I       := MsgSize;
       END;
       { get comment }
       PlayDefs.MI.CommentLen := MsgSize - 1;
       IF PlayDefs.MI.CommentLen>0 THEN
       BEGIN
         K          := _lPos32(Fhandle, FIs32);
         AllocateComment (PlayDefs.MI.CommentLen);
         IF Mi.Comment=NIL THEN BEGIN GetInfo:=ID_NoMem; Globalfreeptr(pbuffer); Exit; END;
         _lSeek32(Fhandle, FIs32, 1, FILE_CURRENT);
         aResult    := _lRead32(FHandle, FIs32, MI.Comment, PlayDefs.MI.CommentLen);
         IF aResult<>PlayDefs.MI.CommentLen THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
         FOR j := 0 TO PlayDefs.MI.CommentLen - 1 DO IF MI.comment^[j] = #0 THEN MI.comment^[j] := #32;
         _lSeekAbs32(Fhandle, FIs32, K);
       END;
       { seek pattern block }
       WHILE Sign <> 'PATT' DO
       BEGIN
         _lSeek32(FHandle, FIs32, I, FILE_CURRENT);
         aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf (TDMFCompMessage));
         IF aResult<>SizeOf(TDMFCompMessage) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
         I := MsgSize;
       END;
       {get more stuff }
       _lRead32(FHandle, FIs32, @MI.DiffPatterns, 2);
       _lRead32(FHandle, FIs32, @MI.NumTracks   , 1);
     END;
     GlobalFreePtr(pBuffer);
   END;
{------------------------------------------STM------------------------------------------------------------}
  STM        :
   BEGIN
     pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TSTMHeader));
     aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TSTMHeader));
     IF (aResult<>SizeOf(TSTMHeader))THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
     MI.NumTracks:=4;
     WITH pSTMHeader(pBuffer)^ DO
     BEGIN
       IF(numpatts>128)OR(numpatts=0)OR((stmsign<>'!Scream!')AND(stmsign<>'BMOD2STM')and(STMSign<>'SWavePro'))
         OR (STMSign2 <> #$1A) THEN
       BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
       IF FileType <> 2 THEN BEGIN GetInfo := id_InvModFile; GlobalFreePtr (PBuffer); Exit; END;
       Move(arrangement,parrangement,128);
       {Höchste Patternnummer finden}
       MI.NumPatterns := 0;
       Lastpattern := 0;
       MI.DiffPatterns:= 0;
       FillChar(RowPlayed,SizeOf(RowPlayed),0);
       FOR i:=0 TO 127 DO
       BEGIN
         IF arrangement[i]<99 THEN inc (MI.NumPatterns) ELSE Break;
         IF arrangement[i]>LastPattern THEN LastPattern:=arrangement[i];
         IF NOT(1 IN RowPlayed[Arrangement[i]])THEN
         BEGIN
           Include(RowPlayed[Arrangement[i]],1);
           Inc(MI.DiffPatterns);
         END;
       END;
       IF (lastpattern>128)OR(MI.NumPatterns=0)THEN
       BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
       MI.DummyPatterns := numpatts - MI.DiffPatterns;
       LastTrack := word (Succ (LastPattern)) * MI.NumTracks;
       SConvert(PlayDefs.MI.ModName,STMName,20);
       ReduceString (PlayDefs.MI.ModName);
       {TrackMemSize := LongInt (LastTrack) * (64 * Succ (SizeOf (tNote)) + 2);{}
       InitSpeed:=songtempo SHR 4;
       InitBPMSpeed:=125;
      {InstrumentenDaten auswerten}
       FillChar(stmsamplememofs,SizeOf(stmsamplememofs),0);
       InstrMemSize:=0;
       MI.NumInstruments:=0;
       FOR i:= 1 TO 31 DO WITH InstrBank[i] DO WITH MI.Instruments[i] DO
       BEGIN
         sconvert(Name,sampname,12);
         IF (SMemOfs <> 0) AND (SampLen > 0) THEN
         BEGIN
           STMSampleMemOfs[i]:=SMemOfs;
           Offset:=0;
           IEnd:=samplen;
           IF (sampreps <> 0)OR(samprepend <> 65535) THEN
           BEGIN
             LoopStart:=sampreps;
             LoopEnd:=samprepend;
           END;
           Volume:=SampVol;
           FineTune:=0;
           Bits:=8;
           TUNING:=NAdjust;
           Inc(MI.NumInstruments);
           Inc(InstrMemSize,IEnd);
         END;
       END;
     END;
     {Panning setzen}
     FOR i:=MI.NumTracks DOWNTO 1 DO DefPanning[i]:=128;
     GlobalFreePtr(pBuffer);
     FillChar(PtrnTempi,SizeOf(PtrnTempi),0);
     FillChar(PtrnBreaks,SizeOf(PtrnBreaks),63);
     NoteMin    := 1;
     NoteMax    := 60;
   END;
{------------------------------------------MOD------------------------------------------------------------}
  NoiseTR,WOW:
    BEGIN
      pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,SizeOf(TMODHeader));
      aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TModHeader));
      IF aResult<>SizeOf(TModHeader) THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
      aResult:=0;
      IF MI.FileType=NoiseTr THEN WITH pMODHeader(pBuffer)^ DO
      BEGIN
        IF Modsign='M.K.' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=4; END ELSE
        IF Modsign='M!K!' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=4; END ELSE
        IF Modsign='M&K!' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=4; END ELSE
        IF ModSign='M.K!' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=4; END ELSE
        IF ModSign='OCTA' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=8; END ELSE
        IF ModSign='CD81' THEN BEGIN MI.FileType:=ProTr; MI.NumTracks:=8; END ELSE
        IF Modsign='FLT4' THEN BEGIN MI.FileType:=StarTr; MI.NumTracks:=4; END ELSE
        IF Modsign='FLT8' THEN BEGIN MI.FileType:=StarTr; MI.NumTracks:=8; END ELSE
        IF ModSign='EX04' THEN BEGIN MI.FileType:=StarTr; MI.NumTracks:=4; END ELSE
        IF ModSign='EX08' THEN BEGIN MI.FileType:=StarTr; MI.NumTracks:=8; END ELSE
        IF Modsign[2]+Modsign[3]+Modsign[4]='CHN' THEN
        BEGIN MI.FileType:=Ftrk; Val(ModSign[1],MI.NumTracks,aResult); END ELSE
        IF Modsign[3]+Modsign[4]='CH' THEN
        BEGIN MI.FileType:=Ftrk; Val(ModSign[1]+ModSign[2],MI.NumTracks,aResult); END ELSE
        MI.NumTracks:=4;
      END ELSE MI.NumTracks:=8;
      IF (aResult<>0)OR(MI.NumTracks<4)OR(MI.NumTracks>32)THEN
      BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
      IF MI.FileType=NoiseTr THEN { 15 Instrumente File }
      BEGIN
      {15 Samples File ==> Header IN 31-Instr. File Header konvertieren!}
        _lSeekAbs32(FHandle, FIs32, FirstFilePos);
        _lRead32   (Fhandle, FIs32, pBuffer, SizeOf(T15ModHeader));
        pMODHeader(pBuffer)^.Arrangesize:=p15MODHeader(pBuffer)^.Arrangesize;
        pMODHeader(pBuffer)^.CIAASpeed  :=p15MODHeader(pBuffer)^.CIAASpeed;
        pMODHeader(pBuffer)^.Arrangement:=p15MODHeader(pBuffer)^.Arrangement;
        FOR i:=16 TO 31 DO WITH pMODHeader(pBuffer)^.InstrBank[i] DO
        BEGIN samplen:=0; samptune:=0; sampvol:=0; sampreps:=0; samprepl:=0; sampname[0]:=#0; END;
      END;
      WITH pModHeader(pBuffer)^ DO
      BEGIN
        IF(arrangesize>128)OR(arrangesize=0)THEN BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
        Move(arrangement,parrangement,128);
        InitBPMSpeed:=125;
        InitSpeed:=6;
        MI.NumPatterns:=arrangesize;
        {Höchste Patternnummer finden}
        LastPattern:=0;
        MI.DiffPatterns:=0;
        FillChar(RowPlayed,SizeOf(RowPlayed),0);
        FOR i:=0 TO Arrangesize-1 DO
        BEGIN
          IF arrangement[i]>LastPattern THEN LastPattern:=arrangement[i];
          IF NOT(1 IN RowPlayed[Arrangement[i]])THEN
          BEGIN
            Include(RowPlayed[Arrangement[i]],1);
            Inc(MI.DiffPatterns);
          END;
        END;
        { find dummy patterns }
        FOR i := Arrangesize TO 127 DO
          IF Arrangement[i] > MI.DummyPatterns THEN MI.DummyPatterns := Arrangement[i];
        IF MI.DummyPatterns > LastPattern THEN dec (MI.DummyPatterns, LastPattern) ELSE MI.DummyPatterns := 0;
        IF (lastpattern>127)THEN BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
        LastTrack := word (Succ (LastPattern)) * MI.NumTracks;
        SConvert(PlayDefs.MI.ModName,ModName,20);
        ReduceString (PlayDefs.MI.ModName);
        {TrackMemSize := LongInt (LastTrack) * (64 * Succ (SizeOf (tNote)) + 2);{}
       {InstrumentenDaten auswerten}
        InstrMemSize:=0;
        MI.NumInstruments:=0;
        FOR i:= 1 TO 31 DO WITH InstrBank[i] DO WITH MI.Instruments[i] DO
        BEGIN
          IF samplen>0 THEN
          BEGIN
            Offset:=0;
            IEnd:=     LongInt(Swap (samplen)) SHL 1;
            LoopStart:=LongInt(Swap (sampreps)) SHL 1;
            LoopEnd:=LoopStart+LongInt(Swap (samprepl)) SHL 1;
            Volume:=SampVol;
            FineTune:=SampTune AND $0F;
            Inc(MI.NumInstruments);
            Inc(InstrMemSize,IEnd);
            Bits:=8;
            Tuning:=DefMODTuning;
            IF NOT(UniLoad)
               AND(MI.FileType=NoiseTr)
               AND((SampTune>$F)
                 OR(SampVol>64)
                 OR(IEnd>65535)
                 OR(LoopStart>IEnd))
            THEN BEGIN GlobalFreePtr(pBuffer); GetInfo:=Id_NoModFile; Exit; END;
          END;
          sconvert(Name,sampname,22);
        END;
      END;
      {Panning setzen}
      FOR i:=MI.NumTracks DOWNTO 1 DO DefPanning[i]:=255*((i MOD 4)DIV 2);
      GlobalFreePtr(pBuffer);
      FillChar(PtrnTempi,SizeOf(PtrnTempi),0);
      FillChar(PtrnBreaks,SizeOf(PtrnBreaks),63);
    END;
{================================ FT2 ================================}
   FT2        :
    BEGIN
      _lSeekAbs32(FHandle, FIs32, FirstFilePos);
      pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,65536);
      aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TFT2Header));
      IF (aResult<>SizeOf(TFT2Header)) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
      WITH PFT2Header(pBuffer)^ DO
      BEGIN
        { error check }
        IF (FT2Sign<>'Extended Module: ')
         OR(FT2Sign2<>#$1a)
         OR NOT((NumChans IN [2..32])AND NOT Odd (NumChans))
         OR(NumPatts>256)OR(NumInstrs>128)
        THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
        { get name AND other info }
        SConvert(PlayDefs.MI.ModName,FT2Name,20);
        ReduceString(PlayDefs.MI.ModName);
        IF Flags<>0 THEN UseLinearTune:=True ELSE UseLinearTune:=False;
        MI.NumPatterns    := SongLen;
        MI.NumTracks      := NumChans;
        MI.DiffPatterns   := 0;
        MI.DummyPatterns  := 0;
        MI.NumInstruments := NumInstrs;
        InitSpeed         := DefTempo;
        InitBPMSpeed      := DefBPM;
        LastPattern       := NumPatts-1;
        LastTrack         := NumPatts*MI.NumTracks;
        Move(OrderTable,pArrangeMent,256);
        {Arrangement walk+DummyPattern detect}
        FillChar(RowPlayed,SizeOf(RowPlayed),0);
        FOR i:=0 TO SongLen-1 DO IF NOT(1 IN RowPlayed[pArrangement[i]])THEN
        BEGIN
          Include(RowPlayed[pArrangement[i]],1);
          Inc(Mi.DiffPatterns);
        END;
        FOR i:=0 TO LastPattern DO IF NOT(1 IN RowPlayed[i])THEN Inc(Mi.DummyPatterns);
        { restart position }
        MI.LoopTo:=RestartPos+1;
        MI.LoopFrom:=-1;
        IF (MI.LoopTo>MI.NumPatterns)OR(MI.LoopTo=1) THEN MI.LoopTo:=-1 ELSE MI.LoopFrom:=MI.NumPatterns;
        { skip additional data }
        _lSeekAbs32(FHandle, FIs32, FirstFilePos+60 + HeaderLen);
      END;
      { skip over pattern data }
      {TrackMemSize:=0;{}
      FOR i := 0 TO LastPattern DO WITH PFT2Pattern (pBuffer)^ DO
      BEGIN
        aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TFT2Pattern));
        IF (aResult<>SizeOf(TFT2Pattern))OR(PackType<>0)THEN
        BEGIN
          GetInfo:=id_NoModFile;
          GlobalFreePtr(pBuffer);
          Exit;
        END;
        {Inc(TrackMemSize,NumRows*Mi.NumTracks*Succ(SizeOf (tNote))+2);{}
        _lSeek32(FHandle, FIs32, HeaderLen - 9 + PackedSize, FILE_CURRENT);
      END;
      { get instrument info }
      GlobalFreePtr(pBuffer);
      IF FT2Instruments<>NIL THEN
      BEGIN
        GlobalPageUnLock(SelectorOf(FT2Instruments));
        GlobalFreePtr(FT2Instruments);
      END;
      FT2Instruments:=NIL;
      IF FT2Samples<>NIL THEN
      BEGIN
        GlobalPageUnLock(SelectorOf(FT2Samples));
        GlobalFreePtr(FT2Samples);
      END;
      FT2Samples:=NIL;
      pBuffer:=NIL;
      FT2Instruments:=GlobalAllocPtr(gmem_Nodiscard OR gmem_zeroinit,SizeOf(tFT2Instruments));
      GlobalPageLock(SelectorOf(FT2Instruments));
      FT2Samples:=GlobalAllocPtr(gmem_Nodiscard OR gmem_zeroinit,SizeOf(tFT2Samples));
      GlobalPageLock(SelectorOf(FT2Samples));
      IF (FT2Instruments=NIL)OR(FT2Samples=NIL)THEN BEGIN GetInfo:=id_NoMem; Exit; END;
      k:=1;
      InstrMemSize:=0;
      FOR i := 1 TO MI.NumInstruments DO WITH FT2Instruments^[i] DO
      BEGIN
        UniLong := _lPos32(FHandle, FIs32);
        aResult := _lRead32(FHandle, FIs32, @FT2Instruments^[i], SizeOf(TFT2Instrument));
        IF (aResult <> SizeOf (TFT2Instrument))THEN BEGIN GetInfo:=id_NoModFile; Exit; END;
        SConvert (MI.Instruments[i].Name, InstrName, 22);
        ReduceString (PlayDefs.MI.ModName);
        IF NumSamples = 0 THEN
        BEGIN
          _lSeekAbs32(FHandle, FIs32, UniLong+InstrSize);
        END ELSE
        BEGIN
          SampleSt:=k;
          {Restlichen Sampleheader lesen und Auswerten}
          aResult := _lRead32(FHandle, FIs32, @SampleHead, SizeOf(TFT2Instrument2));
          IF aResult<>SizeOf(TFT2Instrument2) THEN BEGIN GetInfo:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
          {Sampledaten einlesen}
          UniLong:= _lSeekAbs32(FHandle, FIs32, UniLong+InstrSize)+SampleHead*NumSamples;
          IF (VolType AND 1=0)OR(NumVolPts=0) THEN
          BEGIN
            VolType:=0;
            NumVolPts:=0;
            VolSustPt:=0;
            VolLoopSt:=0;
            VolLoopEnd:=0;
          END ELSE
          BEGIN
            IF (VolType AND 2=0) THEN VolSustPt:=0 ELSE Inc(VolSustPt);
            IF (VolType AND 4=0)OR(VolLoopEnd<VolLoopSt) THEN
            BEGIN
              VolLoopSt:=0;
              VolLoopEnd:=0;
            END ELSE
            BEGIN
              Inc(VolLoopSt);
              Inc(VolLoopEnd);
            END;
            IF NumVolPts>12 THEN NumVolPts:=12;
          END;
          IF (PanType AND 1=0)OR(NumPanPts=0) THEN
          BEGIN
            PanType:=0;
            NumPanPts:=0;
            PanSustPt:=0;
            PanLoopSt:=0;
            PanLoopEnd:=0;
          END ELSE
          BEGIN
            IF (PanType AND 2=0) THEN PanSustPt:=0 ELSE Inc(PanSustPt);
            IF (PanType AND 4=0)OR(PanLoopEnd<PanLoopSt)
            THEN BEGIN PanLoopSt:=0; PanLoopEnd:=0; END
            ELSE BEGIN Inc(PanLoopSt); Inc(PanLoopEnd); END;
            IF NumPanPts>12 THEN NumPanPts:=12;
          END;
          FOR j := 1 TO NumSamples DO WITH FT2Samples^[k] DO
          BEGIN
            SampOffset:= 0;
            FileOffset:= UniLong;
            aResult   := _lRead32(FHandle, FIs32, @FT2Samples^[k], SampleHead);
            IF aResult <> SampleHead THEN BEGIN GetInfo:=id_NoModFile; Exit; END;
            IF NOT UseLinearTune THEN Finetune:=FineTune SHR 4;
            Inc(LoopEnd,LoopStart);
            IF SampleType AND $3=0 THEN
            BEGIN
              LoopStart:=0;
              LoopEnd:=0;
            END;
            IF MI.Instruments[i].IEnd<SampleLen THEN
            BEGIN
              MI.Instruments[i].IEnd:=SampleLen;
              MI.Instruments[i].Bits:=8*Byte((SampleType AND $10)<>0)+8;
              MI.Instruments[i].LoopStart:=LoopStart;
              MI.Instruments[i].LoopEnd:=LoopEND;
              MI.Instruments[i].FineTune:=FineTune SHR 4;
              MI.Instruments[i].Volume:=Volume;
              MI.Instruments[i].Tuning:=DefMODTuning;
            END;
            Inc(InstrMemSize,SampleLen);
            { this is done automatically IF necessary }
            { IF ((SampleType AND 2)<>0)AND(LoopEnd>LoopStart) THEN Inc(InstrMemSize,LoopEnd-LoopStart); }
            inc(unilong,SampleLen);
            inc(k);
            IF k>MaxFT2Samples THEN BEGIN GetInfo:=id_InvModFile; Exit; END;
          END;
          _lSeekAbs32(FHandle, FIs32, UniLong);
        END;
      END;
      FillChar(PtrnTempi,SizeOf(PtrnTempi),0);
      FillChar(PtrnBreaks,SizeOf(PtrnBreaks),63);
      NoteMin    := 1;
      NoteMax    := 96;
    END;
  MTM:
{------------------------------------------MTM------------------------------------------------------------}
    BEGIN
      GetInfo:=0;
      InitBPMSpeed:=125;
      InitSpeed   :=  6;
      pBuffer := GlobalAllocPtr(gmem_nodiscard OR gptr,65536);
      aResult := _lRead32(FHandle, FIs32, pBuffer, SizeOf(TMTMHeader));
      IF aResult<>SizeOf(TMTMHeader) THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
      WITH pMTMHeader(pBuffer)^ DO
      BEGIN
        {MI.FileType testen}
        IF (Sign<>'MTM')OR(Attribute<>0)OR NOT (NTracks IN [1..32]) THEN
        BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
        FOR i:=1 TO 32 DO IF PanPostns[i]>$f THEN
        BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
        sconvert(MI.ModName,MTMName,20);
        ReduceString (PlayDefs.MI.ModName);
        PlayDefs.LastTrack      :=SavedTracks;
        PlayDefs.LastPattern    :=LastPattern;
        PlayDefs.MI.NumPatterns    :=LastToPlay+1;
        PlayDefs.MI.NumInstruments :=NumSamples;
        PlayDefs.MI.NumTracks      :=NTracks;
        {TrackMemSize               :=LongInt (LastTrack) * (BeatPerTrck * Succ (SizeOf (tNote)) + 2);{}
        PlayDefs.MI.CommentLen     :=CommentLen;
        IF CommentLen>0 THEN
        BEGIN
          AllocateComment (PlayDefs.MI.CommentLen);
          IF Mi.Comment=NIL THEN BEGIN GetInfo:=ID_NoMem; globalfreeptr(pbuffer); Exit; END;
          UniLong   :=_lPos32(FHandle, FIs32);
          _lSeekAbs32(FHandle, FIs32, FirstFilePos+194+(Numsamples*37)+(SavedTracks*LongInt(192))+
                      (Lastpattern+1)*32*2);
          IF DosError <> 0 THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
          aResult := _lRead32(FHandle, FIs32, MI.comment, CommentLen);
          IF aResult<>CommentLen THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
          _lSeekAbs32(FHandle, FIs32, unilong);
        END;
        FOR i:=1 TO 32 DO DefPanning[i]:=PanPostns[i]*17;
        IF (lastpattern>127)THEN BEGIN GetInfo:=id_NoModfile; globalfreeptr(pBuffer); Exit; END;
        FillChar(PtrnBreaks,SizeOf(PtrnBreaks),BeatPerTrck-1);
      END;
      aResult := _lRead32(FHandle, FIs32, pBuffer, MI.NumInstruments*SizeOf(tMTMInstrument));
      IF aResult<>MI.NumInstruments*SizeOf(tMTMInstrument) THEN
      BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
      InstrMemSize:=0;
      FOR i:=MI.NumInstruments DOWNTO 1 DO WITH pMTMInstrBank(pBuffer)^[i],MI.Instruments[i] DO
      BEGIN
        sconvert(Name,sampname,22);
        IF Samplen=0 THEN DEC(MI.NumInstruments) ELSE
        BEGIN
          IEnd      :=SampLen;
          LoopStart :=SampRepS;
          LoopEnd   :=SampRepE;
          FineTune  :=SampTune AND $0f;
          Volume    :=SampVol;
          Bits      :=8*(SampAttr AND $1)+8;
          Tuning    :=DefMODTuning;
          Inc(InstrMemSize,IEnd);
        END;
      END;
      {****Arrangement einlesen******}
      aResult := _lRead32(FHandle, FIs32, @PArrangement, SizeOf(tMTMArrangement));
      IF aResult<>SizeOf(tMTMArrangement) THEN BEGIN GetInfo:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
      FillChar(RowPlayed,SizeOf(RowPlayed),0);
      MI.DiffPatterns:=0;
      FOR i:=0 TO MI.NumPatterns-1 DO IF NOT(1 IN RowPlayed[PArrangement[i]])THEN
      BEGIN
        Include(RowPlayed[PArrangement[i]],1);
        Inc(MI.DiffPatterns);
      END;
      GlobalFreePtr(pBuffer);
      FillChar(PtrnTempi,SizeOf(PtrnTempi),0);
      NoteMin    := 2;
      NoteMax    := 64;
    END;
   ELSE getInfo:=Id_NoModFile;
  END;
  { for all formats! }
  Mi.NumInstruments:=0;
  FOR i:=0 TO 255 DO IF Mi.Instruments[i].IEnd>0 THEN Inc(Mi.NumInstruments);
END;

{###################################  MOD  ###############################################################}
PROCEDURE MOD_GetFileData;
TYPE  pNoteArray=^NoteArray;
      NoteArray=ARRAY[1..2048]OF TFilenote;

 FUNCTION GetMODTrack(Num,Ofs,ofs1:Word):Boolean;
 BEGIN
   reached:=False;
   FOR j:=0 TO 63 DO WITH Pattern^[1][j],pNoteArray(pBuffer)^[Ofs+j*Num+(k-ofs1)] DO
   BEGIN
     Instrument:=(byte1 AND $f0)+(byte3 SHR 4);
     TON:=(word(Byte1 AND $0f) SHL 8 + Byte2)SHL 3;
     Effekt:=byte3 AND $0f;
     Operands:=byte4;
     ChnVol:=$FF;
     IF Operands=0 THEN
     CASE Effekt OF
       $00,
       $01,
       $02,
       $0A:Effekt:=NoEffekt;
       $05,
       $06:Effekt:=Effekt-2;
     END;
     IF (Effekt=$0E)AND(Operands IN [$10,$20,$A0,$B0])THEN BEGIN Effekt:=NoEffekt;Operands:=0; END;
     IF (Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
     IF (MI.FileType = ProTr) AND (Ton <> 0) AND ((Ton > 856 shl 3) or (Ton < 113 shl 3)) THEN MI.FileType := FTrk;
   END;
   IF Reached THEN GetMODTrack:=TrackWrite(1,64) ELSE GetMODTrack:=TrackWrite(1,0);
 END;

BEGIN
  {****alle Pattern konvertieren******}
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*parrangement[i]+j;
  BufSize:=64*SizeOf(tFilenote)*MI.NumTracks;
  pBuffer:=GlobalallocPtr(gmem_nodiscard,BufSize);
  FOR i:=0 TO LastPattern DO
  BEGIN
    {Einzelnes Pattern konvertieren}
    aResult := _lRead32(FHandle, FIs32, pBuffer, BufSize);
    IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
    IF (1 IN RowPlayed[i]) THEN
    BEGIN
      IF (MI.FileType=StarTr)AND(MI.NumTracks=8)THEN
      BEGIN
        {Startrekker 8-Kanal-->erst 4 Kanäle, dann die anderen!}
        FOR k:=1 TO 4 DO
          IF NOT GetMODTrack(4,0,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
        FOR k:=5 TO 8 DO
          IF NOT GetMODTrack(4,256,4)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
      END ELSE FOR k:=1 TO MI.NumTracks DO
        IF NOT GetMODTrack(MI.NumTracks,0,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
    END ELSE IF NOT TrackWrite(MI.NumTracks,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
  END;
  ReplaceTrackNums;
 {*****Instrumentendaten einlesen****}
 {Position der Samples finden}
  UniLong:=_lSize32(FHandle, FIs32)-InstrMemSize;
  {Im MOD-File sind evtl. ZOMBIE-Patterns==>}
  {Solange ZOMBIE-Patterns aus MOD-File lesen, bis die Struktur kein Pattern}
  {sein kann==> Samplestart erreicht}
  reached:=false;
  FOR i := 1 TO MI.DummyPatterns DO
  BEGIN
    aResult := _lRead32(FHandle, FIs32, pBuffer, BufSize);
    IF aResult <> BufSize THEN BEGIN reached := true; Break; END;
    FOR j:=0 TO 63 DO FOR k:=1 TO MI.NumTracks DO
    WITH pNoteArray(pBuffer)^[j*MI.NumTracks+k] DO
      IF (byte1 SHR 4 > 1) OR (byte1 AND $0f > 6) THEN reached:=true;
    { IF dummy patterns don't look like patterns remove them only
      IF the file won't become too short }
    IF reached THEN IF _lPos32(FHandle, FIs32) <= UniLong THEN reached := false;
    IF reached THEN Break;
  END;
  GlobalFreePtr(pBuffer);
  IF Reached THEN
  BEGIN
    _lSeek32(FHandle, FIs32, -aResult, FILE_CURRENT);
    MI.DummyPatterns:=i-1;
  END;
  IF UniLong>_lPos32(FHandle,FIs32) THEN MI.ExtraBytes:=UniLong-_lPos32(FHandle,FIs32) ELSE
  IF UniLong<_lPos32(FHandle,FIs32) THEN MI.MissingBytes:=_lPos32(FHandle,FIs32)-UniLong;
  { get instrument data }
  pBuffer:=NIL;
{***Instrumente IN Speicher einlesen***}
  FOR i:=1 TO 31 DO WITH MI.Instruments[i] DO IF IEnd>0 THEN
  IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i,0,False) THEN
  BEGIN Error:=id_NoMem; Exit; END;
  IF MI.FileType IN [NoiseTr, StarTr] THEN
  BEGIN
    NoteMin    := 13;
    NoteMax    := 48;
  END ELSE
  BEGIN
    NoteMin    := 1;
    NoteMax    := 96;
  END;
END;

{###################################  MTM  ###############################################################}
PROCEDURE MTM_GetFileData;
TYPE  pNoteArray=^NoteArray;
      NoteArray=ARRAY[0..63]OF TMTMFilenote;
BEGIN
  {*********Tracks einlesen******}
  BufSize:=64*SizeOf(tMTMFilenote);
  pBuffer:=GlobalallocPtr(gmem_nodiscard,BufSize);
  IF pbuffer=NIL THEN BEGIN Error:=id_NoMem; Exit; END; {Speicher voll!}
  FOR i:=1 TO LastTrack DO
  BEGIN
    {Track konvertieren}
    aResult := _lRead32(Fhandle, FIs32, pBuffer, BufSize);
    IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
    Reached:=False;
    FOR j:=0 TO 63 DO WITH Pattern^[1][j],pNoteArray(pBuffer)^[j] DO
    BEGIN
      Instrument:=(byte1 AND $03)SHL 4+(byte2 SHR 4);
      TON:=PitchTable[Byte1 SHR 2+1+24]; IF TON=Pitchtable[25] THEN TON:=0;
      Effekt:=byte2 AND $0f;
      Operands:=byte3;
      ChnVol:=$FF;
      IF (Effekt=Enhanced)AND($10+(Operands SHR 4)=NoteDelay) THEN Inc(Operands);
      IF (Effekt=0)AND(Operands=0)THEN Effekt:=NoEffekt;
      IF (Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
    END;
    IF Reached
    THEN IF NOT TrackWrite(1,64)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END ELSE
    ELSE IF NOT TrackWrite(1,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
  END;
  GlobalFreePtr (pBuffer);
  {Sequencing Data konvertieren}
  BufSize:=(Lastpattern+1)*SizeOf(tMTMOrderData);
  pBuffer:=GlobalallocPtr(gmem_nodiscard,BufSize);
  IF pbuffer=NIL THEN BEGIN Error:=id_NoMem; Exit; END; {Speicher voll!}
  aResult := _lRead32(Fhandle, FIs32, pBuffer, BufSize);
  IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
  FOR i:=0 TO MI.NumPatterns-1 DO
    FOR j:=1 TO Mi.NumTracks DO Tracks^[i,j]:=pMTMTrackSeq(pBuffer)^[PArrangement[i],J];
  _lSeek32(FHandle, FIs32, MI.CommentLen, FILE_CURRENT);
  ReplaceTrackNums;
{***Instrumente IN Speicher einlesen***}
  IF _lSize32(FHandle, FIs32)-InstrMemSize<_lPos32(FHandle, FIs32)
   THEN MI.MissingBytes:=_lPos32(FHandle, FIs32)+InstrMemSize-_lSize32(FHandle, FIs32)ELSE
  IF _lSize32(FHandle, FIs32)-InstrMemSize>_lPos32(FHandle, FIs32)
   THEN MI.ExtraBytes:=_lSize32(FHandle, FIs32)-InstrMemSize-_lPos32(FHandle, FIs32);
  FOR i:=1 TO 31 DO WITH MI.Instruments[i] DO
  IF IEnd>0 THEN
  IF NOT LoadInstrument(Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i,2*((Bits AND $10) SHR 4)+1,False)THEN
  BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
  globalfreeptr(pbuffer); {ZwischenPuffer hat seine Schuldigkeit getan!}
END;

{###################################  669  ###############################################################}
PROCEDURE f669_GetFileData;
TYPE  pNoteArray=^NoteArray;
      NoteArray=ARRAY[1..2048]OF T669note;
BEGIN
  {****alle Pattern konvertieren******}
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*parrangement[i]+j;
  BufSize:=64*SizeOf(t669note)*MI.NumTracks;
  pBuffer:=GlobalallocPtr(gmem_nodiscard,BufSize);
  FOR i:=0 TO LastPattern DO
  BEGIN
    {Einzelnes Pattern konvertieren}
    aResult := _lRead32(Fhandle, FIs32, pBuffer, BufSize);
    IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
    IF 1 IN RowPlayed[i] THEN FOR k:=1 TO MI.NumTracks DO
    BEGIN
      Reached:=False;
      FOR j:=0 TO 63 DO WITH Pattern^[1][j],pNoteArray(pBuffer)^[j*MI.NumTracks+k] DO
      BEGIN
        IF Byte1<>$FF THEN
        BEGIN
          ChnVol:=(byte2 AND $0f)SHL 2+(byte2 AND $0f)SHR 2+(byte2 AND $0f)SHR 3;
          IF Byte1<>$FE THEN
          BEGIN
            TON:=PitchTable[byte1 SHR 2+1+24];
            Instrument:=(Byte2 SHR 4)+(Byte1 AND $03)SHL 4+1;
          END ELSE BEGIN TON:=0; Instrument:=0; END;
        END ELSE BEGIN TON:=0; Instrument:=0; ChnVol:=$FF; END;
        IF (byte3<>$FF)THEN BEGIN
          Operands := byte3 AND $0F;
          CASE (byte3 SHR 4) OF
            0 : Effekt:=Portamento_up;
            1 : Effekt:=Portamento_down;
            2 : Effekt:=Tone_portamento;
            3 : Effekt:=FrequAdjust;
            4 : Effekt:=Vibrato;
            5 : Effekt:=SetSpeed;
            6 : CASE byte3 AND $0F OF
                  0: Effekt := FinePanLeft;
                  1: Effekt := FinePanRight;
                  ELSE Effekt := NoEffekt;
                END;
            7 : Effekt := SlotRetrigger;
            ELSE Effekt:=NoEffekt;
          END;
        END ELSE Effekt:=NoEffekt;
        IF (ChnVol<>$FF)OR(Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
      END;
      IF Reached
      THEN IF NOT TrackWrite(1,64) THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END ELSE
      ELSE IF NOT TrackWrite(1,0)  THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
    END ELSE IF NOT TrackWrite(MI.NumTracks,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
  END;
  GlobalFreePtr(pBuffer);
  pBuffer:=NIL;
  ReplaceTrackNums;
{***Instrumente IN Speicher einlesen***}
  FOR i:=1 TO 64 DO WITH Mi.Instruments[i] DO
  IF IEnd>0 THEN IF NOT LoadInstrument(Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i, 1,False)THEN
  BEGIN Error:=id_NoMem; Exit; END;
END;

{*****************************OKTALYZER***************************************************}
PROCEDURE okt_getfiledata;
VAR  pattlen:word;
TYPE pNoteArray=^NoteArray;
     NoteArray=ARRAY[1..2048]OF ToktNote;

BEGIN
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*Parrangement[i]+j;
  pBuffer:=GlobalallocPtr(gmem_nodiscard,65535); IF pbuffer=NIL THEN BEGIN Error:=id_NoMem; Exit; END;
  _lSeek32(FHandle, FIs32, -8, FILE_CURRENT);
  FOR I:=0 TO LastPattern DO
  BEGIN
    aResult := _lRead32(Fhandle, FIs32, pBuffer, SizeOf(toktsign));
    pOktSign(pBuffer)^.structsize:=SWAPLONG(pOktSign(pBuffer)^.structsize);
    IF pOktSign(pBuffer)^.b4sign='PBOD' THEN
    BEGIN
      _lRead32(Fhandle, FIs32, @Pattlen, 2);
      Pattlen := Swap(Pattlen);
      BufSize := pOktSign(pBuffer)^.structsize-2;
      aResult := _lRead32(Fhandle, FIs32, pBuffer, BufSize);
      IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
      IF 1 IN RowPlayed[i] THEN
      BEGIN
        FOR j:=0 TO MI.NumPatterns-1 DO IF Parrangement[j]=i THEN PtrnBreaks[j]:=PattLen-1;
        {Auswertung der Pattern-Daten}
        FOR k:=1 TO MI.NumTracks DO
        BEGIN
          FillChar(Pattern^,SizeOf(tTrack),0);
          Reached:=False;
          FOR j:=0 TO (PattLen-1) DO WITH Pattern^[1][j],pNoteArray(pBuffer)^[j*MI.NumTracks+k] DO
          BEGIN
            ChnVol:=$FF;
            Operands:=byte4;
            { ==> check out arpeggios AND pattern jump; old volume is probably wrong! <== }
            CASE byte3 OF
          {2} $2 : Effekt := Portamento_down; { rs_portd-p   }
          {1} $1 : Effekt := Portamento_up  ; { rs_portu-p   }
          {A} $A : Effekt := OktArpg2       ; { rs_arp-p     }{org down(arg1) org up(arg2) org}
          {B} $B : Effekt := OktArpg1       ; { rs_arp2-p    }{org up(arg1) org down(arg2) org}
          {C} $C : Effekt := Arpeggio       ; { rs_arp3-p    }{up(arg1) up(arg2) org}
          {D} $D : Effekt := OktSldDwn      ; { kontinuierliches Slide Down Halbtonschritte}
          {F} $F : Effekt := SetFilter      ; { set filter }
          {H}$11 : Effekt := OktSld1Up      ; {Slide up   Once - sofort}
          {L}$15 : Effekt := OktSld1Dwn     ; {Slide Down Once - sofort}
          {O}$18 : Effekt := OktOldVol      ; {Set Old Volume - bei SetVol altes vol merken}
          {P}$19 : BEGIN Effekt:=PositionJump; Operands:=(Operands AND $0F)+(Operands SHR 4)*10+1; END;
          {R}$1B : Effekt := RetrigNote;
          {S}$1C : Effekt := SetSpeed;       { p-rs_cspeed  }
          {U}$1E : Effekt := OktSldUp;       {Slide Up continuierlich}
          {V}$1F : BEGIN                     { rs_volume-p  }
                     IF Operands <= $40 THEN Effekt := SetVolume
                     ELSE IF Operands<$50 THEN BEGIN Effekt:=VolumeSlide; Operands:= Operands-$40; END
                     ELSE IF Operands<$60 THEN BEGIN Effekt:=VolumeSlide; Operands:=(Operands-$50) SHL 4; END
                     ELSE IF Operands<$70 THEN BEGIN Effekt:=FineVolDwn;  Operands:= Operands-$60; END
                     ELSE IF Operands<$80 THEN BEGIN Effekt:=FineVolUp;   Operands:= Operands-$70; END;
                   END;
          {E,G,I,J,K,M,N,Q,T,W,X,Y,Z - nichts}
              ELSE Effekt:=NoEffekt;
            END;
            IF byte1<>0 THEN
            BEGIN
              Ton        := PitchTable[byte1+12+24];
              Instrument := byte2+1;
            END;
            IF (Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
          END;
          IF Reached
          THEN IF NOT TrackWrite(1,Pattlen)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END ELSE
          ELSE IF NOT TrackWrite(1,0)      THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
        END;
      END ELSE IF NOT TrackWrite(MI.NumTracks,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
    END ELSE BEGIN Error:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
  END;
  ReplaceTrackNums;
  {Lesen der Dummy-Pattern}
  REPEAT
    aResult := _lRead32(Fhandle, FIs32, pBuffer, SizeOf(toktsign));
    IF pOktSign(pBuffer)^.b4sign='PBOD'
    THEN _lSeek32(FHandle, FIs32, SWAPLONG(pOktSign(pBuffer)^.structsize), FILE_CURRENT)
    ELSE IF pOktSign(pBuffer)^.b4sign='SBOD' THEN Break
    ELSE BEGIN Error:=id_NoModFile; GlobalFreePtr(pBuffer); Exit; END;
  UNTIL (pOktSign(pBuffer)^.b4sign='SBOD')OR(_lEOF32(FHandle, FIs32));
  _lSeek32(FHandle, FIs32, -8, FILE_CURRENT);
  {Laden der Instrumente}
  I:=0;
  REPEAT
    INC(i); IF I>255 THEN BREAK;
    IF MI.Instruments[i].IEnd>0 THEN
    BEGIN
      aResult := _lRead32(Fhandle, FIs32, pBuffer, SizeOf(toktsign));
      {IF aResult=0 THEN Break;}
      poktsign(pBuffer)^.structsize:=SWAPLONG(poktsign(pBuffer)^.structsize);
      IF poktsign(pBuffer)^.b4sign='SBOD' THEN WITH MI.Instruments[i] DO
      BEGIN
        iend:=poktsign(pBuffer)^.structsize;
        IF (Bits AND $1F = 7) AND NOT (One_8)
          THEN IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i, 4,False)THEN
               BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END ELSE
          ELSE IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i, 0,False)THEN
               BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
      END ELSE Break;
    END;
  UNTIL _lEOF32(FHandle, FIs32);
  GlobalFreePtr(pBuffer);
END;

{=============================== FAR loader routine by Jensi ===============================}
PROCEDURE FAR_GetFileData;
VAR
  FarPatternData: PFarPatternData;
  FarHeader2: PFarHeader2;
  PatternLength: word;
BEGIN
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*PArrangement[i]+j;
  { read pattern data }
  _lSeekAbs32(FHandle, FIs32, FirstFilePos+SizeOf(TFarHeader)+MI.CommentLen);
  IF DosError<>0 THEN BEGIN Error:=id_NoModFile; Exit; END;
  FarHeader2 := GlobalAllocPtr(gmem_nodiscard,SizeOf(TFarHeader2));
  aResult    := _lRead32(Fhandle, FIs32, FarHeader2, SizeOf(TFarHeader2));
  IF aResult<>SizeOf(TFarHeader2) THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(FarHeader2); Exit; END;
  { skip extra data }
  _lSeekAbs32(FHandle, FIs32, FirstFilePos+HeaderLength);
  IF DosError<>0 THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(FarHeader2); Exit; END;
  { convert patterns }
  FarPatternData:=GlobalAllocPtr(gmem_nodiscard, SizeOf(TFarPatternData));
  FOR i:=0 TO LastPattern DO
  BEGIN
    PatternLength:=FarHeader2^.PatLength[i];
    IF (PatternLength>0) THEN
    BEGIN
      IF PatternLength>SizeOf(TFarPatternData)THEN
      BEGIN Error:=id_NoModFile; GlobalFreePtr(FarHeader2); GlobalFreePtr(FarPatternData); Exit; END;
      aResult := _lRead32(Fhandle, FIs32, FarPatternData, PatternLength);
      IF aResult<>PatternLength THEN
      BEGIN Error:=id_NoModFile; GlobalFreePtr(FarHeader2); GlobalFreePtr(FarPatternData); Exit; END;
      { convert single pattern }
      FOR j:=0 TO MI.NumPatterns-1 DO IF PArrangement[j]=i THEN PtrnBreaks[j]:=Succ(FarPatternData^.BreakLoc);
      IF 1 IN RowPlayed[i]THEN
      BEGIN
        FOR k:=1 TO MI.NumTracks DO {IF NOT(k IN FarChannelMap)THEN
        BEGIN
          IF NOT TrackWrite(1,0)THEN
          BEGIN
            Error:=id_NoMem;
            GlobalFreePtr(FarHeader2);
            GlobalFreePtr(FarPatternData);
            Exit;
          END;
        END ELSE}
        BEGIN
          Reached:=False;
          FOR j:=0 TO Pred((PatternLength-2)DIV 64)DO WITH Pattern^[1,j],FarPatternData^.PtrnData[j,k] DO
          BEGIN
            IF b3<>0 THEN ChnVol:=Pred(b3) SHL 2+Pred(b3)SHR 2+Pred(b3) SHR 3 ELSE ChnVol:=$FF;
            IF b1=0 THEN BEGIN Ton:=0; Instrument:=0; END
            ELSE BEGIN Ton:=PitchTable[b1+12+24]; Instrument:=Succ(b2); END;
            { process effects }
            Operands := b4 AND $0F;
            CASE b4 SHR 4 OF
              $00: CASE b4 AND $0F OF
                     $00: Effekt:=NoEffekt;
                     ELSE Effekt:=FarSpecial; { not supported }
                   END;
              $01: Effekt:=FarSldUp;
              $02: Effekt:=FarSldDwn;
              $03: Effekt:=FarPortamento;
              $04: Effekt:=FarRetrig;
              $05: Effekt:=VibratoDepth;
              $06: Effekt:=FarVibrato;
              $07: Effekt:=FarVolUp;
              $08: Effekt:=FarVolDwn;
              $09: Effekt:=VibratoSust;
              $0A: Effekt:=VolumePort;
              $0B: Effekt:=EPanning;
              $0C: BEGIN Effekt:=NoteDelay; Inc(Operands); END;
              $0D: Effekt:=FineTempoDwn;
              $0E: Effekt:=FineTempoUp;
              $0F: Effekt:=SetSpeed;
            END;
            IF (ChnVol<>$FF)OR(Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
          END;
          IF Reached
          THEN IF NOT TrackWrite(1,(PatternLength-2)DIV 64)THEN
          BEGIN
            Error:=id_NoMem;
            GlobalFreePtr(FarHeader2);
            GlobalFreePtr(FarPatternData);
            Exit;
          END ELSE ELSE
          IF NOT TrackWrite(1,0)THEN
          BEGIN
            Error:=id_NoMem;
            GlobalFreePtr(FarHeader2);
            GlobalFreePtr(FarPatternData);
            Exit;
          END;
        END;
      END ELSE IF NOT TrackWrite (MI.NumTracks, 0)THEN
      BEGIN
        Error:=id_NoMem;
        GlobalFreePtr(FarHeader2);
        GlobalFreePtr(FarPatternData);
        Exit;
      END;
    END ELSE IF NOT TrackWrite (MI.NumTracks, 0) THEN
    BEGIN
      Error:=id_NoMem;
      GlobalFreePtr(FarHeader2);
      GlobalFreePtr(FarPatternData);
      Exit;
    END;
  END;
  GlobalFreePtr (FarPatternData);
  ReplaceTrackNums;
  { skip dummies after LastPattern }
  unilong:=0; FOR i:=Succ(LastPattern) TO 255 DO inc(unilong,FarHeader2^.PatLength[i]);
  GlobalFreePtr (FarHeader2);
  { skip sample map }
  _lSeek32(FHandle, FIs32, unilong+SizeOf(TFarSampleMap), FILE_CURRENT);
  IF DosError<>0 THEN BEGIN Error:=id_NoModFile; Exit; END;
  { convert instruments }
  pBuffer:=NIL;
{***Instrumente IN Speicher einlesen***}
  FOR i:=1 TO 64 DO WITH MI.Instruments[i] DO IF Pred (i) IN FarSampleMap THEN
  BEGIN
    { discard header }
    _lSeek32(FHandle, FIs32, SizeOf (TFarInstrument), FILE_CURRENT);
    IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i, 2*((Bits AND $10) SHR 4),False)THEN
    BEGIN Error:=id_NoMem; Exit; END;
  END;
END;

{###################################  STM  ###############################################################}
PROCEDURE STM_GetFileData;
TYPE  pNoteArray=^NoteArray;
      NoteArray=ARRAY[1..2048]OF TFilenote;
BEGIN
  {****alle Pattern konvertieren******}
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*parrangement[i]+j;
  BufSize:=64*SizeOf(tFilenote)*MI.NumTracks;
  pBuffer:=GlobalallocPtr(gmem_nodiscard,BufSize);
  FOR i:=0 TO LastPattern DO
  BEGIN
    {Einzelnes Pattern konvertieren}
    IF i >= MI.DummyPatterns + MI.DiffPatterns THEN
    BEGIN
      IF NOT TrackWrite (MI.NumTracks, 0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
      Continue;
    END;
    aResult := _lRead32(Fhandle, FIs32, pBuffer, BufSize);
    IF aResult<>BufSize THEN BEGIN Error:=id_NoModFile; globalfreeptr(pBuffer); Exit; END;
    IF 1 IN RowPlayed[i] THEN FOR k:=1 TO MI.NumTracks DO
    BEGIN
      Reached:=False;
      FOR j:=0 TO 63 DO WITH Pattern^[1][j],pNoteArray(pBuffer)^[j*MI.NumTracks+k] DO
      BEGIN
        CASE byte1 OF
          $FE: Ton := $FFFF;
          $FF: Ton := 0;
          ELSE IF (byte1 SHR 4 > 7) OR (byte1 AND $0F > 11) THEN Ton := 0
               ELSE Ton := PitchTable[byte1 SHR 4 * 12 + byte1 AND $0F + 1+24];
        END;
        Instrument:=byte2 SHR 3;
        ChnVol:=((byte3 AND $F0) SHR 1)+(byte2 AND $07);
        Operands:=byte4;
        CASE byte3 AND $0F OF
          0{.}:Effekt:=NoEffekt;
          1{A}:BEGIN Effekt:=SetSpeed; Operands:=byte4 SHR 4; END;
          2{B}:Effekt:=PositionJump;
          3{C}:Effekt:=PatternBreak;
          4{D}:Effekt:=VolumeSlide;
          5{E}:Effekt:=Portamento_Down;
          6{F}:Effekt:=Portamento_Up;
          7{G}:Effekt:=Tone_portamento;
          8{H}:Effekt:=Vibrato;
          9{I}:Effekt:=Tremor;
         10{J}:Effekt:=Arpeggio;
          ELSE Effekt:=NoEffekt;
          (*
          11{K}:Effekt:=VibraVolSlid;
          12{L}:Effekt:=Tone_Portamento; { operands different }
          13{M}:Effekt:=Tremolo;
          14{N}:Effekt:=PlayOffset;
          15{O}:Effekt:=PlayOffset;
          *)
        END;
        {Fehlerbehandlung}
        IF ChnVol>64 THEN ChnVol:=$FF;
        IF (ChnVol<>$FF)OR(Effekt<>NoEffekt)OR(Operands<>0)OR(Ton<>0)OR(Instrument<>0)THEN Reached:=True;
      END;
      IF Reached
      THEN IF NOT TrackWrite(1,64)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END ELSE
      ELSE IF NOT TrackWrite(1,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
    END ELSE IF NOT TrackWrite(MI.NumTracks,0)THEN BEGIN Error:=id_NoMem; globalfreeptr(pBuffer); Exit; END;
  END;
  GlobalFreePtr(pBuffer);
  IF NOT TrackWrite(MI.NumTracks,0)THEN BEGIN Error:=id_NoMem; Exit; END;
  pBuffer:=NIL;
  ReplaceTrackNums;
{***Instrumente IN Speicher einlesen***}
  FOR i:=1 TO 31 DO WITH MI.Instruments[i] DO IF IEnd>0 THEN
  BEGIN
    _lSeekAbs32(FHandle, FIs32, FirstFilePos+LongInt(16)*LongInt(stmsamplememofs[i]));
    IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i,0,False)THEN
    BEGIN Error:=id_NoMem; Exit; END;
  END;
END;

{=============================== S3M loader routine by Jensi ===============================}
PROCEDURE S3M_GetFileData;
VAR
  S3MHeader:      PS3MHeader;
  S3MInstruments: PInstruments;
  S3MPatterns:    PPatterns;
  S3MPatternData: PS3MPatternData;
  l:    word;
  what: byte;
LABEL S3MLoadErr,S3MLoadErr1;
BEGIN
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO IF not (PArrangement[i] in [$FE, $FF])
    THEN Tracks^[i][j]:=MI.NumTracks*PArrangement[i]+j
    else Tracks^[i][j]:=0;
  { read header }
  _lSeekAbs32(FHandle, FIs32, FirstFilePos);
  IF DosError<>0 THEN BEGIN Error:=id_NoModFile; Exit; END;
  S3MHeader := GlobalAllocPtr(gmem_nodiscard,SizeOf(TS3MHeader));
  aResult   := _lRead32(Fhandle, FIs32, S3MHeader, SizeOf(TS3MHeader));
  IF aResult<>SizeOf(TS3MHeader) THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(S3MHeader); Exit; END;
  WITH S3MHeader^ DO
  BEGIN
    { read pattern pointers }
    _lSeekAbs32(FHandle, FIs32, FirstFilePos+SizeOf(TS3MHeader)+OrdNum+InsNum*2);
    IF DosError<>0 THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(S3MHeader); Exit; END;
    S3MPatterns := GlobalAllocPtr(gmem_nodiscard,PatNum*2);
    aResult     := _lRead32(Fhandle, FIs32, S3MPatterns, PatNum*2);
    IF aResult<>PatNum*2 THEN
    BEGIN Error:=id_NoModFile; GlobalFreePtr(S3MPatterns); GlobalFreePtr(S3MHeader); Exit; END;
    { convert patterns }
    S3MPatternData:=GlobalAllocPtr(gmem_nodiscard,SizeOf(TS3MPatternData));
    FOR i:=0 TO LastPattern DO IF (1 IN RowPlayed[i])AND(i<MI.DummyPatterns+MI.DiffPatterns)AND(S3MPatterns^[i]<>0)THEN
    BEGIN
      { read single (packed) pattern }
      _lSeekAbs32(FHandle, FIs32, FirstFilePos+LongInt (S3MPatterns^[i]) SHL 4);
      IF DosError<>0 THEN
      BEGIN
S3MLoadErr:
        Error:=id_NoModFile;
        GlobalFreePtr(S3MPatternData);
        GlobalFreePtr(S3MPatterns);
        GlobalFreePtr(S3MHeader);
        Exit;
      END;
      aResult := _lRead32(Fhandle, FIs32, S3MPatternData, SizeOf(TS3MPatternData));
      IF aResult<S3MPatternData^.Length THEN GOTO S3MLoadErr;
      { convert single pattern }
      IF S3MPatternData^.Length>SizeOf(TS3MPatternData) THEN GOTO S3MLoadErr;
      { ^^ this is unnecessary ^^ }
      FOR j:=0 TO 63 DO FOR k:=1 TO MI.NumTracks DO WITH Pattern^[k,j] DO
      BEGIN
        Instrument:=0;
        Ton:=0;
        Effekt:=NoEffekt;
        Operands:=0;
        ChnVol:=$FF;
      END;
      l:=1;
      FOR j:=0 TO 63 DO WITH S3MPatternData^ DO
      BEGIN
        WHILE PackedData[l]<>0 DO
        BEGIN
          what:=PackedData[l];
          k:=Succ(what AND $1F);
          IF k > MI.NumTracks THEN
          BEGIN
            { skip dummy data }
            IF what AND $20<>0 THEN inc(l,2);
            IF what AND $40<>0 THEN inc(l,1);
            IF what AND $80<>0 THEN inc(l,2);
          END ELSE WITH Pattern^[k,j] DO
          BEGIN
            IF what AND $20<>0 THEN
            BEGIN
              CASE PackedData[l+1] OF
                $00..$7F: IF PackedData[l+1] AND $0F > 11 THEN Ton := 0 else
                     Ton := PitchTable[PackedData[l+1] SHR 4*12 + PackedData[l+1] AND $0F + 1];
                $FE: Ton := $FFFF;
                $FF: Ton := 0;
                ELSE Ton := 0;
              END;
              Instrument := PackedData[l+2];
              inc(l,2);
            END;
            IF what AND $40<>0 THEN BEGIN ChnVol:=PackedData[l+1]; inc(l); END;
            IF what AND $80<>0 THEN
            BEGIN
              Effekt:=PackedData[l+1];
              Operands:=PackedData[l+2];
              inc(l,2);
              CASE Chr(Pred(Effekt)+Ord('A')) OF
                'A': Effekt:=SetSpeed;
                'B': Effekt:=PositionJump;
                'C': Effekt:=PatternBreak;
                'D': Effekt:=VolumeSlide;
                'E': Effekt:=Portamento_Down;
                'F': Effekt:=Portamento_Up;
                'G': Effekt:=Tone_Portamento;
                'H': Effekt:=Vibrato;
                'I': Effekt:=Tremor;
                'J': Effekt:=Arpeggio;
                'K': Effekt:=VibraVolslid;
                'L': Effekt:=PortVolslide;
                {'M', 'N': nothing}
                'O': Effekt:=PlayOffset;
                {'P': nothing}
                'Q': Effekt:=RetrigVolSlide;
                'R': Effekt:=Tremolo;
                'S': BEGIN
                       CASE Operands SHR 4 OF
                         $00: Effekt:=SetFilter;
                         $01: Effekt:=GlissdCtrl;
                         $02: Effekt:=FineTune;
                         $03: Effekt:=VibraWave;
                         $04: Effekt:=TremWave;
                         {5, 6, 7: nothing}
                         $08: Effekt:=EPanning;
                         {9: nothing}
                         $0A: Effekt:=StereoControl;
                         $0B: Effekt:=PattrnLoop;
                         $0C: Effekt:=NoteCut;
                         $0D: BEGIN Effekt:=NoteDelay; Inc(Operands); END;
                         $0E: Effekt:=PattrnDelay;
                         $0F: Effekt:=InvLoop;
                         ELSE Effekt:=NoEffekt;
                       END;
                       Operands := Operands AND $0F;
                     END;
                'T': Effekt:=SetBPMSpeed;
                'U': Effekt:=FineVibrato;
                'V': Effekt:=SetGlobVol;
                {'W': nothing}
                'X': Effekt:=Panning;
                {'Y', 'Z': nothing}
                ELSE Effekt:=NoEffekt;
              END;
            END;
          END;
          inc (l);
          IF l>Length-1 THEN Break;
        END;
        inc (l);
        IF l>Length-1 THEN Break;
      END;
      {--- TEMP Remove multiple Pattern Loops (KLF_CHIB.S3M 1994)---}
      FOR j:=0 TO 63 DO
      BEGIN
        L:=0;
        FOR k:=1 TO MI.NumTracks DO WITH Pattern^[k,j] DO
        IF (Effekt=PattrnLoop)AND(Operands<>0) THEN
        BEGIN
          IF(Operands>L) THEN
          BEGIN
            IF k>1 THEN FOR L:=1 TO K-1 DO WITH Pattern^[l,j] DO BEGIN Effekt:=NoEffekt; Operands:=0; END;
            L:=Operands;
          END ELSE
          BEGIN
            Effekt:=NoEffekt;
            Operands:=0;
          END;
        END;
      END;
      {--- END TEMP ---}
      IF NOT TrackWrite(MI.NumTracks,64)THEN GOTO S3MLoadErr;
    END ELSE IF NOT TrackWrite (MI.NumTracks, 0)THEN GOTO S3MLoadErr;
    GlobalFreePtr(S3MPatternData);
    GlobalFreePtr(S3MPatterns);
    ReplaceTrackNums;
    { read instrument pointers }
    _lSeekAbs32(FHandle, FIs32, FirstFilePos+SizeOf(TS3MHeader)+OrdNum);
    IF DosError<>0 THEN BEGIN Error:=id_NoModFile; GlobalFreePtr(S3MHeader); Exit; END;
    S3MInstruments := GlobalAllocPtr(gmem_nodiscard,InsNum*2);
    aResult        := _lRead32(Fhandle, FIs32, S3MInstruments, InsNum*2);
    IF aResult<>InsNum*2 THEN
    BEGIN Error:=id_NoModFile; GlobalFreePtr(S3MInstruments); GlobalFreePtr(S3MHeader); Exit; END;
    S3MSample := GlobalAllocPtr (gmem_nodiscard, SizeOf (TS3MSample));
    { convert instruments }
    pBuffer:=NIL;
  {***Instrumente IN Speicher einlesen***}
    FOR i:=1 TO InsNum DO IF(MI.Instruments[i].IEnd>0)AND(MI.Instruments[i].Bits<128)THEN WITH MI.Instruments[i] DO
    BEGIN
      { find instrument }
      _lSeekAbs32(FHandle, FIs32, FirstFilePos+LongInt(S3MInstruments^[i])SHL 4);
      IF (DosError<>0)OR Boolean(BITS AND $40)THEN
      BEGIN
S3MLoadErr1:
        Error := id_NoModFile;
        GlobalFreePtr(S3MSample);
        GlobalFreePtr(S3MInstruments);
        GlobalFreePtr(S3MHeader);
        Exit;
      END;
      aResult := _lRead32(Fhandle, FIs32, S3MSample, SizeOf(TS3MSample));
      IF aResult<>SizeOf(TS3MSample) THEN GOTO S3MLoadErr1;
      _lSeekAbs32(FHandle, FIs32, FirstFilePos+LongInt(S3MSample^.MemSeg.w1) SHL 4);
      IF DosError<>0 THEN GOTO S3MLoadErr1;
      { read instrument }
      IF NOT LoadInstrument (Name,Offset,IEnd,LoopStart,LoopEnd,Volume,Bits,i, 2*((Bits AND $10)SHR 4)+(Ffv-1),False)THEN
      BEGIN
        Error:=id_NoMem;
        GlobalFreePtr(S3MSample);
        GlobalFreePtr(S3MInstruments);
        GlobalFreePtr(S3MHeader);
        Exit;
      END;
    END;
    GlobalFreePtr(S3MSample);
    GlobalFreePtr(S3MInstruments);
  END;
  GlobalFreePtr(S3MHeader);
END;

{###################################  FT2  loader by Kay (Wer will das nur wissen?) ##########################}
PROCEDURE FT2_GetFileData;
VAR EndOfData:Word;
    PatLen:Word;
    CurrFlags:Byte;
    CurrPtr:pByte;
BEGIN
  FOR i:=0 TO MI.NumPatterns-1 DO FOR j:=1 TO MI.NumTracks DO Tracks^[i][j]:=MI.NumTracks*PArrangement[i]+j;
  pBuffer:=GlobalAllocPtr(gmem_nodiscard,65536); IF pBuffer=NIL THEN BEGIN Error:=ID_NoMem; Exit; END;
  _lSeekAbs32(FHandle, FIs32, FirstFilePos);
  aResult := _lRead32(Fhandle, FIs32, pBuffer, SizeOf(TFT2Header));
  _lSeekAbs32(FHandle, FIs32, FirstFilePos+60+PFT2Header(pBuffer)^.HeaderLen);
  { read pattern data }
  FOR k:=0 TO LastPattern DO
  BEGIN
    FOR j:=0 TO 255 DO FOR i:=1 TO MI.NumTracks DO WITH Pattern^[i,j] DO
    BEGIN
      Instrument:=0;
      Ton:=0;
      Effekt:=NoEffekt;
      Operands:=0;
      ChnVol:=0;
    END;
    aResult := _lRead32(Fhandle, FIs32, pBuffer, SizeOf (TFT2Pattern));
    {Convert Pattern...}
    PatLen:=PFT2Pattern(pBuffer)^.NumRows;
    FOR j:=0 TO MI.NumPatterns-1 DO IF Parrangement[j]=k THEN PtrnBreaks[j]:=PatLen-1;
    EndOfData:=PFT2Pattern(pBuffer)^.PackedSize;
    _lSeek32(FHandle, FIs32, PFT2Pattern(pBuffer)^.HeaderLen - 9, FILE_CURRENT);
    IF EndOfData>0 THEN
    BEGIN
      aResult := _lRead32(Fhandle, FIs32, pBuffer, EndOfData);
      i:=1;
      j:=0;
      CurrPtr:=pBuffer;
      REPEAT
        WITH Pattern^[i][j] DO
        BEGIN
          CurrFlags:=CurrPtr^;
          IF CurrFlags AND $80<>0 THEN Inc(CurrPtr) ELSE CurrFlags:=$FF;
          IF CurrFlags AND $01<>0 THEN
          BEGIN
            Ton:=CurrPtr^;
            IF Ton>96 THEN Ton:=$FFFF;
            Inc(CurrPtr);
          END;
          IF CurrFlags AND $02<>0 THEN BEGIN Instrument:=CurrPtr^; Inc(CurrPtr); END;
          IF CurrFlags AND $04<>0 THEN BEGIN ChnVol    :=CurrPtr^; Inc(CurrPtr); END;
          IF CurrFlags AND $08<>0 THEN BEGIN Effekt    :=CurrPtr^; Inc(CurrPtr); END;
          IF CurrFlags AND $10<>0 THEN BEGIN Operands  :=CurrPtr^; Inc(CurrPtr); END;
          CASE Effekt OF
            NoEffekt,0:IF Operands=0 THEN Effekt:=NoEffekt ELSE Effekt:=Arpeggio;
            $1..$8:;
            $09{9}:IF (TON<1)OR(TON>96) THEN BEGIN Effekt:=NoEffekt; Operands:=0; END;
            $A..$F:;
            $10{G}:Effekt:=SetGlobVol;
            $11{H}:Effekt:=GlobVolSld;
            $14{K}:Effekt:=KeyOff;
            $15{L}:Effekt:=SetEnvelopePos;
            $19{P}:Effekt:=PanSld;
            $1B{R}:Effekt:=RetrigVolSlide;
            $1D{T}:Effekt:=Tremor;
            $21{X}:begin
                     case operands shr 4 of
                       1: Effekt:=XtraFSldUp;
                       2: Effekt:=XtraFSldDwn;
                       else Effekt := NoEffekt;
                     end;
                     Operands := Operands AND $0F;
                   end;
            ELSE Effekt:=NoEffekt;
          END;
          IF (Effekt=Enhanced)AND($10+(Operands SHR 4)=NoteDelay) THEN Inc(Operands);
        END;
        Inc(i);
        IF i>Mi.NumTracks THEN
        BEGIN
          Inc(j);
          i:=1;
        END;
      UNTIL Ofs(CurrPtr^)>=EndOfData;
    END {EndOfData<>0};
    IF NOT TrackWrite(Mi.NumTracks,Patlen)THEN BEGIN Error:=ID_NoMEM; GlobalFreePtr(pBuffer); Exit; END;
  END;
  GlobalFreePtr(pBuffer);
  ReplaceTrackNums;
  { get samples }
  k:=1;
  FOR i := 1 TO MaxFT2Insts DO WITH FT2Instruments^[i] DO IF NumSamples>0 THEN
  FOR j := 1 TO NumSamples DO WITH FT2Samples^[k] DO
  BEGIN
    IF SampleLen>0 THEN
    BEGIN
      _lSeekAbs32(FHandle, FIs32, FileOffset);
      CurrFlags:=8+8*Byte(SampleType AND $10<>0);
      IF NOT LoadInstrument (Mi.Instruments[i].Name,SampOffset,SampleLen,LoopStart,LoopEnd,Volume,
                             CurrFlags,k,7+Byte(SampleType AND $10<>0),(SampleType AND $02)<>0)THEN
      BEGIN
        Error:=ID_NoMem;
        Exit;
      END;
      IF CurrFlags=8
      THEN SampleType:=SampleType AND NOT($10)
      ELSE SampleType:=SampleType OR $10; {}
    END;
    inc(K);
  END;
END;

END.
