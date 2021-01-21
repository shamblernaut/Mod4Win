{$I Effects.INC}
FUNCTION DoSurround(aInst,aPanVal:Integer;aTonH:Word):BOOLEAN;
VAR K:Integer;
BEGIN
  DoSurround:=False;
  FOR k:=1 TO MI.NumTracks DO WITH Channels[k] DO
  IF (eInst IN EventChg)
  AND(Inst=aInst)
  AND(NewTonH=aTonH)
  AND(Abs(PanVal-aPanVal)>15)
  THEN Exit;
  DoSurround:=True;
END;

FUNCTION OneChannelLoops:Boolean;
VAR IK:Integer;
BEGIN
  OneChannelLoops:=True;
  FOR IK:=MI.NumTracks DOWNTO 1 DO IF Channels[IK].PatternLoops>0 THEN Exit;
  OneChannelLoops:=False;
END;

PROCEDURE SetVolTable; FAR;FORWARD;

VAR ii:Byte;
    pjump,pbreak:boolean;
    jumpto,breakto:byte;
    NextBytePos:Longint;
    GlobalFadeOut:Boolean;
    GlobalFadeOutTime:Longint;

CONST ModRepeats:WordBool=FALSE;

PROCEDURE ProcessAutoVibrato(N:Byte);
TYPE tTable=ARRAY[0..255] OF shortint;
     pTable=^tTable;
VAR NewPtr:Byte;
    NewDepth:byte;
    NewVal:Integer;
BEGIN
  WITH Channels[n],FT2Instruments^[Inst] DO
  IF ((NewTonH<>0)OR(Tonh<>0))AND(VibrDepth<>0)AND(VibrRate<>0)THEN
  BEGIN
    IF (VibrSweep<>0)AND(VibTck<VibrSweep) THEN
    BEGIN
      NewDepth:=VibrDepth*VibTck DIV VibrSweep;
      Inc(VibTck);
    END ELSE NewDepth:=VibrDepth;
    NewPtr:=VibPtr+Longint(CurrVBlanc+1)*(Speed-1)*VibrRate*4 DIV 3 DIV Speed;
    IF (NOT(ePitch IN EventChg))OR(NewTonh=0) THEN NewTonh:=Tonh;
    NewVal:=VibTbl;
    {NewVal:=Longint(NewDepth)*Longint(pTable(VibTbl)^[NewPtr]DIV 42);{}
    ASM
      XOR bx,bx
      XOR ax,ax
      mov bl,NewPtr
      add bx,NewVal
      mov al,[bx]
      mov cl,newdepth
      imul cl
      cwd
      mov cx,42
      idiv cx
      mov NewVal,ax
    END;
    NewTonh:=NewTonh+NewVal;
    IF CurrVBlanc+1=Speed THEN VibPtr:=NewPtr;
    Include(EventChg,ePitch);
  END;
END;

FUNCTION ProcessEnvelope(VAR NewVal:Byte;
                         VAR CurrTick:Word;
                         VAR CurrPos:Byte;
                         CONST MaxPos,SustainPos,LoopStartPos,LoopEndPos:Byte;
                         CONST DoFadeOut:Boolean;
                         CONST EnvPoints:tEnvPts):Boolean;
VAR Value:Byte;
BEGIN
  ProcessEnvelope:=False;
  IF (CurrPos<MaxPos)AND(DoFadeOut OR (SustainPos<>CurrPos) OR (CurrTick<>EnvPoints[SustainPos].Tick)) THEN
  BEGIN
    IF CurrTick=EnvPoints[CurrPos+1].Tick THEN
    BEGIN
      Value:=EnvPoints[CurrPos+1].Value;
      Inc(CurrPos);
      IF CurrPos<=MaxPos THEN
      BEGIN
        IF CurrPos=LoopEndPos THEN
        BEGIN
          CurrPos:=LoopStartPos;
          IF LoopStartPos>1 THEN CurrTick:=EnvPoints[LoopStartPos].Tick ELSE CurrTick:=0;
        END;
        IF (DoFadeOut)OR(CurrPos<>SustainPos) THEN Inc(CurrTick);
      END;
    END ELSE
    BEGIN
      IF EnvPoints[CurrPos+1].Tick>EnvPoints[CurrPos].Tick
      THEN Value:= (Longint(CurrTick)-Longint(EnvPoints[CurrPos].Tick))
                  *(Longint(EnvPoints[CurrPos+1].Value)-Longint(EnvPoints[CurrPos].Value))
                DIV(Longint(EnvPoints[CurrPos+1].Tick) -Longint(EnvPoints[CurrPos].Tick))
                  +Longint(EnvPoints[CurrPos].Value);
      Inc(CurrTick);
    END;
    ProcessEnvelope:=Value<>NewVal;
    NewVal:=Value;
  END;
END;

PROCEDURE SetEnvPos(VAR NewVal:Byte;NewPos:Word;VAR EnvPos:Byte;VAR EnvTck:Word;
                    CONST EnvPts:tEnvPts;CONST NumPts,LoopEnd:Word);
VAR I:Word;
BEGIN
  IF NumPts>1 THEN
  BEGIN
    EnvTck:=NewPos;
    IF EnvTck>EnvPts[NumPts ].Tick                 THEN EnvTck:=EnvPts[NumPts ].Tick;
    IF (LoopEnd>0)AND(EnvTck>EnvPts[LoopEnd].Tick) THEN EnvTck:=EnvPts[LoopEnd].Tick;
    EnvPos:=1;
    FOR i:=2 TO NumPts DO IF EnvPts[I].Tick>EnvTck THEN
    BEGIN
      EnvPos:=I-1;
      Break;
    END;
    NewVal:= (Longint(EnvTck)-Longint(EnvPts[EnvPos].Tick))
            *(Longint(EnvPts[EnvPos+1].Value)-Longint(EnvPts[EnvPos].Value))
          DIV(Longint(EnvPts[EnvPos+1].Tick) -Longint(EnvPts[EnvPos].Tick))
            +Longint(EnvPts[EnvPos].Value);
  END;
END;

{ this procedure sets up new pitch values FOR DAC mix }
PROCEDURE DoPhiPlo(n:Byte; NewNote,oPlo:Word; FineTune:Shortint);
{      Note:=hiword(tunetable[finetune]*Note);
  (Phi,Plo):=65536*Cx*CxTuning/Samplerate/Note
           :=NoteStretch/Samplerate/Note
                           ...Cx = Note für Mittlere Oktave (normal: C2, S3M: C4)
                           ...CxTuning = Samplefrequenz für Cx}
VAR TTNote : longint;
BEGIN
  if NewNote = 0 then with Channels[n] do
  begin
    Note := 0;
    include (EventChg, EReset);
    exit;
  end;
  IF UseLinearTune THEN
  BEGIN
    {TTNote:=2^(9216-NewNote)/1536)*8363/SampleRate*65536}
    TTNote:=Round(Exp(ln(2.0)*(9216.0-NewNote)/1536.0)*DefMODTuning/SoundSettings.SampleRate*65535.0);
  END ELSE
  BEGIN
    TTNote:=NewNote;
    IF FineTune<>0 THEN TTNote:=HiWord(Longint(tunetable[FineTune AND $0F])*Longint(TTNote));
    TTNote := Round(1.0*NoteStretch/SoundSettings.SampleRate/TTNote);
  END;
  with Channels[n] do
  BEGIN
    MODEvent.Pitch:=TTNote;
    IF TTNote>30000 THEN JmpFlags:=JmpFlags OR fHiPitch ELSE JmpFlags:=JmpFlags AND(NOT fHiPitch);
  END;
  ASM
    mov di, oplo
    DB 66h; mov ax, word Ptr TTNote
    DB 66h; mov [di],ax
  END;
END;

{ this procedure sets up new volume and pan values FOR both, DAC mix }
PROCEDURE VolAdjust1(n:Byte);
VAR BR,BL,HB:word;
    VL: longint;
    VR: longint;
BEGIN
  IF N<>0 THEN
  WITH Channels[n] DO
  BEGIN
    IF Vol>64 THEN Vol:=64;
    BR:=0+Vol;
    IF Mi.FileType=FT2
    THEN BR:=Longint(BR)*GlobVol*EnvVol DIV 64*FadeVol DIV (64*65536)
    ELSE BR:=Word(BR)*GlobVol DIV 64;
    AvgVol:=BR;
    IF AvgVol<>0 THEN
    BEGIN
      {S3M Stereo Control}
      IF Scontrol THEN
      BEGIN
        IF Scont<>0 THEN
        CASE Scont-1 OF
          0:BEGIN AvgVol:=  AvgVol DIV 4; PanVal:=  0; END;
          1:BEGIN AvgVol:=  AvgVol DIV 4; PanVal:=255; END;
          2:BEGIN AvgVol:=  AvgVol DIV 2; PanVal:=  0; END;
          3:BEGIN AvgVol:=  AvgVol DIV 2; PanVal:=255; END;
          4:BEGIN AvgVol:=3*AvgVol DIV 4; PanVal:= 85; END;
          5:BEGIN AvgVol:=3*AvgVol DIV 4; PanVal:=170; END;
          6:BEGIN AvgVol:=  AvgVol DIV 2; PanVal:=128; END;
          7:BEGIN                         PanVal:=128; END;
        END ELSE  AvgVol:=  AvgVol DIV 4;
        BR:=AvgVol;
      END;
      HB:=0+PanVal;
      IF Mi.FileType=FT2 THEN HB:=HB+((Integer(EnvPan)-32)*(128-Abs(Integer(HB)-128)))DIV 32;
      IF (HB>=119)AND(HB<=136) THEN HB:=128;
      IF NOT SoundSettings.Panning THEN
      BEGIN
        IF HB>128 THEN HB:=255 ELSE IF HB<128 THEN HB:=0;
      END;
      IF HB<>128 THEN
      BEGIN
        BL:=(BR*(255-HB));
        BR:=(BR*     HB );
      END ELSE
      BEGIN
        BL:=128*BR;
        BR:=128*BR;
      END;
      LVol:=Word(BL) div 255;
      RVol:=Word(BR) div 255;
      IF lVol>64 THEN lVol:=64;
      IF rVol>64 THEN rVol:=64;
    END ELSE
    BEGIN
      LVol:=0;
      RVol:=0;
    END;
    VL:=Longint(lVol)*VolMaster DIV 2;
    VR:=Longint(rVol)*VolMaster DIV 2;
    IF VL<Longint(65535) THEN MODEvent.lVolume:=VL ELSE MODEvent.lVolume:=65535;
    IF VR<Longint(65535) THEN MODEvent.rVolume:=VR ELSE MODEvent.rVolume:=65535;
    IF SoundSettings.DeviceID>=DEV_HDD THEN
    BEGIN
      JmpFlags:=JmpFlags AND(NOT fPanned);
      IF  rVol = lVol THEN JmpFlags:=JmpFlags OR fMono   ELSE
      IF  lVol = 0    THEN JmpFlags:=JmpFlags OR fRight  ELSE
      IF  rVol = 0    THEN JmpFlags:=JmpFlags OR fLeft   ELSE JmpFlags:=JmpFlags OR fPanned;
    END;
  END;
END;

PROCEDURE VolAdjust(n:Byte);
BEGIN
  VolAdjust1(n);
  IF MI.FileType=Oktalyz THEN VolAdjust1(RelatedChn[n]);
END;

FUNCTION GetTonh (Ton:Word; Tuning:Longint; FineTune:ShortInt):word; assembler; asm
{ GetTonh := Ton * DefMODTuning / Tuning
          := Ton IF Tuning = 0
          := -1 IF overflow }
  cmp UseLinearTune,0
  je @Amiga
  {Linear freq. Table==> Period:=(10*12-Note)*16*4*2-Finetune}
  mov ax,Ton
  cmp ax,1
  jg  @1
  mov ax,1
@1:
  dec ax
  mov bx,10*12
  sub bx,ax
  SHL bx,7 {*16*4*2}
  mov al,FineTune
  cbw
  sub bx,ax
  mov ax,bx
  jmp @out
@Amiga:
  DB 66h; XOR ax,ax
  mov ax, Ton
  OR  ax,ax
  je @out
  cmp mi.FileType,FT2
  jne @3
  mov bx,offset PitchTable
  add bx,ax
  add bx,ax
  mov ax,[bx]
@3:
  DB 66h; mov cx, word Ptr Tuning
  jcxz @out
  DB 66h; mov  bx, word Ptr DefMODTuning  { mov ebx, dword ptr ... }
  DB 66h; mul  bx
  DB 66h; DIV  cx
  DB 66h; cmp  ax, $FFFF; DW 0  { cmp eax, $0000FFFF }
  jb @out
  mov ax, $FFFF
@out:
END;

FUNCTION NoteToVal(Note:Word; Tune:Longint; FineTune:Byte):Byte; NEAR; ASSEMBLER;
ASM
   cmp UseLinearTune,0
   je @Amiga
   {Linear freq. Table==> Note:=10*12-(2*Period+FineTune)/2/16/4}
   mov bx,note
   SHR bx,1 {/2 für effekte...}
   mov al,FineTune
   cbw
   add ax,bx
   add ax,bx
   SHR ax,7
   sub ax,10*12
   neg ax
   inc ax
   jmp @Exit
@Amiga:
   DB 66h; XOR ax,ax
   mov ax,Note
   cmp MI.FileType,S3M
   je @0
   cmp MI.FileType,FT2
   je @0
   cmp MI.FileType,FTrk
   je @0
   cmp MI.FileType,WOW
   je @0
   DB 66h; SHL ax,2
@0:
   DB 66h; mov cx,word Ptr Tune
   DB 66h; mul cx
   DB 66h; XOR cx,cx
   mov cx,word Ptr DefMODTuning
   DB 66h; DIV cx
   DB 66h; mov dx,ax
   DB 66h; OR dx,dx
   jnz @NoNull
   mov ax,145
   jmp @Exit
@NoNull:
   DB 66h; shr ax,16
   jz @NoHigh
   XOR al,al
   jmp @Exit
@NoHigh:
   mov cx,145
   mov si,Offset PitchTable
   Add si,cx
   Add si,cx
@LOOP:
   cmp dx,[si]
   jbe @GotIt
   sub si,2
   loop @loop
@GotIt:
   mov ax,cx
@Exit:
END;

PROCEDURE InitEnvelope(n,Inst:Byte);
BEGIN
  WITH FT2Instruments^[Inst],Channels[n] DO
  BEGIN
    {Reset Volume Envelope}
    IF(VolType AND 1<>0) THEN
    BEGIN
      EnvVolPos:=1;
      EnvVol:=VolEnvPts[1].Value;
      Include(EventChg,eVol);
    END ELSE
    BEGIN
      EnvVolPos:=0;
      EnvVol:=64;
    END;
    EnvVolTck:=0;
    {Reset Panning Envelope}
    IF(PanType AND 1<>0) THEN
    BEGIN
      EnvPanPos:=1;
      EnvPan:=PanEnvPts[1].Value;
      Include(EventChg,eVol);
    END ELSE
    BEGIN
      EnvPanPos:=0;
      EnvPan:=32;
    END;
    EnvPanTck:=0;
    {Reset auto Vibrato}
    VibTck:=0;
    CASE VibrType OF
      0:VibTbl:=Ofs(SinTable);
      1:VibTbl:=Ofs(SqrTable);
      2:VibTbl:=Ofs(RampUpTable);
      3:VibTbl:=Ofs(RampDnTable);
    END;
    VibPtr:=0;
    {Reset Fadeout}
    FadeVol  :=65536;
    DoFadeOut:=False;
  END;
END;

{ this procedure sets up new instruments FOR or DAC mix }
PROCEDURE InstrAdjust (n: byte; NewTon:Word);
BEGIN
  WITH Channels[n] DO
  BEGIN
    IF(effkt=FARportamento)OR(effkt=tone_portamento)OR(Effkt=PortVolslide)OR((MI.FileType=FT2)AND(OldChnVol AND $F0=$F0))THEN
    BEGIN
      IF (Mi.FileType=FT2) THEN {FT2 Bug! Retrigger note for TonePortamento...}
      BEGIN
        IF Note=0 THEN Note:=IOffset;
        IF NOT(OldChnVol IN [$70..$7F,$90..$9F]) THEN IF Vol=0  THEN Vol:=FT2Samples^[SmpNum].Volume;
        IF FadeVol<>65536 THEN InitEnvelope(N,SmpNum);
        Exit;
      END ELSE IF Inst=Nxtinst THEN Exit;
      NewTon:=0;
    END;
    IF Mi.FileType=FT2 THEN
    BEGIN
      WITH FT2Instruments^[NxtInst] DO
      IF NumSamples>0 THEN SmpNum:=Word(SampleSt)+Word(SampleNos[NewTon]) ELSE
      BEGIN
        Note:=0;
        Include(EventChg,eReset);
        Exit;
      END;
      IF NxtInst<>0 THEN
      BEGIN
        IF (Pattern^[n][CurrNote].Instrument<>0)THEN
        BEGIN
          PanVal :=FT2Samples^[SmpNum].Panning;
          Vol    :=FT2Samples^[SmpNum].Volume;
        END;
        InitEnvelope(N,NxtInst);
        Include (EventChg, EVol);
      END;
    END ELSE SmpNum:=NxtInst;
    {Instrument neu anspielen}
    Inst:=Nxtinst;
    JmpFlags:=JmpFlags AND (NOT f16Bit);
    IF Mi.FileType=FT2 THEN
    BEGIN
      Note    :=FT2Samples^[SmpNum].SampOffset;
      Iend    :=FT2Samples^[SmpNum].SampleLen;
      lend    :=FT2Samples^[SmpNum].loopend;
      IOffset :=FT2Samples^[SmpNum].SampOffset;
      IFinTun :=FT2Samples^[SmpNum].FineTune;
      ITune   :=Round(DefMODTuning*Exp(ln(2)*FT2Samples^[SmpNum].RelNote/12));
      LLn     :=FT2Samples^[SmpNum].loopstart;
      IF FT2Samples^[SmpNum].SampleType AND $10<>0 THEN JmpFlags:=JmpFlags OR f16Bit;
    END ELSE
    BEGIN
      Note    :=MI.Instruments[NxtInst].Offset;
      Iend    :=MI.Instruments[NxtInst].IEnd;
      lend    :=MI.Instruments[NxtInst].loopend;
      IF MI.Instruments[NxtInst].Bits AND $1F=16 THEN JmpFlags:=JmpFlags OR f16Bit;
      IFinTun :=MI.Instruments[NxtInst].FineTune;
      IOffset :=MI.Instruments[NxtInst].Offset;
      ITune   :=0+MI.Instruments[NxtInst].Tuning;
      LLn     :=MI.Instruments[NxtInst].loopstart;
    END;
    IF LEnd<>0 THEN Iend:=LEnd;
    IF LLn<>0
    THEN LLn:=IEnd-LLn
    ELSE LLn:=IEnd+65536;
    Dec(IEnd);
    IF LEnd>0 THEN Dec(LEnd);
    rest :=0;
    MODEvent.Inst := SmpNum;
    Include (EventChg, EInst);
    IF NewTon<>0 THEN
    IF UseLinearTune
    THEN tonh:=GetTonh (NewTon+Integer(FT2Samples^[SmpNum].RelNote), ITune, iFinTun)
    ELSE tonh:=GetTonh (NewTon, ITune, iFinTun);
  END;
END;

{$IFDEF CompressTracks}
PROCEDURE GetPattern(N,NRows:Word); ASSEMBLER;
VAR Rows:Word;
{Gepacktes TrackFormat:
 WORD ...Länge des Tracks in Byte einschliesslich dieses Words
   +Beliebig oft
     Byte ...Flags:
                Bit0...Instr.   <> 00h
                Bit1...Ton      <> 00h
                Bit2...Effekt   <> FFh (NoEffekt)
                Bit3...Operands <> 00h
                Bit4...ChnVol   <> FFh
                [Bit 5-7 Anzahl der folgenden LeerZeilen]
       +Byte ...Falls (Flags AND 01h)>0 Instrument
       +Word ...Falls (Flags AND 02h)>0 Ton
       +Byte ...Falls (Flags AND 04h)>0 Effekt
       +Byte ...Falls (Flags AND 08h)>0 Operands
       +Byte ...Falls (Flags AND 10h)>0 ChnVol
Speicher:
     Tracks     ...[1..32][0..NumPatterns-1]OF Word
     Pattern    ...Statisches Pattern zum Spielen [1..NumTracks][0..MaxPattLen-1(63)]OF TNote
     TrackData  ...[0..Lasttrack]OF tTrack
     Samples}
ASM
  cmp nRows,0
  je  @exit
  {FillChar Pattern^ mit NormalWerten}
  les di,Pattern
  DB 66h; XOR ax,ax
  DB 66h; XOR cx,cx
  mov cx,MI.NumTracks
 {mov eax,..} DB 66h,0b8h,00h,00h,00h,0ffh
  XOR bx,bx
  mov bh,DefChVol
@Fill0:
  push cx
  mov cx,NRows
  push di
@Fill1:
  {mov es:[di],eax} DB 26h,66h,89h,05h
  mov word Ptr es:[di+4],bx
  add di,6
  loop @Fill1
  pop di
  add di,6*256
  pop cx
  loop @Fill0
  {FS:[BX]-->TrackSequenceData}
  {lfs bx,Tracks}DB 0fh,0b4h,1eh; DW OFFSET Tracks
  mov ax,4*32
  mul n
  add bx,ax
  {ES:[DI]-->Pattern}
  les di,Pattern
  {ES:[ESI]-->TrackData[TrackSequenceData]}
  XOR cx,cx
  mov cl,Byte Ptr MI.NumTracks
@NewPos:
  mov ax,NRows
  mov Rows,ax
  DB 64h,66h; mov si,[bx]
  add bx,4
  {valid track?}
  DB 66h; OR  si,si
  jnz @nodummy
  add di,600h
  loop @NewPos
  JMP @Exit
@nodummy:
  push di
  push cx
  DB 66h; XOR cx,cx
  {TrackLänge holen}
  {mov cx,fs:[esi]} DB 64h,67h,8bh,0Eh
  DB 66h; add cx,si
  DB 66h; add si,2
  {Track kopieren}
  DB 66h; cmp cx,si
  je @trackmoved
@trackmove:
  {Note von ES:[ESI] nach ES:[DI] entpacken...}
  {mov al,fs:[esi]} DB 64h,67h,8ah,06h
  DB 66h; inc si
  {Instrument}
  SHR al,1
  jnc @noInst
 {mov dl,fs:[esi]} DB 64h,67h,8ah,16h
  DB 66h; inc si
  mov es:[di],dl
@NoInst:
  inc di
  {Note}
  SHR al,1
  jnc @noNote
 {mov dx,fs:[esi]} DB 64h,67h,8bh,16h
  DB 66h; add si,2
  mov es:[di],dx
@NoNote:
  add di,2
  {Effekt}
  SHR al,1
  jnc @noEffect
 {mov dl,fs:[esi]} DB 64h,67h,8ah,16h
  DB 66h; inc si
  mov es:[di],dl
@NoEffect:
  inc di
  {Operands}
  SHR al,1
  jnc @NoOps
 {mov dl,fs:[esi]} DB 64h,67h,8ah,16h
  DB 66h; inc si
  mov es:[di],dl
@NoOps:
  inc di
  {Volume}
  SHR al,1
  jnc @NoVol
 {mov dl,fs:[esi]} DB 64h,67h,8ah,16h
  DB 66h; inc si
  mov es:[di],dl
@NoVol:
  inc di
  {DummyNotenIncrement}
  mov dl,6
  mul dl
  add di,ax
  {Note entpackt...}
  Dec Rows
  jz  @trackmoved
  DB 66h; cmp si,cx
  jb @trackmove
@trackmoved:
  pop cx
  pop di
  add di,600h
  dec cx
  jnz @NewPos
@Exit:
END;
{$ELSE CompressTracks}
PROCEDURE GetPattern(N,NRows:Word); ASSEMBLER;
{Gepacktes TrackFormat:
 WORD ...Länge des Tracks in Byte einschliesslich dieses Words
   +Beliebig oft tNote
Speicher:
     Tracks     ...[1..32][0..NumPatterns-1]OF Word
     Pattern    ...Statisches Pattern zum Spielen [1..NumTracks][0..MaxPattLen-1(63)]OF TNote
     TrackData  ...[0..Lasttrack]OF tTrack
     Samples}
ASM
  cmp nRows,0
  je  @exit
  {FillChar Pattern^ mit NormalWerten}
  les di,Pattern
  DB 66h; XOR ax,ax
  DB 66h; XOR cx,cx
  mov cx,MI.NumTracks
 {mov eax,..} DB 66h,0b8h,00h,00h,00h,0ffh
  XOR bx,bx
  mov bh,DefChVol
@Fill0:
  push cx
  mov cx,NRows
  push di
@Fill1:
  {mov es:[di],eax} DB 26h,66h,89h,05h
  mov word Ptr es:[di+4],bx
  add di,6
  loop @Fill1
  pop di
  add di,6*256
  pop cx
  loop @Fill0
  {FS:[BX]-->TrackSequenceData}
  {lfs bx,Tracks}DB 0fh,0b4h,1eh; DW OFFSET Tracks
  mov ax,4*32
  mul n
  add bx,ax
  {ES:[DI]-->Pattern}
  les di,Pattern
  {dx->MaxTrackSize}
  mov ax,nRows
  mov cx,type tNote
  mul cx
  mov dx,ax
  {CX->TrackCounter}
  mov cl,Byte Ptr MI.NumTracks
@NewPos:
  {ES:[ESI]-->TrackData[TrackSequenceData]}
  DB 64h,66h; mov si,[bx]
  add bx,4
  {valid track?}
  DB 66h; OR  si,si
  jnz @nodummy
  add di,600h
  loop @NewPos
  JMP @Exit
@nodummy:
  push di
  push cx
  {TrackLänge holen}
  {mov cx,fs:[esi]} DB 64h,67h,8bh,0Eh
  sub cx,2
  {}
  cmp cx,dx
  jbe @NoOverflow
  mov cx,dx
@NoOverflow:{}
  shr cx,1
@trackmove:
  DB 66h; add si,2
  {mov ax,fs:[esi]} DB 64h,67h,8Bh,06h
  mov es:[di],ax
  add di,2
  loop @TrackMove
  pop cx
  pop di
  add di,600h
  loop @NewPos
@Exit:
END;
{$ENDIF CompressTracks}

CONST BackChannels:pMixChannels=NIL;

PROCEDURE FreeSavedChannels;
BEGIN
  IF BackChannels<>NIL THEN GlobalFreePtr(BackChannels);
  BackChannels:=NIL;
END;

PROCEDURE SaveChannels;
VAR I:Integer;
BEGIN
  IF BackChannels=NIL
  THEN BackChannels:=GlobalAllocPtr(gptr,SizeOf(tMixChannels));
  FOR I:=1 TO 32 DO Move(Channels[i],BackChannels^[i],SizeOf(tMixChannel));
END;

PROCEDURE RestoreChannels;
VAR I:Integer;
BEGIN
  IF BackChannels<>NIL THEN FOR I:=1 TO 32 DO Move(BackChannels^[i],Channels[i],SizeOf(tMixChannel));
END;

PROCEDURE ProcessNewNote(Chn:Byte);
BEGIN
  {Nächste Notenzeile holen}
  WITH Pattern^[Chn][CurrNote],Channels[Chn] DO
  BEGIN
    IF (Effkt IN [Arpeggio,OKTarpg1,OKTarpg2])AND(Effekt<>Effkt)THEN
    BEGIN
      NewTonH:=TonH;
      Include(EventChg,ePitch);
    END;
    CASE MI.FileType OF
      { FAR Vibratos }
      fFAR: IF (Effkt=VibratoSust)AND(Effekt=NoEffekt) THEN
            BEGIN
              Effekt:=Effkt;
              Operands:=Opnd;
            END;
      { 669/UNIS effects stay on until parameter 0 is given }
      f669, UNIS:
        IF (Ton=0)OR(Instrument=0) AND
          (Effkt IN [Portamento_up,Portamento_down,Tone_portamento,Vibrato,FrequAdjust]) THEN
        CASE Effekt OF
          NoEffekt: BEGIN Effekt:=Effkt; Operands:=Opnd; END;
          Portamento_up,Portamento_down,Tone_portamento,Vibrato: IF(Operands=0)THEN Effekt:=NoEffekt;
        END;
      { handle S3M's KeyOff effect }
      FT2, STM, S3M:
        IF Ton=$FFFF THEN
        BEGIN
          Ton:=0;
          IF (Mi.FileType=FT2)AND(EnvVolPos<>0) THEN
          BEGIN
            DoFadeOut:=True;
            Instrument:=0;
          END ELSE
          BEGIN
            Note := 0;
            Include (EventChg, EReset);
          END;
        END;
    END;
    {Neue Effekte auswerten und evtl. auf Werte der alten Effekte setzen...}
    CASE Effekt OF
      NoEffekt       :IF Effkt=NoEffekt THEN ELSE
                      { FAR portamentos }
                      IF (Effkt=FARPortamento)THEN BEGIN Effekt:=FARPortamento; Operands:=Opnd; END ELSE
                      IF (Effkt=VolumePort)AND(Vol<>VolPrt)AND(ChnVol=DefChVol) THEN
                      BEGIN Effekt:=VolumePort; Operands:=Opnd; END ELSE
                      { UNIS retrigger }
                      IF (Effkt=SlotRetrigger)THEN
                      BEGIN
                        Effekt:=SlotRetrigger;
                        Operands:=Opnd;
                      END;
      SetGlobVol     :BEGIN
                        GlobVol:=Operands; IF GlobVol>64 THEN GlobVol:=64;
                        FOR i:=1 TO MI.NumTracks DO Include(Channels[i].EventChg,eVol);
                      END;
      XtraFSldUp,
      Portamento_up  :IF MI.FileType in [S3M, STM, OKTalyz, FT2] THEN
                      BEGIN
                        IF Operands=0 THEN Operands:=PrtOps ELSE PrtOps:=Operands;
                        IF MI.FileType in [S3M, STM] THEN
                        BEGIN
                          IF Operands >= $E0 THEN
                          begin
                            if Operands >= $F0 then Effekt := FSldUp else Effekt := XtraFSldUp;
                            Operands := Operands AND $0F;
                          END;
                        END;
                      END;
      XtraFSldDwn,
      Portamento_down:IF MI.FileType in [S3M, STM, OKTalyz, FT2] THEN
                      BEGIN
                        IF Operands=0 THEN Operands:=PrtOps ELSE PrtOps:=Operands;
                        IF MI.FileType in [S3M, STM] THEN
                        BEGIN
                          IF Operands >= $E0 THEN
                          begin
                            if Operands >= $F0 then Effekt := FSldDwn else Effekt := XtraFSldDwn;
                            Operands := Operands AND $0F;
                          END;
                        END;
                      END;
      FineVolUp,
      FineVolDwn,
      VolumeSlide    :BEGIN
                        IF Operands=0 THEN Operands:=VSldOps ELSE VSldOps:=Operands;
                        IF MI.FileType in [S3M, STM] THEN
                        BEGIN
                          IF (Operands AND $0F = $0F)AND(Operands AND $F0 <> 0) THEN
                          BEGIN
                            Effekt := FineVolUp;
                            Operands := Operands SHR 4;
                          END ELSE IF (Operands AND $F0=$F0)AND(Operands AND $0F<>0) THEN
                          BEGIN
                            Effekt := FineVolDwn;
                            Operands := Operands AND $0F;
                          END;
                        END;
                        IF Operands=0 THEN Effekt:=NoEffekt;
                      END;
      GlobVolSld     :BEGIN
                        IF Operands=0 THEN Operands:=GVSldOps ELSE GVSldOps:=Operands;
                        IF Operands=0 THEN Effekt:=NoEffekt;
                      END;
      PanSld         :BEGIN
                        IF Operands=0 THEN Operands:=PSldOps ELSE PSldOps:=Operands;
                        IF Operands=0 THEN Effekt:=NoEffekt;
                      END;
      Arpeggio       :BEGIN
                        IF effkt<>Arpeggio THEN Acnt:=2;
                        IF Operands=0 THEN Operands:=ArpOps;
                        IF Operands=0 THEN effekt:=NoEffekt ELSE ArpOps:=Operands;
                      END;
      SetSpeed       :DoSetSpeed(Operands);
      SetBPMSpeed    :IF Operands >= 32 THEN
                      BEGIN
                        ForceBPM:=True;
                        DoSetSpeed(Operands);
                        ForceBPM:=False;
                      END;
      Tone_portamento:BEGIN
                        IF (Mi.FileType<>FT2)AND(Effkt<>Tone_portamento)AND(Ton=0) THEN Effekt:=NoEffekt;
                        {Falls kein Operand angegeben ist, alten benutzen}
                        IF Operands=0 THEN Operands:=PrtNOps ELSE PrtNOps:=Operands;
                        IF Operands=0 THEN Effekt:=NoEffekt;
                      END;
      FARportamento  :IF Ton<>0 THEN
                      BEGIN
                        tpton:=ton;
                        FPTonh:=Tonh;
                        IF Operands=0 THEN effekt:=NoEffekt ELSE FPVblanc:=1;
                      END ELSE IF Effkt<>FARPortamento THEN Effekt:=NoEffekt;
      VolumePort     :IF (ChnVol<>DefChVol)AND(Operands<>0) THEN
                      BEGIN
                        VolPrt:=ChnVol;
                        ChnVol:=DefChVol;
                        VolSld:=Vol;
                        VPVBlanc:=1;
                      END ELSE IF Effkt<>VolumePort THEN Effekt:=NoEffekt;
      PortVolslide   :BEGIN
                        IF Operands=0 THEN Operands:=VSldOps ELSE VSldOps:=Operands;
                        IF Operands=0 THEN Effekt:=NoEffekt;
                      END;
      FineVibrato,
      Vibrato        :BEGIN
                        {Pointer auf Sinustable eventuell zurücksetzen}
                        IF NOT (effkt IN [Vibrato, FineVibrato])
                          THEN IF MI.FileType = MTM THEN vtini := 128 ELSE vtini:=0;
                        {Falls Operands (teilweise) nicht angegeben, sind alte zu benutzen}
                        IF(Operands AND $f0)=0 THEN Operands:=Operands OR(vops AND $f0);
                        IF(Operands AND $0f)=0 THEN Operands:=Operands OR(vops AND $0f);
                        {Fehlerbehandlung}
                        IF Operands=0 THEN effekt:=NoEffekt ELSE vops:=Operands;
                      END;
      VibraVolslid   :IF Operands=0 THEN Operands:=VSldOps ELSE VSldOps:=Operands;
      VibratoDepth   :FarVibDepth:=Operands;
      VibratoSust,
      FarVibrato     :BEGIN
                        IF NOT (effkt IN [VibratoSust, FarVibrato]) THEN vtini:=0;
                        IF effekt = VibratoSust THEN IF Operands=0 THEN effekt:=NoEffekt;
                      END;
      Tremolo        :BEGIN
                        {Pointer auf Sinustable eventuell zurücksetzen}
                        IF effkt<>Tremolo THEN
                        BEGIN
                          vtini:=0;
                          VolSld:=Vol;
                        END;
                        IF (ChnVol<>DefChVol)
                        THEN IF Mi.FileType<>FT2
                          THEN VolSld:=ChnVol
                          ELSE IF (ChnVol<$60)AND(ChnVol>0) THEN VolSld:=ChnVol-$10;
                        {Falls Operands (teilweise) nicht angegeben, sind alte zu benutzen}
                        IF(Operands AND $f0)=0 THEN Operands:=Operands OR(tops AND $f0);
                        IF(Operands AND $0f)=0 THEN Operands:=Operands OR(tops AND $0f);
                        {Fehlerbehandlung}
                        IF Operands=0 THEN effekt:=NoEffekt ELSE tops:=Operands;
                      END;
      OKTarpg1       :BEGIN
                        IF effkt<>OKTarpg1 THEN Acnt:=0;
                        IF Operands=0 THEN Operands:=Arp1Ops;
                        IF Operands=0 THEN effekt:=NoEffekt ELSE Arp1Ops:=Operands;
                      END;
      OKTarpg2       :BEGIN
                        IF effkt<>OKTarpg2 THEN Acnt:=0;
                        IF Operands=0 THEN Operands:=Arp2Ops;
                        IF Operands=0 THEN effekt:=NoEffekt ELSE Arp2Ops:=Operands;
                      END;
      OktSldUp       :IF Operands=0 THEN
                      BEGIN Operands:=OktSuOps; IF Operands=0 THEN Effekt:=NoEffekt;
                      END ELSE OktSuOps:=Operands;
      OktSldDwn      :IF Operands=0 THEN
                      BEGIN Operands:=OktSdOps; IF Operands=0 THEN Effekt:=NoEffekt;
                      END ELSE OktSdOps:=Operands;
      OktSld1Up      :IF Operands=0 THEN
                      BEGIN Operands:=OktSu1Ops; IF Operands=0 THEN Effekt:=NoEffekt;
                      END ELSE OktSu1Ops:=Operands;
      OktSld1Dwn     :IF Operands=0 THEN
                      BEGIN Operands:=OktSd1Ops; IF Operands=0 THEN Effekt:=NoEffekt;
                      END ELSE OktSd1Ops:=Operands;
      FineTempoUp    :DoFineFarTempo (Operands);
      FineTempoDwn   :DoFineFarTempo (-Operands);
      FarRetrig      :IF Operands>0 THEN Operands:=1+Speed DIV Operands;
      RetrigVolSlide :BEGIN
                        IF Operands AND $0F=0 THEN Operands:=Operands OR (RetrOps AND $0F);
                        IF Operands AND $F0=0 THEN Operands:=Operands OR (RetrOps AND $F0);
                        IF Operands=0 THEN Effekt:=NoEffekt ELSE RetrOps:=Operands;
                        IF Effkt <> RetrigVolSlide THEN Acnt := 0;
                      END;
      SlotRetrigger  :IF Operands=0 THEN Effekt:=NoEffekt ELSE
                      IF OldVolume=0 THEN
                      BEGIN
                        Operands:=Operands DIV 2;
                        RetrOps:=0;
                        OldVolume:=4;
                      END;
      Tremor         :BEGIN
                        IF Operands = 0 THEN Operands := TremorOps ELSE TremorOps := Operands;
                        IF Effkt <> Tremor THEN Acnt := 0;
                      END;
      VibraWave: BEGIN
          CASE (Operands AND $03) OF
            0: VTable:=Ofs(sintable);
            1: VTable:=Ofs(rampUptable);
            2: VTable:=Ofs(sqrtable);
          END;
          IF Operands AND $04 = 0 THEN vtini:=0;
        END;
      TremWave: BEGIN
          CASE (Operands AND $03) OF
            0: TTable:=Ofs(sintable);
            1: TTable:=Ofs(rampUptable);
            2: TTable:=Ofs(sqrtable);
          END;
          IF Operands AND $04 = 0 THEN vtini:=0;
        END;
      Pattrndelay:BEGIN
                    IF Operands=0 THEN Operands:=PdelOps;
                    IF Operands=0 THEN Effekt:=NoEffekt ELSE
                    BEGIN
                      delaypattern:=true;
                      patterndelay:=Operands;
                      PDelOps:=Operands;
                    END;
                  END;
      FineVolUp  :IF Operands=0 THEN
                  BEGIN Operands:=fVuOps; IF Operands=0 THEN Effekt:=NoEffekt;
                  END ELSE fVuOps:=Operands;
      FineVolDwn :IF Operands=0 THEN
                  BEGIN Operands:=fVdOps; IF Operands=0 THEN Effekt:=NoEffekt;
                  END ELSE fVdOps:=Operands;
      GlissdCtrl :GlissandoOn:=Operands <> 0;
    END;
    IF (NewFineTune<>0)OR(NewTonh<>TonH) THEN
    BEGIN
      {Period:=(10*12-Note)*16*4*2-Finetune}
      IF (NewFineTune<>0) AND UseLinearTune THEN TonH:=TonH-ShortInt(NewFineTune)+ShortInt(iFinTun);
      NewTonH:=TonH;
      NewFineTune:=0;
      Include(EventChg,ePitch);
    END;
    {Neues Instrument laden}
    IF instrument>0 THEN
    BEGIN
      {Instrument-Nr. nur merken! und evtl. altes noch weiterspielen}
      nxtinst:=instrument;
      {Lautstärke des Kanals hart setzen}
      IF Mi.FileType<>FT2 THEN Vol:=MI.Instruments[Instrument].Volume ELSE
      {TEMP!... IF (effekt<>tone_portamento)AND(Effekt<>PortVolslide)AND(ChnVol AND $F0<>$F0)THEN{}
      BEGIN
        PanVal :=FT2Samples^[SmpNum].Panning;
        Vol    :=FT2Samples^[SmpNum].Volume;
        InitEnvelope(Chn,instrument);
      END;
      Include (EventChg, EVol);
    END;
    {Channel Volume setzen}
    IF Mi.FileType<>FT2 THEN IF (ChnVol<>DefChVol) THEN BEGIN Vol:=ChnVol; Include(EventChg,eVol); END ELSE ELSE
    {IF Mi.FileType=FT2 THEN...Volume Channel Effects}
    CASE ChnVol OF
      $00..$5F:{NIX};
      $A1..$AF:{Set Vibrato Speed}
               IF NOT (effekt IN [Vibrato, FineVibrato]) THEN vops:=vops AND $0F+(ChnVol SHL 4)AND $F0;
      $80..$8F:{FineVolSldDn}
               IF (Effekt<>FineVolDwn)THEN
               BEGIN
                 IF ChnVol=$80 THEN ChnVol:=$80 OR (fVSldDnOps AND $0F) ELSE fVSldDnOps:=ChnVol;
                 IF ChnVol=$80 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $90..$9F:{FineVolSldUp}
               IF (Effekt<>FineVolUp)THEN
               BEGIN
                 IF ChnVol=$90 THEN ChnVol:=$90 OR (fVSldUpOps AND $0F) ELSE fVSldUpOps:=ChnVol;
                 IF ChnVol=$90 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $60..$6F:{VolSldDn}
               IF Effekt<>VolumeSlide THEN
               BEGIN
                 IF ChnVol=$60 THEN ChnVol:=$60 OR (VSldDnOps AND $0F) ELSE VSldDnOps:=ChnVol;
                 IF ChnVol=$60 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $70..$7F:{VolSldUp}
               IF Effekt<>VolumeSlide THEN
               BEGIN
                 IF ChnVol=$70 THEN ChnVol:=$70 OR (VSldUpOps AND $0F) ELSE VSldUpOps:=ChnVol;
                 IF ChnVol=$70 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $D0..$DF:{PanSlide Left}
               IF Effekt<>PanSld THEN
               BEGIN
                 IF ChnVol=$D0 THEN ChnVol:=$D0 OR (PSldLOps AND $0F) ELSE PSldLOps:=ChnVol;
                 IF ChnVol=$D0 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $E0..$EF:{PanSlide Right}
               IF Effekt<>PanSld THEN
               BEGIN
                 IF ChnVol=$E0 THEN ChnVol:=$E0 OR (PSldROps AND $0F) ELSE PSldROps:=ChnVol;
                 IF ChnVol=$E0 THEN ChnVol:=DefChVol;
               END ELSE ChnVol:=0;
      $B0..$BF:{Vibrato}IF NOT (effekt IN [Vibrato, FineVibrato]) THEN
               BEGIN
                 {Pointer auf Sinustable eventuell zurücksetzen}
                 IF NOT (OldChnVol IN [$B0..$BF]) THEN vtini:=0;
                 {Falls Operands (teilweise) nicht angegeben, sind alte zu benutzen}
                 IF ChnVol=$B0 THEN ChnVol:=ChnVol OR (vops AND $0f);
                 {Fehlerbehandlung}
                 IF ChnVol=$B0 THEN ChnVol:=0 ELSE vops:=(vops AND $F0)OR(ChnVol AND $0f);
                 IF vops AND $F0=0 THEN ChnVol:=DefChVol;
               END;
      $F0..$FF:{Tone Portamento}
               IF NOT (Effekt IN [Tone_Portamento,PortVolSlide,Portamento_Up,Portamento_Down])THEN
               BEGIN
                 IF ChnVol=$F0 THEN ChnVol:=$F0 OR (PrtNOps SHR 4) ELSE PrtNOps:=ChnVol and $0F shl 4;
                 IF ChnVol=$F0 THEN ChnVol:=DefChVol;
               END;
    END;
    effkt:=Effekt;
    Opnd:=Operands;
    OldChnVol:=ChnVol;
    {neuen Ton anspielen...}
    IF (ton>0) THEN
    BEGIN
      InstrAdjust (Chn,Ton);
      IF (Mi.Filetype=FT2)AND(effekt=PlayOffset)AND(IEnd>0) THEN
      BEGIN
        IF Opnd=0 THEN Opnd:=OfsOps ELSE OfsOps:=Opnd;
        MODEvent.Offset:=Longint (Opnd) SHL 8;
        DoPlayOffset(Channels[Chn]);
      END;
      NewTonh:=Tonh;
      Include(EventChg,ePitch);
    END;
    { Sample Offset is only executed the very first time we process the note, even with Pattern Delay }
    {zur aktuellen Position im Sample noch XX00 Hex dazuaddieren}
    IF (Mi.Filetype<>FT2)AND(effkt=PlayOffset)AND(IEnd>0) THEN
    BEGIN
      IF Opnd=0 THEN Opnd:=OfsOps ELSE OfsOps:=Opnd;
      MODEvent.Offset := Longint (Opnd) SHL 8;
      DoPlayOffset(Channels[Chn]);
    END;
    CASE Effekt OF
      Tone_portamento,
      PortVolslide   :BEGIN
                        IF Ton<>0 THEN IF UseLinearTune
                        THEN tpton:=GetTonh (ton+Integer(FT2Samples^[SmpNum].RelNote),ITune,iFinTun)
                        ELSE tpton:=GetTonh (ton,ITune,iFinTun);
                        IF (effkt <> Tone_Portamento) AND (effkt <> PortVolSlide) THEN TPGlTon := tonh;
                      END;
    END;
    {VolColumn FX}
    IF Mi.FileType=FT2 THEN
    CASE Byte(ChnVol)OF
      $10..$50:{Set Volume}BEGIN Vol:=ChnVol-$10; Include(EventChg,eVol); END;
      $C0..$CF:{Set Panning}
               BEGIN PanVal:=(ChnVol AND $0f)*17; Include(EventChg,eVol); END;
      $F0..$FF:{Tone Portamento}
               IF NOT (Effekt IN [Tone_Portamento,PortVolSlide,Portamento_Up,Portamento_Down])THEN
               BEGIN
                 IF Ton<>0 THEN IF NOT UseLinearTune
                 THEN tpton:=GetTonh (ton,ITune,iFinTun)
                 ELSE tpton:=GetTonh (ton+Integer(FT2Samples^[SmpNum].RelNote),ITune,iFinTun);
                 IF (effkt <> Tone_Portamento) AND (effkt <> PortVolSlide) THEN TPGlTon := tonh;
               END;
    END;
  END;
END;

PROCEDURE ProcessFirstTick(Chn:Byte);
BEGIN
  WITH Pattern^[Chn][CurrNote],Channels[Chn] DO
  BEGIN
    {Normal FX}
    CASE effkt OF
      NoEffekt       :;
      Panning        :BEGIN
                        IF Mi.FileType=FT2
                        THEN PanVal:=Opnd
                        ELSE IF Opnd<128 THEN PanVal:=Opnd SHL 1 ELSE
                                         IF Opnd>128 THEN PanVal:=128 ELSE PanVal:=255;
                        Include(EventChg,eVol);
                      END;
      SetVolume      :BEGIN Vol:=opnd; Include(EventChg,eVol); OldVolume:=Vol; END;
      OktOldVol      :BEGIN Vol:=OldVolume; Include(EventChg,eVol); END;
      StereoControl  :BEGIN SCont:=Succ(Opnd); Include(EventChg,eVol); Include(EventChg,eVol); END;

      FineTune   :BEGIN
                    {Period:=(10*12-Note)*16*4*2-Finetune}
                    IF UseLinearTune THEN
                    BEGIN
                      NewFineTune:=opnd SHL 4;
                      TonH:=TonH-ShortInt(iFinTun)+ShortInt(NewFineTune);
                      IF NewTonH<>0 THEN NewTonH:=NewTonH-ShortInt(iFinTun)+ShortInt(NewFineTune);
                    END ELSE NewFineTune:=opnd;
                    Include(EventChg,ePitch);
                  END;
      EPanning   :BEGIN PanVal:=Opnd*17; Include(EventChg,eVol); END;
      SetEnvelopePos :
        WITH FT2Instruments^[Inst] DO
        BEGIN
          SetEnvPos(EnvVol,Opnd,EnvVolPos,EnvVolTck,VolEnvPts,NumVolPts,VolLoopEnd);
          {SetEnvPos(EnvPan,Opnd,EnvPanPos,EnvPanTck,PanEnvPts,NumPanPts,PanLoopEnd);}
        END;
      KeyOff: IF (Mi.FileType=FT2) AND (EnvVolPos<>0) THEN
        BEGIN
          Ton := 0;
          DoFadeOut:=True
        END ELSE
        BEGIN
          Ton := 0;
          Note := 0;
          Include (EventChg, EReset);
        END;
    END;
  END;
END;

CONST TmpBool:WordBool=True;
      PlayerRepeats:WordBool=False;
PROCEDURE Play_the_Module;
LABEL Kernel,EPJMP,S3M_Inc,NewPattern;
BEGIN
  IF PM_Is_Working THEN BEGIN PM_Is_Working:=False; GOTO Kernel; END;
{******************Nächstes Pattern bearbeiten*************************************}
  Breakto:=0;
  REPEAT
  {Nächstes Pattern laden}
    if MI.FileType = S3M then case PArrangement[CurrPattern] of
      $FE: goto S3M_Inc;
      $FF: begin CurrPattern := $FFFF; goto S3M_Inc; end;
    end;
    GetPattern(CurrPattern,256);
    CurrNote:=Breakto;
    FOR II:=1 TO MI.NumTracks DO WITH Channels[ii]DO Patternloopst:=0;
    Breakto:=0;
    IF PtrnTempi[CurrPattern]>0 THEN DoSetSpeed (PtrnTempi[CurrPattern]);
{******************Nächste Notenzeile bearbeiten**********************************}
    REPEAT
NewPattern:
      IF NOT delayPattern THEN
      BEGIN
        FOR II:=1 TO MI.NumTracks DO
        WITH Pattern^[II][CurrNote] DO
        IF (Effekt<>NoteDelay) OR ((Ton=0)AND(Instrument=0))
        THEN ProcessNewNote(II)
        ELSE Channels[ii].Effkt:=NoEffekt;
      END ELSE
      BEGIN
        dec(patterndelay);
        IF patterndelay=0 THEN DelayPattern:=false;
      END;
      { interpret all effects that are executed only on the first tick of each line }
      FOR II:=1 TO MI.NumTracks DO ProcessFirstTick(II);
      CurrVBlanc:=0;
{******************Nächsten Speed-Takt bearbeiten***********************************}
      REPEAT
        IF GlobalFadeOut THEN
        BEGIN
          Inc(GlobalFadeOutTime);
          IF GlobalFadeOutTime MOD 8=0 THEN
          IF GlobVol<>0 THEN
          BEGIN
            FOR II:=1 TO MI.NumTracks DO Include(Channels[ii].EventChg,eVol);
            Dec(GlobVol);
          END ELSE
          BEGIN
            PlayerRepeats:=True;
            GlobalFadeOut:=False;
          END;
        END;
       {Effekte austesten}
        FOR ii:=1 TO MI.NumTracks DO WITH Channels[ii] DO
        BEGIN
          IF NOT (ePitch IN EventChg) THEN NewTonH:=TonH; {!TEMP!}
          WITH Pattern^[II][CurrNote] DO
          IF (Effekt=NoteDelay)
          AND(CurrVBlanc+1=Operands)
          AND((Ton<>0)OR(Instrument<>0))
          THEN ProcessNewNote(II);{}
          {Normal FX}
          CASE effkt OF
            NoEffekt       :;
            Arpeggio       :
              BEGIN
                IF UseLinearTune THEN
                BEGIN
                  IF Acnt=0 THEN Dec(NewTonh,Longint(Opnd AND $0F)*2*16*4) ELSE
                  IF Acnt=1 THEN Dec(NewTonh,Longint(Opnd SHR $04)*2*16*4);
                  IF Acnt<>0 THEN Dec(Acnt) ELSE Acnt:=2;
                END ELSE NewTonh:=DoArpeggio(opnd,Ofs(Acnt),Ofs(NewTonh));
                Include(EventChg,ePitch);
              END;
            OKTarpg1       : BEGIN NewTonh:=DoArpeggio1(opnd,Ofs(Acnt),Ofs(Tonh)); Include(EventChg,ePitch); END;
            OKTarpg2       : BEGIN NewTonh:=DoArpeggio2(opnd,Ofs(Acnt),Ofs(Tonh)); Include(EventChg,ePitch); END;
            Tremor         :
              BEGIN
                IF Vol > 0
                THEN IF Acnt>=Opnd SHR 4   THEN BEGIN TVol:=Vol; Vol:=0; Include(EventChg,eVol); Acnt:=0; END ELSE
                ELSE IF Acnt>=Opnd AND $0F THEN BEGIN Vol:=TVol;         Include(EventChg,eVol); Acnt:=0; END;
                inc (Acnt);
              END;
            FarRetrig      :
              IF CurrVblanc=Opnd THEN
              BEGIN
                Note:=IOffset;
                Include (EventChg, EInst);
                Inc(opnd,Pattern^[ii][CurrNote].operands);
              END;
            RetrigNote : IF Opnd<>0 THEN
              IF (CurrVBlanc<>0)AND(CurrVBlanc MOD Opnd=0) THEN
              BEGIN
                Note:=IOffset;
                IF (Mi.FileType=FT2)AND(SmpNum<>0)AND(NOT DoFadeOut) THEN InitEnvelope(II,SmpNum);
                Include (EventChg, EInst);
              END;
            RetrigVolSlide :BEGIN
              IF Acnt >= Opnd AND $0F THEN
              BEGIN
                Note:=IOffset;
                IF (Mi.FileType=FT2)AND(SmpNum<>0)AND(NOT DoFadeOut) THEN InitEnvelope(II,SmpNum);
                Include (EventChg,EInst);
                CASE Opnd SHR 4 OF
                 0 :;
                 1 :BEGIN dec (Vol, 1);       IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 2 :BEGIN dec (Vol, 2);       IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 3 :BEGIN dec (Vol, 4);       IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 4 :BEGIN dec (Vol, 8);       IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 5 :BEGIN dec (Vol,16);       IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 6 :BEGIN Vol:=2 * Vol DIV 3; IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 7 :BEGIN Vol:=    Vol DIV 2; IF NOT Vol IN [0..64]THEN Vol :=  0; Include(EventChg,eVol); END;
                 8 :;
                 9 :BEGIN inc (Vol, 1);       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 10:BEGIN inc (Vol, 2);       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 11:BEGIN inc (Vol, 4);       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 12:BEGIN inc (Vol, 8);       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 13:BEGIN inc (Vol,16);       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 14:BEGIN Vol:=3 * Vol DIV 2; IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                 15:BEGIN Vol:=2 * Vol;       IF NOT Vol IN [0..64]THEN Vol := 64; Include(EventChg,eVol); END;
                END;
                Acnt := 0;
              END;
              inc (Acnt);
             END;
            SlotRetrigger  :
              IF CurrVblanc=RetrOps THEN
              BEGIN
                Note:=IOffset;
                Include (EventChg, EInst);
                RetrOps:=(RetrOps+opnd)MOD Speed;
                DEC(OldVolume);
                IF OldVolume=0 THEN Effkt:=NoEffekt;
              END;
            NoteCut    :
              IF CurrVBlanc=opnd THEN IF MI.FileType IN [STM, S3M]
                THEN BEGIN note := 0; Include (EventChg, EReset); END
                ELSE BEGIN vol  := 0; Include(EventChg,eVol); END;
          END;
        END;
        FOR ii:=1 TO MI.NumTracks DO WITH Channels[ii] DO
        BEGIN
          {Process Envelope...}
          IF Mi.FileType=FT2 THEN WITH FT2Instruments^[Inst]DO
          BEGIN
            {Process Volume Envelope}
            IF ProcessEnvelope(EnvVol,EnvVolTck,EnvVolPos,NumVolPts,VolSustPt,VolLoopSt,VolLoopEnd,
                               DoFadeOut,VolEnvPts) THEN Include(EventChg,eVol);
            {Process Panning Envelope}
            IF ProcessEnvelope(EnvPan,EnvPanTck,EnvPanPos,NumPanPts,PanSustPt,PanLoopSt,PanLoopEnd,
                               DoFadeOut,PanEnvPts) THEN Include(EventChg,eVol);
            {Process auto Vibrato}
            ProcessAutoVibrato(II);
            {Process Fadeout}
            IF DoFadeOut AND(VolFadeOut<>0) THEN
            BEGIN
              Dec(FadeVol,2*VolFadeOut);
              IF FadeVol<0 THEN
              BEGIN
                DoFadeOut:=False;
                FadeVol:=0;
              END;
              Include(EventChg,eVol);
            END;
          END;
          {Process Pitch changes...}
          IF (ePitch IN EventChg)THEN
          BEGIN
            {Limit St3!}
            IF Mi.FileType IN [STM,S3M] THEN
            BEGIN
              {St3 underflow -> CUT!}
              IF NewTonh<=gen_lolimit THEN NewTonH:=0 ELSE
              {Periods below 128 -> Limit!}
              IF NewTonh<128 THEN NewTonH:=128;
            END;
            IF UseLinearTune
            THEN NoteVal:=NoteToVal(NewTonh, ITune, IFinTun)-FT2Samples^[smpnum].RelNote
            ELSE NoteVal:=NoteToVal(NewTonh, ITune, IFinTun);
            PlayTimes^[PlayTimeBuffer].Notes[II]:=NoteVal;
            IF NewFineTune=0
            THEN DoPHiPLo(II,NewTonh,Ofs(Plo),iFinTun)
            ELSE DoPHiPLo(II,NewTonh,Ofs(Plo),NewFineTune);
          END;
          {Process Volume and Panning changes...}
          IF eVol IN EventChg THEN VolAdjust(II);
          {}
          IF UseSurround AND(eInst IN EventChg) THEN
          BEGIN
            MixBuf:=MixBufferOfs;
            IF PanVal IN [0..15,240..255]THEN
            IF DoSurround(Inst,PanVal,NewTonh)
            THEN MixBuf:=SurBufferOfs
            ELSE MixBuf:=MixBufferOfs;
          END;
        END;
        {Process PlayTimes...}
        IF NOT Search THEN WITH PlayTimes^[PlayTimeBuffer] DO
        BEGIN
          Pattrn:=CurrPattern;
          PlayLine:=CurrNote;
          Time:=PlayTime;
          FOR i:=1 TO MI.NumTracks DO WITH Channels[i] DO
          BEGIN
            IF Effkt<>NoEffekt THEN Include(Effects[i],Effkt);
            IF OldChnVol<>DefChVol THEN
            BEGIN
              IF Mi.FileType=FT2 THEN
              BEGIN
                CASE OldChnVol OF
                  $00..$0F:{NIX}                ;
                  $10..$50:{Set Volume}         Include(Effects[i], SetVolume);
                  $60..$6F:{VolSldDn}           Include(Effects[i], VolumeSlide);
                  $70..$7F:{VolSldUp}           Include(Effects[i], VolumeSlide);
                  $80..$8F:{FineVolSldDn}       Include(Effects[i], FineVolDwn);
                  $90..$9F:{FineVolSldUp}       Include(Effects[i], FineVolUp);
                  $A0..$AF:{Set Vibrato Speed}  ;
                  $B0..$BF:{Vibrato}            Include(Effects[i], Vibrato);
                  $C0..$CF:{Set Panning}        Include(Effects[i], EPanning);
                  $D0..$DF:{PanSlide Left}      Include(Effects[i], PanSld{Left});
                  $E0..$EF:{PanSlide Right}     Include(Effects[i], PanSld{Right});
                  $F0..$FF:{Tone Portamento}    Include(Effects[i], Tone_Portamento);
                END;
              END ELSE Include(Effects[i], SetVolume);
            END;
            IF (Note>0)AND(AvgVol<>0) THEN
            BEGIN
              PlayInstr[i]:=Inst;
              Volumes[i]:=(lVol SHR 3)+(rVol SHL 1)AND $F0;
              IF NoteVal<>0 THEN Notes[I]:=NoteVal;
            END;
          END;
          BPM:=BPMSpeed;
          SPD:=Speed;
        END;
        IF Search THEN
        BEGIN
          IF SearchPattern
          THEN BEGIN IF (CurrPattern=SearchP)AND((CurrNote=SearchL)OR(SearchL=-1)) THEN Exit; END
          ELSE BEGIN IF (PlayTime>=SearchTime) OR PlayerRepeats THEN Exit; END;
        END ELSE IF ((OS<>Windows_NT)AND(SoundSettings.tMODE=MMTimer))OR(SoundSettings.DeviceID<DEV_HDD) THEN Exit;
Kernel:
        IF Search THEN
        BEGIN
          IF PeakSearch THEN
          BEGIN
            IF SoundSettings.SampleRate>1000 THEN
            BEGIN
              SaveChannels;{In HDD mode do an exact PeakSearch!}
              WHILE NOT ProcessSampleBlock(PlayerRepeats) DO;
              BEGIN
                IF Longint(MaxLevel)>Longint(32767) THEN
                BEGIN
                  RestoreChannels;
                  Dec(SoundSettings.PreAmp);
                  SetVolTable;
                  MaxLevel:=0;
                  GOTO Kernel;
                END;
              END;
            END ELSE WHILE NOT ProcessSampleBlock(PlayerRepeats) DO;
            IF ShowPeakSearch THEN SetPercent(100*PlayTime DIV Mi.TotalTime);
          END ELSE CalcSamplePositions;
        END ELSE
        {IF UseDAC THEN}
        BEGIN
          IF TmpBool THEN
          FOR ii := 1 TO Mi.NumTracks DO WITH Channels[ii] DO EventChg:=[];
          TmpBool:=ProcessSampleBlock(PlayerRepeats);
          IF NOT TmpBool THEN Exit;
          {PlaytimeBuffer handling}
          WITH PlayTimes^[PlayTimeBuffer] DO
          IF NextBytePos<WaveBytePos THEN
          BEGIN
            BytePos:=WaveBytePos;
            IF PlayTimeBuffer=MaxPlayBuffer THEN PlayTimeBuffer:=0 ELSE Inc(PlayTimeBuffer);
            FillChar(PlayTimes^[PlayTimeBuffer],SizeOf(tPlayTime),0);
            NextBytePos:=WaveBytePos+PlayBufferLen;
          END;
        END;
       {Effekte austesten}
        FOR ii:=1 TO MI.NumTracks DO WITH Channels[ii] DO
        BEGIN
          NewTonH:=TonH; {!TEMP!}
          {VolColumn FX}
          IF Mi.FileType=FT2 THEN
          CASE Byte(OldChnVol)OF
            $61..$6F:{VolSldDn}BEGIN DoVolumeSlide(OldChnVol AND $0F,Ofs(Vol)); Include(EventChg,eVol); END;
            $71..$7F:{VolSldUp}BEGIN DoVolumeSlide(OldChnVol SHL 4,Ofs(Vol)); Include(EventChg,eVol); END;
            $80..$8F:{FineVolSldDn}
              IF CurrVBlanc=0 THEN
              BEGIN
                dec(Vol,OldChnVol AND $0F);
                IF Vol>64 THEN Vol:=0;
                Include(EventChg,eVol);
              END;
            $90..$9F:{FineVolSldUp}
            BEGIN
              IF CurrVBlanc=0 THEN
              BEGIN inc(Vol,OldChnVol AND $0F); Include(EventChg,eVol); END;
            END;
            $D1..$DF:{PanSlide Left}BEGIN DoPanningSlide(OldChnVol AND $0F,Ofs(PanVal)); Include(EventChg,eVol); END;
            $E1..$EF:{PanSlide Right}BEGIN DoPanningSlide(OldChnVol SHL 4,Ofs(PanVal)); Include(EventChg,eVol); END;
            $B1..$BF:BEGIN
                       NewTonH:=DoVibrato(vops,Ofs(NewTonh),Ofs(VTini),VTable,False);
                       Include(EventChg,ePitch);
                     END;
            $F0..$FF:BEGIN
                       Inc(NewTonH,DoTone_Portamento(PrtNOps,Ofs(Tonh),Ofs(TpTon),Ofs(TpGlTon),GlissandoOn));
                       Include(EventChg,ePitch);
                     END;
          END;
          {first Tick FX...}
          IF CurrVBlanc=0 THEN
          CASE effkt OF
            FSldUp     :
              IF(tonh>0)THEN
              BEGIN
                i:=8*Opnd;
                IF tonh>14+I
                THEN BEGIN dec(tonh,i); dec(Newtonh,i) END
                ELSE BEGIN tonh:=14; NewTonh:=14; END;
                Include(EventChg,ePitch);
              END;
            FSldDwn    :
              IF(tonh>0)THEN
              BEGIN
                i:=8*Opnd;
                IF tonh<(54784-i)
                THEN BEGIN inc(tonh,i); inc(Newtonh,i) END
                ELSE BEGIN tonh:=54784; NewTonh:=54784; END;
                Include(EventChg,ePitch);
              END;
            XtraFSldUp   :
              IF (tonh>0)THEN
              BEGIN
                i:=2*Opnd;
                IF tonh>14+I
                THEN BEGIN dec(tonh,i); dec(Newtonh,i) END
                ELSE BEGIN tonh:=14; NewTonh:=14; END;
                Include(EventChg,ePitch);
              END;
            XtraFSldDwn  :
              IF (tonh>0)THEN
              BEGIN
                i:=2*Opnd;
                IF tonh<(54784-i)
                THEN BEGIN inc(tonh,i); inc(Newtonh,i) END
                ELSE BEGIN tonh:=54784; NewTonh:=54784; END;
                Include(EventChg,ePitch);
              END;
            FineVolUp    :BEGIN inc(Vol,opnd); Include(EventChg,eVol); END;
            FineVolDwn   :BEGIN dec(Vol,opnd); IF Vol>64 THEN Vol:=0; Include(EventChg,eVol); END;
            FinePanLeft  :BEGIN IF PanVal>17     THEN dec (PanVal,17) ELSE PanVal:=0;  Include(EventChg,eVol); END;
            FinePanRight :BEGIN IF PanVal<255-17 THEN inc (PanVal,17) ELSE PanVal:=255; Include(EventChg,eVol); END;
            OktSld1Up    :BEGIN NewTonh:=DoOktSldUp(Opnd,Ofs(Tonh)); Include(EventChg,ePitch); END;
            OktSld1Dwn   :BEGIN NewTonh:=DoOktSldDwn(Opnd,Ofs(Tonh)); Include(EventChg,ePitch); END;
            FarVolUp  :BEGIN inc(Vol,4*opnd); Include(EventChg,eVol); END;
            FarVolDwn :BEGIN dec(Vol,4*opnd); IF Vol>64 THEN Vol:=0; Include(EventChg,eVol); END;
            FarSldUp       :
              IF(tonh>0)THEN
              BEGIN
                IF tonh>(word(14)+word(48*Opnd))THEN dec(tonh,word(48*opnd))ELSE tonh:=14;
                NewTonh:=Tonh;
                Include(EventChg,ePitch);
              END;
            FarSldDwn      :
              IF(tonh>0)THEN
              BEGIN
                IF tonh<(54784-48*Opnd)THEN inc(tonh,48*opnd) ELSE tonh:=54784;
                NewTonh:=Tonh;
                Include(EventChg,ePitch);
              END;
            FrequAdjust: IF (tpton > 0) THEN
              BEGIN
                tonh := tpton;
                AdjustFrequencyUp (tonh, opnd);
                IF Tonh < 14 THEN Tonh := 14;
                NewTonh:=Tonh;
                Include(EventChg,ePitch);
              END;
          END;
          {Normal FX}
          CASE effkt OF
            NoEffekt       :;
            Portamento_up  : IF MI.FileType IN [f669, UNIS] THEN
                             BEGIN
                               IF(Tonh>0)THEN
                               BEGIN
                                 AdjustFrequencyUp(Tonh, Opnd);
                                 IF Tonh < 14 THEN BEGIN Tonh := 14; Effkt:=NoEffekt; END;
                                 NewTonh:=Tonh;
                                 Include(EventChg,ePitch);
                               END;
                             END ELSE
                             BEGIN
                               Inc(NewTonH,DoPortamento_up(opnd,Ofs(Tonh)));
                               Include(EventChg,ePitch);
                             END;
            Portamento_down: IF MI.FileType IN [f669, UNIS] THEN
                             BEGIN
                               IF(Tonh>0)THEN
                               BEGIN
                                 AdjustFrequencyDown(Tonh,Opnd);
                                 IF Tonh > 54784 THEN BEGIN Tonh := 54784; Effkt:=NoEffekt; END;
                                 NewTonh:=Tonh;
                                 Include(EventChg,ePitch);
                               END;
                             END ELSE
                             BEGIN
                               Inc(NewTonH,DoPortamento_down(opnd,Ofs(Tonh)));
                               Include(EventChg,ePitch);
                             END;
            Tone_portamento: IF MI.FileType IN [f669, UNIS] THEN
                             BEGIN
                               IF(Tonh>0)THEN
                               BEGIN
                                 IF Tonh < TpTon THEN
                                 BEGIN
                                   AdjustFrequencyDown(Tonh,Opnd);
                                   IF Tonh > TpTon THEN BEGIN Tonh := TpTon; Effkt:=NoEffekt; END;
                                   NewTonh:=Tonh;
                                   Include(EventChg,ePitch);
                                 END ELSE IF Tonh > TpTon THEN
                                 BEGIN
                                   AdjustFrequencyUp(Tonh,Opnd);
                                   IF Tonh < TpTon THEN BEGIN Tonh := TpTon; Effkt:=NoEffekt; END;
                                   NewTonh:=Tonh;
                                   Include(EventChg,ePitch);
                                 END;
                               END;
                             END ELSE
                             BEGIN
                               Inc(NewTonH,DoTone_Portamento(opnd,Ofs(Tonh),Ofs(TpTon),Ofs(TpGlTon),GlissandoOn));
                               Include(EventChg,ePitch);
                             END;
            FineVibrato,
            Vibrato        : IF MI.FileType IN [f669, UNIS] THEN
                             BEGIN
                               IF TPTon > 0 THEN
                               BEGIN
                                 CASE VTIni MOD 4 OF
                                   0, 2: Tonh := TPTon;
                                   1: BEGIN Tonh := TPTon; AdjustFrequencyUp   (Tonh, Opnd); END;
                                   3: BEGIN Tonh := TPTon; AdjustFrequencyDown (Tonh, Opnd); END;
                                 END;
                                 inc (VTIni);
                                 NewTonh:=Tonh;
                                 Include(EventChg,ePitch);
                               END;
                             END ELSE
                             BEGIN
                               NewTonH:=DoVibrato(opnd,Ofs(NewTonh),Ofs(VTini),VTable,Effkt = FineVibrato);
                               Include(EventChg,ePitch);
                             END;
            PortVolslide   : BEGIN
                               Inc(NewTonH,DoTone_Portamento(PrtNOps,Ofs(Tonh),Ofs(TpTon),Ofs(TpGlTon),GlissandoOn));
                               Include(EventChg,ePitch);
                               DoVolumeSlide(Opnd,Ofs(Vol));
                               Include(EventChg,eVol);
                             END;
            VibraVolslid   : BEGIN
                               NewTonH:=DoVibrato(vops,Ofs(NewTonh),Ofs(VTini),VTable,false);
                               Include(EventChg,ePitch);
                               DoVolumeSlide(Opnd,Ofs(Vol));
                               Include(EventChg,eVol);
                             END;
            VolumeSlide    : BEGIN
                               DoVolumeSlide(Opnd,Ofs(Vol));
                               Include(EventChg,eVol);
                             END;
            GlobVolSld     : BEGIN
                               DoVolumeSlide(Opnd,Ofs(GlobVol));
                               FOR i:=1 TO Mi.NumTracks DO Include(Channels[i].EventChg,eVol);
                             END;
            PanSld         : BEGIN DoPanningSlide(Opnd,Ofs(PanVal)); Include(EventChg,eVol); END;
            Tremolo        : BEGIN DoTremolo(Opnd,Ofs(Vol),Ofs(Volsld),Ofs(VTini),TTable); Include(EventChg,eVol); END;
            OktSldUp       : BEGIN
                               NewTonh:=DoOktSldUp(Opnd,Ofs(Tonh));
                               Include(EventChg,ePitch);
                             END;
            OktSldDwn      : BEGIN
                               NewTonh:=DoOktSldDwn(Opnd,Ofs(Tonh));
                               Include(EventChg,ePitch);
                             END;
            FARportamento  :
               BEGIN
                 IF FPTonh>Tonh THEN
                 BEGIN
                   Tonh:=FPTonh-Longint(FPVblanc)*LongInt(FPTonh-TpTon)DIV Opnd DIV Speed;
                   IF Tonh<TpTon THEN BEGIN Effkt:=NoEffekt; Tonh:=TpTon; END;
                 END ELSE
                 BEGIN
                   Tonh:=FPTonh+LongInt(FPVblanc)*LongInt(TpTon-FPTonh)DIV Opnd DIV Speed;
                   IF Tonh>TpTon THEN BEGIN Effkt:=NoEffekt; Tonh:=TpTon; END;
                 END;
                 INC(FPVblanc);
                 NewTonh:=Tonh;
                 Include(EventChg,ePitch);
               END;
            VolumePort:IF Vol=VolPrt THEN Effkt:=NoEffekt ELSE
              BEGIN
                IF VolPrt>Volsld
                THEN Vol:=VolSld+(Word(VPVBlanc)*Word(VolPrt-VolSld)DIV Speed DIV Opnd)
                ELSE Vol:=VolSld-(Word(VPVBlanc)*Word(VolSld-VolPrt)DIV Speed DIV Opnd);
                Include(EventChg,eVol);
                inc(VPVBlanc);
              END;
            VibratoSust,
            FarVibrato     :
              { ==> wrong!  FAR does the vibrato on the frequency similar to 669! <== }
              IF TPTon > 0 THEN IF FarVibDepth<>0 THEN
              BEGIN
                Tonh:=Longint(TPTon)-Longint(TPTon)*LongInt(SinTable[vtini])*FarVibDepth {DIV 32768} SHR 16;
                NewTonh:=Tonh;
                Include(EventChg,ePitch);
                INC(vtini,12*opnd);
              END;
          {TTMMPP}
          END;
        END;
        inc (CurrVBlanc);
        IncTime;
      UNTIL CurrVBlanc>=Speed; {bis nächste Notenzeile erreicht ist}
{******************Nächsten Speed-Takt bearbeitet***********************************}
      Include (RowPlayed [CurrPattern], CurrNote);
      IF NOT delaypattern THEN
      BEGIN
        FOR II:=MI.NumTracks DOWNTO 1 DO WITH Pattern^[ii][CurrNote],Channels[ii] DO
        IF Effekt=PattrnLoop THEN
        BEGIN
          IF Operands=0 THEN PatternLoopst:=CurrNote   ELSE
          BEGIN
            IF PatternLoops=0 THEN IF PatternLoopst<=CurrNote THEN PatternLoops:=Operands+1 ELSE ELSE
            IF PatternLoops=1 THEN BEGIN PatternLoops:=0; PatternLoopst:=CurrNote+1; END;
            IF PatternLoops>1 THEN
            BEGIN
              GetPattern(CurrPattern,CurrNote);
              CurrNote:=patternloopst;
              Dec(patternloops);
              GOTO NewPattern;
            END;
          END;
        END;
        pjump:=false;
        pbreak:=false;
        FOR II:=1 TO MI.NumTracks DO WITH Channels[ii] DO
        CASE effkt OF
          NoEffekt:;
          PositionJump   :IF (Opnd<MI.NumPatterns) THEN
                          BEGIN
                            pjump:=true;
                            jumpto:=opnd;
                          END;
          PatternBreak   :BEGIN
                            pbreak:=true;
                            IF UsePattBrkOps THEN breakto:=10*(opnd SHR 4)+(opnd AND $0F);
                          END;
        END;
        (*
        {IF Song is repeating without Repeat 1 ...let it play to it's end as if there where no Pattern Jump}
        IF  pjump
        AND(breakto IN RowPlayed[jumpto])
        AND(NOT RepeatSong)
        AND(SoundSettings.DeviceID<>DEV_HDD) THEN
        BEGIN
          pjump := false;
          pbreak := false;
          break; { jump to end of pattern }
        END;
        *)
        {Perform te Pattern Jump}
        IF pjump AND(Search OR(NOT OnlyOnePattern)) THEN
        BEGIN
          CurrPattern:=jumpto;
          GOTO EPJMP;
        END ELSE IF pbreak THEN GOTO S3M_Inc ELSE inc(CurrNote);
      END;
    UNTIL CurrNote>PtrnBreaks[CurrPattern];
{******************Nächste Notenzeile bearbeitet**********************************}
S3M_Inc:
    IF (NOT Search)AND(OnlyOnePattern) THEN
    BEGIN
      PlayTime:=PatternTimes[CurrPattern];
      RowPlayed[CurrPattern]:=[];
    END ELSE INC(CurrPattern);
    IF(MI.FileType IN[FT2,fFAR,UNIS,f669])AND(CurrPattern=MI.LoopFrom)THEN CurrPattern:=MI.LoopTo-1;
    IF CurrPattern>=MI.NumPatterns THEN
    BEGIN
      CurrPattern:=0;
      FOR i:=1 TO MI.NumTracks DO WITH channels[i] DO
      BEGIN
        GlobVol:=InitGlobVol;
        PlayTime:=0;
        Effkt:=NoEffekt;
        GlissandoOn:=False;
        PanVal:=DefPanning[i];
        tpton    :=0;
        TPGlTon  :=0;
        Volsld   :=0;
        VTini    :=0;
        Acnt     :=0;
        VOps     :=0;
        TOps     :=0;
        PrtOps   :=0;
        PrtNOps  :=0;
        OfsOps   :=0;
        ArpOps   :=0;
        VSldOps  :=0;
        fVuOps   :=0;
        fVdOps   :=0;
        PdelOps  :=0;
        RetrOps  :=0;
        Vtable   :=Ofs(sintable);
        Ttable   :=Ofs(sintable);
        Arp1Ops  :=0;
        Arp2Ops  :=0;
        OktSuOps :=0;
        OktSu1Ops:=0;
        OktSdOps :=0;
        OktSd1Ops:=0;
        OldVolume:=0;
        TVol     :=0;
        TremorOps:=0;
        VolPrt   :=0;
        VPVBlanc :=0;
        FPVBlanc :=0;
        FPTonh   :=0;
        Scont    :=0;
        OldChnVol:=0;
        gVSldOps :=0;
        VSldUpOps:=0;
        VSldDnOps:=0;
        fVSldUpOps:=0;
        fVSldDnOps:=0;
        PSldOps  :=0;
        PSldLOps :=0;
        PSldROps :=0;
        Vol      :=0;
        EventChg :=[eVol];
      END;
      ForceBPM:=True;
      DoSetSpeed(InitBpmSpeed);
      ForceBPM:=False;
      DoSetSpeed(InitSpeed);
      IF MI.FileType=fFar THEN DoFineFarTempo(0);
      FarVibDepth := 4;
    END;
EPJMP:
    {Song-Loops rausfinden und evtl unterbinden}
    IF PtrnBreaks[CurrPattern]<breakto THEN breakto:=0;
    IF (NOT OneChannelLoops)AND(breakto IN RowPlayed[CurrPattern]) THEN
    BEGIN
      PlayerRepeats:=True;
      IF CurrPattern=0 THEN PlayTime:=0 ELSE
      IF CurrPattern=MI.LoopTo-1 THEN
      BEGIN
        PlayTime:=LoopToTime;
        IF SoundSettings.DeviceID=DEV_HDD THEN
        BEGIN
          GlobalFadeOut:=True;
          GlobalFadeOutTime:=0;
          PlayerRepeats:=False;
        END;
      END ELSE PlayTime:=PatternTimes[CurrPattern];
      FillChar(RowPlayed,SizeOf(RowPlayed),0);
    END;
  UNTIL False{CurrPattern>=MI.NumPatterns};
{******************Nächstes Pattern bearbeitet*************************************}
END;

CONST LastWrittenBuffer:Word=0;
      OldLastBuffer:Word=0;
      SuspendWrite:WordBool=False;
      ProcessingUpdate:WordBool=False;
      LastUpdate:Longint=0;
      UnfilledBuffers:Longint=0;
PROCEDURE ProcessUpdate;
VAR Buffer:Word;
    ti:Word;
    MMT:tMMTime;
BEGIN
  IF ProcessingUpdate THEN Exit ELSE ProcessingUpdate:=True;
  LastUpdate:=TimeGetTime;
  IF SoundSettings.DeviceID>=DEV_HDD THEN
  BEGIN
    {PufferZeit berechnen}
    MI.BufTimeLen:=0;
    UnfilledBuffers:=0;
    FOR Buffer:=0 TO WaveBuffers-1 DO WITH WAVEHeader[Buffer] DO
    BEGIN
      IF Playing     THEN MI.BufTimeLen:=MI.BufTimeLen+wh.dwbufferlength ELSE
      IF DoUnPrepare THEN
      BEGIN
        WaveOutUnprepareHeader(WaveHandle,Addr(WaveHeader[Buffer]),SizeOf(tWaveHdr));
        DoUnPrepare:=False;
      END;
      IF NOT Filled THEN Inc(UnfilledBuffers);
    END;
    MI.BufTimeLen:=(MI.BufTimeLen)*(1000 DIV WaveFormat.wf.nBlockAlign)DIV Soundsettings.Samplerate;
    IF NOT Pause THEN
    BEGIN
      Buffer:=LastWrittenBuffer;
      IF RepeatSong THEN SuspendWrite:=False;
      WHILE (NOT SuspendWrite)
      AND   (NOT MODRepeats)
      AND   (WaveHeader[Buffer].Filled)
      AND   (NOT WaveHeader[Buffer].Playing)DO
      BEGIN
        {$IFDEF WAVE}
        IF (SoundSettings.DeviceID=DEV_HDD)THEN WITH WaveHeader[Buffer] DO
        BEGIN
          WriteWaveFile(RecordFile,RecFile32Bit,PlayMemSel,wh.dwUser,wh.dwUser+wh.dwbufferlength);
          Filled:=False;
          IF Repeating THEN MODRepeats:=True;
          Repeating:=False;
        END ELSE
        {$ENDIF}
        WITH WaveHeader[Buffer] DO
        BEGIN
          WaveOutPrepareHeader(WaveHandle,Addr(WaveHeader[Buffer]),SizeOf(TWaveHdr));
          WaveOutWrite(WaveHandle,Addr(WaveHeader[Buffer]),SizeOf(TWaveHdr));
          Playing:=True;
          IF Repeating AND (NOT RepeatSong) THEN SuspendWrite:=True;
        END;
        Buffer:=(Buffer+1)MOD WaveBuffers;
      END;
      LastWrittenBuffer:=Buffer;
      IF WaveHandle<>0 THEN
      BEGIN
        {gerade spielenden Block (LastBuffer) ermitteln}
        IF PlayTimeBuffer=0 THEN LastBuffer:=MaxPlayBuffer ELSE LastBuffer:=PlayTimeBuffer-1;
        MMT.wType:=Time_Bytes;
        WaveOutGetPosition(WaveHandle,@MMT,SizeOf(MMT));
        CASE MMT.wType OF
          Time_Bytes:;
          TIME_Samples:MMT.cb:=MMT.Sample*WaveFormat.wf.nBlockAlign;
          Time_MS:MMT.cb:=Trunc(1.0*MMT.ms*WaveFormat.wf.nBlockAlign*Soundsettings.Samplerate/1000);
        END;
        WHILE (MMT.cb<PlayTimes^[LastBuffer].BytePos)AND(LastBuffer<>PlayTimeBuffer)
        DO IF LastBuffer<>0 THEN Dec(LastBuffer) ELSE LastBuffer:=MaxPlayBuffer;
      END ELSE LastBuffer:=PlayTimeBuffer;
    END;
  END ELSE
  BEGIN
    LastBuffer:=PlayTimeBuffer;
  END;
  IF (MODRepeats){$IFDEF MCI}OR(PlayTimes^[LastBuffer].Time>=PlayEndTime){$ENDIF MCI} THEN
  BEGIN
    MODRepeats:=False;
    IF NOT RepeatSong THEN
    BEGIN
      {Wenn Player nich mehr spielt, aber noch Puffer ausgelesen werden...}
      ende:=true;
      IF (WaveHandle<>0) THEN WaveoutReset(WaveHandle);
      WaveBytePos:=0;
      NextBytePos:=0;
      pm_is_working:=false;
      MI.Playing:=False;
      {$IFNDEF MCI}
      MI.CurrTime:=MI.TotalTime;
      {$ELSE}
      MI.CurrTime:=PlayEndTime;
      {$ENDIF MCI}
      IF NOT PeakHold THEN MI.CPUUsage:=0;
      ProcessingUpdate:=False;
      Exit;
    END;
  END;
  IF (OldLastBuffer<>LastBuffer)OR(SoundSettings.DeviceID<DEV_HDD) THEN
  BEGIN
    WITH PlayTimes^[LastBuffer] DO
    BEGIN
      MI.CurrPttrn:=Pattrn+1;
      MI.CurrTime:=Time;
      MI.CurrEffects:=Effects;
      MI.CurrInstr:=PlayInstr;
      MI.CurrVols:=Volumes;
      MI.CurrNotes:=Notes;
      MI.CurrBPM:=BPM;
      MI.CurrSpd:=SPD;
      MI.CurrLine:=PlayLine+1;
    END;
    OldLastBuffer:=LastBuffer;
  END;
  ProcessingUpdate:=False;
END;

VAR CurrUsage:Longint;
CONST MixedOne:WordBool=False;
VAR FPU_BUF:ARRAY[0..255]OF Byte;
FUNCTION Playone(TIME:Longint):Boolean;
LABEL Again,Usage;
BEGIN
  Playone:=False;
  IF MODRepeats AND (NOT RepeatSong) THEN Exit;
  {Neuen Block berechnen??}
  IF (SoundSettings.DeviceID<DEV_HDD)OR(NOT WaveHeader[CurrBuffer].Filled)THEN
  {wenn zu berechnender Block gespielt wurde,...}
  BEGIN
    StartTime:=time; {Zeitnahme für CPU-Auslastung}
    {Save FPU State}
    IF SoundSettings.tMODE=MMTimer THEN asm fnsave [offset FPU_BUF] end;
Again:
    MixedOne:=True;
    PlayerRepeats:=False;
    IF (SoundSettings.DeviceID>=DEV_HDD) THEN
    IF (WaveEnd<>WaveHeader[CurrBuffer].wh.dwUser+BufferSize)THEN
    BEGIN
      WaveOfs:=WaveHeader[CurrBuffer].wh.dwUser;
      WaveEnd:=WaveOfs+BufferSize;
    END ELSE ELSE FillChar(PlayTimes^[PlayTimeBuffer],SizeOf(tPlayTime),0);
    play_the_Module; {neuen Block berechnen}
    IF SoundSettings.DeviceID>=DEV_HDD THEN
    BEGIN
      IF (WaveOfs>=WaveEnd) OR (PlayerRepeats)THEN
      WITH WaveHeader[CurrBuffer] DO
      BEGIN
        IF PlayerRepeats THEN Repeating:=True;
        wh.dwbufferlength:=WaveOfs-wh.dwUser;
        IF wh.dwbufferlength=0 THEN GOTO Again;
        Filled:=True;
        BytePos:=WaveBytePos;
      END;
    END ELSE IF PlayerRepeats THEN MODRepeats:=True;
    {Restore FPU State}
    IF SoundSettings.tMODE=MMTimer THEN ASM frstor [offset FPU_BUF] END;
    {Calculate CPU Load}
Usage:
    IF ((OS<>Windows_NT)AND(SoundSettings.tMODE=MMTimer))OR(SoundSettings.DeviceID<DEV_HDD)
    THEN CurrUsage:=100*Longint(TimeGetTime-StartTime)*1000*16 DIV (TimeStep SHR 12)
    ELSE CurrUsage:=100*(Longint(TimeGetTime-StartTime)*WaveFormat.wf.nAvgBytesPerSec
                         DIV Longint(WaveHeader[CurrBuffer].wh.dwBufferLength));
    {
    IF CurrUsage<90000 THEN GOTO Usage;
    {}
    IF (SoundSettings.DeviceID>=DEV_HDD)AND(WaveHeader[CurrBuffer].Filled) THEN
    BEGIN
      CurrBuffer:=(CurrBuffer+1)MOD WaveBuffers;
      PostAppMessage(TaskHandle,wm_User,0,0);
    END;
    IF (SoundSettings.DeviceID<DEV_HDD)
    AND(Abs(PlayTime-MI.CurrTime)>200)
    AND(Abs(PlayTime-MI.CurrTime)<8000)
    THEN PostAppMessage(TaskHandle,wm_User,0,0);
    IF PeakHold
    THEN IF MI.CPUUsage<CurrUsage THEN MI.CPUUsage:=CurrUsage ELSE
    ELSE IF ((OS<>Windows_NT)AND(SoundSettings.tMODE=MMTimer))OR(SoundSettings.DeviceID<DEV_HDD)
      THEN MI.CPUUsage:=(31*MI.CPUUsage+CurrUsage) DIV 32
      ELSE MI.CPUUsage:=( 4*MI.CPUUsage+CurrUsage) DIV 5;
    {End CPU Load}
    Playone:=True;
    PM_is_working:=true; {PlayModule arbeitet noch!}
  END;
END;

CONST NextMixTIME  :Longint=0;
      NextSurrTime :Longint=0;
      MixTIME      :Word=0;
      Do_KeyOff    :Boolean=False;
      Do_SurrKeyOff:Boolean=False;
      ProcessSurround:Boolean=True;

FUNCTION MixTimeInc:Longint;ASSEMBLER;
ASM
  DB 66h; XOR ax,ax
  mov ax,MixTIME
  DB 66h; add ax,word Ptr TimeStep
  mov MixTIME,ax
  DB 66h; SHR ax,16
  {shld edx,eax,16}DB 66h, 0Fh, 0A4h, 0C2h, 10h
END;

PROCEDURE FastWordSwap (var a, b: word); assembler;
asm
  les  si, a; mov di,word Ptr b;
  mov ax,word Ptr es:[si]; mov bx,word Ptr es:[di]
  mov word Ptr es:[si],bx; mov word Ptr es:[di],ax
end;

PROCEDURE MMTimerproc(wTimerID, wMsg:word; dwUser, dw1, dw2:longint); EXPORT;
VAR Ij:Longint;
LABEL _Exit;
BEGIN
  IF Ende THEN Exit;
  ASM
    mov  al,1
    xchg al,InCritical
    cmp  al,0
    jne  _Exit
  END;
  IF Catch(CatchBuf)=0 THEN
  BEGIN
    dw2:=TimeGetTime;
    IF dw2-LastUpdate>800 THEN {Wait to give Time to the system...} ELSE
    IF SoundSettings.DeviceID<DEV_HDD THEN
    BEGIN
      IF NextMixTIME<=dw2 THEN
      BEGIN
        Inc(NextMixTime,MixTimeInc);
        IF NextMixTIME<dw2 THEN NextMixTIME:=dw2;
        IF NOT PlayOne(dw2)THEN NextMixTime:=TimeGetTime+55;
      END;
    END ELSE
    {HDD,DAC...}
    IF (OS=Windows_NT)THEN PlayOne(dw2) ELSE
    IF dw2-NextMixTime>=0 THEN
    BEGIN
      IF (NOT PlayOne(dw2))OR(CurrUsage>98000)
      THEN NextMixTime:=TimeGetTime+55
      ELSE IF ((OS<>Windows_NT)AND(SoundSettings.tMODE=MMTimer))OR(SoundSettings.DeviceID<DEV_HDD)
        THEN IF (UnfilledBuffers>3)OR(UnfilledBuffers>WaveBuffers SHR 1)
          THEN NextMixTime:=dw2+(5000 DIV BPMSpeed) SHR 3
          ELSE NextMixTime:=dw2+(4500 DIV BPMSpeed) SHR 1
        ELSE NextMixTime:=dw2+50; {Fill buffer at 4 times speed}
    END;
  END;
  InCritical:=False;
_Exit:
END;

PROCEDURE AppTaskProc; EXPORT;
BEGIN
  IF Ende THEN Exit;
  IF SoundSettings.DeviceID=DEV_HDD THEN PlayOne(TimeGetTime);
  ProcessUpdate;
END;

CONST VxDTimerProc:Pointer=NIL;
FUNCTION VVxDTimerProc:Word; FAR;
BEGIN
  IF Ende THEN Exit;
  IF SoundSettings.tMode=MMTask THEN MMTimerProc(0,0,0,0,0);
  ProcessUpdate;
  VVxDTimerProc:=1;
END;

PROCEDURE NT_TimerProc(hwind:hwnd; msg:word; idTimer:word; Time:Longint); EXPORT;
BEGIN
  MMTimerProc(0,0,0,0,0);
  NextMixTime:=TimeGetTime;
END;

PROCEDURE BeginPause(Proceed:Boolean);   EXPORT;FORWARD;
PROCEDURE EndPause(Proceed:Boolean);     EXPORT;FORWARD;

CONST WatchTID:Integer=0;
PROCEDURE WatchTimerproc(hwind:hwnd; msg:word; idTimer:word; Time:Longint); EXPORT;
BEGIN
  IF NOT ende THEN
  IF(NOT Pause)AND(NOT MixedOne)THEN
  BEGIN
    BeginPause(False);
    EndPause(False);
  END ELSE MixedOne:=False;
END;


PROCEDURE TimerOn;
VAR I:Integer;
BEGIN
  ende:=false;
  MixTIME := 0;
  LastUpdate:=TimeGetTime;
  NextMixTIME:=LastUpdate+50;
  MixedOne:=False;
  UseSurround:=SoundSettings.Surround;
  IF PeakSearch THEN Exit;
  IF (SoundSettings.DeviceID<>DEV_HDD) THEN
  BEGIN
    {$IFDEF PrtLayer}
    StopTimer(VxDTID);
    IF VxDTID<>0 THEN StopTimer(VxDTID);
    VxDTID:=StartTimer(VxDTimerProc,100);
    {$ELSE}
    SetTaskProc(VxDTimerProc);
    {$ENDIF}
  END;{ }
  {$IFNDEF DEBUG}
  IF (SoundSettings.TMode=MMTimer)THEN
  IF (OS<>Windows_NT)OR(SoundSettings.DeviceID<DEV_HDD) THEN
  BEGIN
    IF MMTID<>0 THEN timeKillEvent(MMTID);
    MMTID:=timeSetEvent(1,1,MMTimerProc,0,time_Periodic);
  END ELSE
  BEGIN
    IF MMTID<>0 THEN KillTimer(0,MMTID);
    MMTID:=SetTimer(0,1,1,@NT_TimerProc);
  END;
  {$ENDIF}
  IF (SoundSettings.TMode=WMTIMER)THEN
  BEGIN
    IF MMTID<>0 THEN KillTimer(0,MMTID);
    MMTID:=SetTimer(0,1,1,@NT_TimerProc);
  END;
  IF WatchTID<>0 THEN KillTimer(0,WatchTID);
  WatchTID:=SetTimer(0,4444,3000,@WatchTimerProc);
END;

CONST TimerOffing:Boolean=False;
PROCEDURE TimerOff;
BEGIN
  IF TimerOffing THEN Exit;
  TimerOffing:=True;
  Ende:=True;
  IF WatchTID<>0   THEN KillTimer(0,WatchTID);
  IF VxDTID<>0   THEN {$IFDEF PrtLayer} StopTimer(VxDTID); {$ELSE} SetTaskProc(NIL); {$ENDIF}
  IF MMTID<>0 THEN
  {$IFNDEF DEBUG}
  IF ((OS<>Windows_NT)OR(SoundSettings.DeviceID<DEV_HDD))AND(SoundSettings.TMode=MMTIMER)
  THEN timeKillEvent(MMTID)
  ELSE {$ENDIF} KillTimer(0,MMTID);
  WHILE InCritical DO Yield;
  VxDTID  :=0;
  MMTID   :=0;
  WatchTID:=0;
  TimerOffing:=False;
END;

PROCEDURE WaveOutCallBack(hWaveOut, wMsg:Word; dwInstance, dwParam1, dwParam2:Longint);EXPORT;
BEGIN
  IF (wMsg=WOM_DONE)THEN WITH pMyWaveHDR(dwParam1)^ DO
  BEGIN
    IF Repeating THEN pWord(dwInstance)^:=1;
    DoUnPrepare:=True;
    Playing:=False;
    Filled:=False;
    Repeating:=False;
  END;
END;

PROCEDURE CalcVolMaster;
VAR MV  : Extended;
    Amp : Extended;
BEGIN
  IF SoundSettings.OverSamp>1
  {Cubic splines can overamp the original sample by 25%=2dB}
  THEN Amp:=exp10((SoundSettings.PreAmp AND $7F-22)/20)
  ELSE Amp:=exp10((SoundSettings.PreAmp AND $7F-20)/20);
  IF (Mi.FileType<>S3M)AND(Mi.NumTracks>4)
  THEN MV:=256*8/(4+(MI.NumTracks-4)*0.2)
  ELSE MV:=256*8/MI.NumTracks;
  IF (Mi.FileType=S3M)AND(8*Mastervol>MV) THEN MV:=8*Mastervol;
  IF SControl
  THEN VolMaster:=Round(4*MV*Amp)
  ELSE VolMaster:=Round(1*MV*Amp);
END;

PROCEDURE SetVolTable; ASSEMBLER;
ASM
  {Nach bl die Anzahl der zu spielenden Kanäle...}
  cmp PlayMemSel,0
  je  @Exit
  call CalcVolMaster
  DB 66h; mov dx,word Ptr VolMaster
  {VolumeTable berechnen...}
  {mov gs,PlayMemSel}DB 8Eh,2Eh; DW OFFSET PlayMemSel
  DB 66h; XOR si,si  {0..64}
  mov cx,0ffffh
  DB 66h; SHL cx,16
  mov cx,-128       {-128 bis 127}
  XOR di,di
@VolLoop:
  Db 66h; mov ax,cx
  {imul eax,esi} DB 66h, 0Fh,0Afh,0C6h
  {imul eax,edx} DB 66h, 0Fh,0Afh,0C2h
  Db 66h; sar ax,9
  {cmp eax,-32768}DB 66h,3dh,00h,80h,0FFh,0FFh
  jnl @Vol0
  mov ax,-32768
  jmp @Vol1
@Vol0:
  {cmp eax, 32767}DB 66h,3dh,0FFh,7Fh,00h,000h
  jng @Vol1
  mov ax,32767
@Vol1:
  {movzx bx,cl} DB 0Fh,0B6h,0D9h
  add bx,bx
  {mov word ptr gs:[di+bx],ax}DB 65h,89h,01h
  DB 66h; inc cx
  cmp cx,128
  jl @VolLoop
  mov cx,0ffffh
  DB 66h; SHL cx,16
  mov cx,-128
  add di,200h
  inc si
  cmp si,65
  jl @VolLoop
@Exit:
END;

FUNCTION long2ptr(s:word; l:longint):pointer; NEAR; ASSEMBLER;
ASM
  {Segment berechnen}
  mov ax,word Ptr l+2
  mul selectorinc
  mov dx,s
  add dx,ax
  {Offset schreiben}
  mov ax,word Ptr l
  {Ergebnis steht in DS:AX}
END;

{*************************SoundDevice Aktivieren************************************}
PROCEDURE ActivateSound(VAR Error:word);
BEGIN
  Error:=0;
  IF GetProfileInt('Player32','PalTiming',0)<>0
  THEN DefMODTuning:=Trunc(fPAL/2/428)
  ELSE DefMODTuning:=Trunc(fNTSC/2/428);
  WITH soundsettings DO
  BEGIN
    CalcVolMaster;
    WaveBuffers:=nBuffers;
    CASE deviceID OF
      dev_HDD: BufferSize:=32768;
      ELSE
      BEGIN
        BufferSize:=((Longint(Samplerate)*stereo*(Bits DIV 8) DIV 5+31)DIV 32)*32;
        PlayBufferLen:=Longint(WaveBuffers)*Longint(BufferSize) DIV (MaxPlayBuffer+1);
      END;
    END;
    IF SoundSettings.DeviceID>=DEV_HDD THEN
    BEGIN
      PlayMemOfs:=((PlayMemOfs+31)DIV 32)*32;
      IF NOT(FitPlayMem(PlayMemOfs+Longint(WaveBuffers)*Longint(BufferSize)))THEN BEGIN Error:=id_NoMem; Exit; END;
      SetVolTable;
      WITH WaveFormat,wf DO
      BEGIN
        wformattag:=1;
        nchannels:=stereo;
        nsamplespersec:=samplerate;
        navgbytespersec:=longint(samplerate)*stereo*(bits DIV 8);
        nblockalign:=stereo*(bits DIV 8);
        wbitspersample:=bits;
      END;
      WaveHandle:=0;
      IF (DeviceID>=-1)AND(DeviceID<255)
      THEN k:=waveoutopen(@WaveHandle,deviceid,@WaveFormat,Longint(@WaveOutCallBack),Longint(@MODRepeats),CALLBACK_FUNCTION)
      ELSE k:=0;
      IF k=0 THEN
      BEGIN
        WaveBytePos:=0;
        NextBytePos:=0;
        FillChar(WaveHeader,SizeOf(WaveHeader),0);;
        FOR i:=0 TO WaveBuffers-1 DO WITH WaveHeader[i],wh DO
        {Block Header für Blöcke festlegen}
        BEGIN
          dwuser:=Longint(PlayMemOfs)+Longint(i)*Longint(BufferSize);
          lpdata:=pchar(long2ptr(PlayMemSel,dwUser));
        END;
      END ELSE Error:=k+10000;
    END;
    IF Error=0 THEN
    BEGIN
      IF PlayTimes<>NIL THEN
      BEGIN
        GlobalPageUnLock(SelectorOf(PlayTimes));
        GlobalFreePtr(PlayTimes);
      END;
      PlayTimes:=NIL;
      PlayTimes:=GlobalAllocPtr(gmem_NoDiscard,Word(SizeOf(tPlayTimes)));
      IF PlayTimes=NIL THEN BEGIN Error:=ID_NoMem; Exit; END;
      GlobalPageLock(SelectorOf(PlayTimes));
    END ELSE PlayTimes:=NIL;
  END;
END;

{*************************SoundDevice DeAktivieren**********************************}
PROCEDURE DeActivateSound;
BEGIN
  TimerOff;
  PM_Is_Working:=False;
  IF Wavehandle>0 THEN
  BEGIN
    WaveBytePos:=0;
    NextBytePos:=0;
    WaveoutReset(WaveHandle);
    WaveoutClose(WaveHandle);
  END;
  WaveHandle:=0;
  FitPlayMem(PlayMemOfs);{ }
  IF PlayTimes<>NIL THEN
  BEGIN
    GlobalPageUnLock(SelectorOf(PlayTimes));
    GlobalFreePtr(PlayTimes);
  END;
  PlayTimes:=NIL;
END;

PROCEDURE KanalInit;
VAR p:pointer;
BEGIN
  timeroff;
  NoteStretch:=65536.0*8.0*428.0*DefMODTuning;
  pm_is_working:=false;
  GlobVol:=InitGlobVol;
  IF WaveHandle<>0 THEN waveoutreset(WaveHandle);
  FOR i:=0 TO WaveBuffers-1 DO WITH WaveHeader[i] DO
  BEGIN
    wh.dwFlags:=0;
    BytePos:=0;
    Playing:=False;
    Filled:=False;
    DoUnprepare:=False;
    Repeating:=False;
  END;
  SamplesRemain:=0;
  BufSamples:=0;
  WaveBytePos:=0;
  NextBytePos:=0;
  CurrBuffer:=0;
  PlayTimeBuffer:=0;
  LastBuffer:=0;
  LastWaveBuffer:=0;
  OldLastBuffer:=LastBuffer;
  CurrPattern:=0;
  PlayTime:=0;
  IF NOT PeakHold THEN MI.CPUUsage:=0;
  MI.BufTimeLen:=0;
  {
  MI.CurrPttrn:=0;
  Fillchar(MI.CurrEffects,Sizeof(Effectstate),0);
  Fillchar(MI.CurrVols,Sizeof(tByteState),0);
  Fillchar(MI.CurrNotes,Sizeof(tByteState),0);
  {}
  ForceBPM:=True;
  DoSetSpeed(InitBpmSpeed);
  ForceBPM:=False;
  DoSetSpeed(InitSpeed);
  IF MI.FileType=fFar THEN DoFineFarTempo(0);
  FarVibDepth := 4;
  DelayPattern:=false;
  FillChar(RowPlayed,SizeOf(RowPlayed),0);
  IF PlayTimes<>NIL THEN FillChar(PlayTimes^,Word(SizeOf(tPlayTimes)),0);
  FillChar(Channels,SizeOf(Channels),0);
  FOR i:=1 TO MI.NumTracks DO WITH channels[i] DO
  BEGIN
    patternloopst:=0;
    patternloops:=0;
    Effkt:=NoEffekt;
    Vtable:=Ofs(sintable);
    Ttable:=Ofs(sintable);
    GlissandoOn:=False;
    PanVal:=DefPanning[i];
    PlayChannel:=-1;
    CASE SoundSettings.OverSamp OF
      1:JmpFlags:=fOverSamp;
      2:JmpFlags:=fCPUCubic;
      3:JmpFlags:=fFPUCubic;
      ELSE JmpFlags:=0;
    END;
    MixBuf:=MixBufferOfs;
  END;
  IF PlayMemSel <> 0 THEN
  BEGIN
    p:=long2ptr(Playmemsel+8,0);
    FillChar(p^,SurrBufferLen,0);
  END;
  BufEnd:=0;
  BufStart:=Loword(Longint(8)*Longint(Soundsettings.Samplerate)DIV 1000*SurrLen);
  WaveEnd:=MaxLongInt;
  WaveOfs:=MaxLongInt;
  RFadeOut:=0;
  LFadeOut:=0;
  RSurFadeOut:=0;
  LSurFadeOut:=0;
  GlobalFadeOut:=False;
  GlobalFadeOutTime:=0;
  LastWrittenBuffer:=0;
  PlayerRepeats:=False;
  MODRepeats:=False;
  SuspendWrite:=False;
  ProcessingUpdate:=False;
END;
