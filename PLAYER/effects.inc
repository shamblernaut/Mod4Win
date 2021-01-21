{Include Datei zur Effektberechnung}

CONST
  gen_lolimit = 14     ; { [B-9] }  gen_hilimit = 1712 *32; { 54784 [C-0] }

PROCEDURE DoVoiceHigher; ASSEMBLER;
{DS:DI-->Voice
 Cl:Steps
 RETURN: CX:NewVoice}
ASM
     or cl,cl
     jz @Out0
     cmp UseLinearTune,0
     je @Amiga
     {Freq:=Tune*2^(6-(Period/128/12)==>128*Opnd abziehen}
     mov ax,[di]
     XOR ch,ch
     SHL cx,7
     sub ax,cx
     mov cx,ax
     jmp @out
@Amiga:
 {Das folgende berechnet CX:=DS:[DI]*2^(-cx/12)}
     XOR ch,ch
     mov bx,Halfnote
     mov ax,$FFFF
@1:  mul bx
     mov ax,dx
     loop @1
     mov cx,[di]
     mul cx
     mov cx,dx
     jmp @Out
@Out0:
     mov cx,[di]
@Out:
END;

PROCEDURE DoVoiceLower; ASSEMBLER;
{DS:DI-->Voice
 Cl:Steps
 RETURN: CX:NewVoice}
ASM
     or cl,cl
     jz @Out0
     cmp UseLinearTune,0
     je @Amiga
     {Freq:=Tune*2^(6-(Period/128/12)==>128*Opnd zuzählen}
     XOR ch,ch
     SHL cx,7
     add cx,word Ptr [di]
     jmp @Exit
@Amiga:
 {Das folgende berechnet CX:=DS:[DI]*2^(cx/12)}
     {AND ecx,000000ffh} DB 66h,81h,0e1h,0ffh,00,00,00
     DB 66h; mov bx,word Ptr Halfnote1
     DB 66h; XOR ax,ax
     dec ax
@1:  DB 66h; mul bx
     DB 66h; SHR ax,16
     DB 66h; SHL dx,16
     DB 66h; add ax,dx
     loop @1
     mov cx,[di]
     DB 66h; mul cx
     DB 66h; cmp dx,0
     je @2
     mov cx,gen_hilimit
     jmp @Exit
@2:  DB 66h; SHR ax,16
     mov cx,ax
     jmp @Exit
@Out0:
     mov cx,[di]
@Exit:
END;

{**********************************ARPEGGIO***********************************}
FUNCTION DoArpeggio(ops:byte; oAcnt,oTon:Word):Word; NEAR; ASSEMBLER;
ASM
          {Für weiter unten DS:DI auf TON stezen}
          mov di,oton
          mov cx,[di]
          cmp ops,0
          je  @apprd
          {Nächsten Halbtonschritt holen INC(ACNT MOD 3)}
          mov si,oAcnt
          mov cl,[si]
          dec cl
          cmp cl,3
          jb  @appra
          mov cl,2
@appra:   mov Byte Ptr [si],cl     {in acnt steht der Halbtonschritt (0,1,2)}
          {Operands laden (0,x,y)}
          XOR bx,bx
          mov bl,ops
          SHL cl,2
          SHR bl,cl
          mov cl,bl
          AND cx,$000f       {In cx steht die Anzahl der Halbtonschritte zu Schritt 0}
          {Wenn Originalnote-->keine Berechnung der Halbtonschritte}
          jcxz @apprc
          call DoVoiceHigher
          jmp @apprd
          {In cx neue Tonhöhe}
@apprc:   mov cx,[di]
@apprd:   mov ax,cx
END;
{******************************OKTaARPEGGIO*Up*Down***************************}
FUNCTION DoArpeggio1(ops:byte; oAcnt,oTon:Word):WORD; NEAR; ASSEMBLER;
ASM
          {Für weiter unten DS:DI auf TON stezen}
          mov di,oton
          mov cx,[di]
          cmp ops,0
          je  @apprd
          {Nächsten Halbtonschritt holen INC(ACNT MOD 3)}
          mov si,oAcnt
          mov cl,[si]
          inc cl
          cmp cl,4
          jb  @appra
          XOR cl,cl
@appra:   mov Byte Ptr [si],cl     {in acnt steht der Halbtonschritt (0,1,2,3)}
          {Operands laden (0,x,y)}
          cmp cl,1
          je  @apprc
          cmp cl,3
          je  @apprc
          cmp cl,0
          jne @apprb
          mov cl,ops
          SHR cl,4
          call DoVoiceHigher
          jmp @apprd
@apprb:   mov cl,ops
          AND cl,0fh
          call DoVoiceLower
          jmp @apprd
          {In cx neue Tonhöhe}
@apprc:   mov cx,[di]
@apprd:   mov ax,cx
END;
{******************************OKTaARPEGGIO*Down*UP***************************}
FUNCTION DoArpeggio2(ops:byte; oAcnt,oTon:Word):Word; NEAR; ASSEMBLER;
ASM
          {Für weiter unten DS:DI auf TON stezen}
          mov di,oton
          mov cx,[di]
          cmp ops,0
          je  @apprd
          {Nächsten Halbtonschritt holen INC(ACNT MOD 3)}
          mov si,oAcnt
          mov cl,[si]
          inc cl
          cmp cl,4
          jb  @appra
          XOR cl,cl
@appra:   mov Byte Ptr [si],cl     {in acnt steht der Halbtonschritt (0,1,2,3)}
          {Operands laden (0,x,y)}
          cmp cl,1
          je  @apprc
          cmp cl,3
          je  @apprc
          cmp cl,0
          jne @apprb
          mov cl,ops
          SHR cl,4
          call DoVoiceLower
          jmp @apprd
@apprb:   mov cl,ops
          AND cl,0fh
          call DoVoiceHigher
          jmp @apprd
          {In cx neue Tonhöhe}
@apprc:   mov cx,[di]
@apprd:   mov ax,cx
END;

{***************** Get delta value for portamento effects *******************}
FUNCTION GetDeltaPeriod (Ops: byte): word; assembler; asm
          {OriginalTon um (8*(Speed-1)/speed)*Operands*(Takt+1) Einheiten erhöhen}
          DB 66h; XOR ax,ax
          DB 66h; XOR dx,dx
          DB 66h; XOR bx,bx
          {(Speed-1)*8...}
          mov al,speed
          dec al
          shl ax,3
          {...*Operands}
          mov bl,Ops
          DB 66h; mul bx
          {...*(Takt+1)}
          mov bl,CurrVBlanc
          inc bl
          DB 66h; mul bx
          {.../Speed}
          mov bl,speed
          DB 66h; DIV bx
          {Überlauf-Prüfung}
          DB 66h; mov dx,ax
          DB 66h; SHR dx,16
          { set zero flag if no overflow }
          cmp dx,0
          je @out
          mov ax,$ffff
@out:
END;

{*******************************PORTAMENTO UP********************************}
FUNCTION DoPortamento_up(Ops:BYTE; oTon:Word):Integer; NEAR; ASSEMBLER;
ASM
          mov di,oton
          mov cx,[di]
          cmp ops,0
          je  @pub
          push word Ptr Ops
          call GetDeltaPeriod
          je  @0
          mov cx,gen_lolimit
          jmp @2
@0:       mov cx,[di]
          cmp cx,ax
          ja  @1
          mov cx,gen_lolimit
          jmp @2
@1:       sub cx,ax
@2:       mov dl,CurrVBlanc {Am Ende einer Notenzeile erreichten Ton merken!}
          inc dl
          cmp dl,Speed
          jne @pub
          mov [di],cx
@pub:     mov ax,cx
          sub ax,[di]
END;

{*******************************PORTAMENTO DOWN******************************}
FUNCTION DoPortamento_down(Ops:BYTE; oTon:Word):Integer; NEAR; ASSEMBLER;
ASM
          mov di,oton
          mov cx,[di]
          cmp ops,0
          je  @pdb
          push word Ptr Ops
          call GetDeltaPeriod
          je  @0
          mov cx,gen_hilimit
          jmp @1
@0:       mov cx,[di]
          add cx,ax
          {Überlauf-Prüfung}
          jnc @1
          mov cx,gen_hilimit
@1:       mov dl,CurrVBlanc {Am Ende einer Notenzeile erreichten Ton merken!}
          inc dl
          cmp dl,Speed
          jne @pdb
          mov [di],cx
@pdb:     mov ax,cx
          sub ax,[di]
END;

{*******************************Tone PORTAMENTO******************************}
FUNCTION DoTone_portamento(Ops:BYTE; oTon,oTpTon,oTpGlTon:Word; GlissandoOn: boolean):Integer; NEAR; ASSEMBLER;
ASM
          {[DI]-->TON; [SI]-->Zu erreichender Ton}
          mov di,oton
          mov cx,[di]
          cmp Ops,0
          je @tpb
          mov si,otpton
          cmp word Ptr [si],0
          je  @tpb
          mov cx,[si]
          cmp cx,[di]
          je  @tpb
          push word Ptr Ops
          call GetDeltaPeriod
          je  @0         { ax is the delta adjustment }
          mov cx,[si]
          jmp @tpa
@0:       mov dx,[si]    { dx is the destination period }
          mov cx,[di]    { cx is the current tonh }
          {Glissando oder normal?}
          cmp GlissandoOn,0
          je  @00
          mov bx,oTpGlTon
          mov cx,[bx]
@00:      cmp cx,dx      {Ton erhöhen oder verringern?}
          jb  @tpua
          {verringern!}
          sub cx,ax      { cx is the new tonh now }
          {Überlauf-Prüfung}
          ja  @1
          mov cx,dx
          jmp @tpa
@1:       cmp  GlissandoOn,0
          je   @2
          {Glissando...}
          push bx
          push cx
          push dx
          mov  cx,1
          call DoVoiceHigher
          mov  ax,cx
          pop  dx
          pop  cx
          pop  bx
          cmp  cx,ax
          jna  @11
          mov  ax,cx
          mov  cx,[di]
          jmp  @tpa
@11:      mov  cx,ax
          mov  [di],cx
@2:       cmp cx,dx      {END-Tonhöhe erreicht?}
          jae @tpa
          mov cx,dx      {END-Tonhöhe erreicht!!}
          jmp @tpa
@tpua:    {erhöhen!}
          add cx,ax
          {Überlauf-Prüfung}
          jnc @3
          mov cx,dx
          jmp @tpa
@3:       cmp  GlissandoOn,0
          je   @4
          {Glissando...}
          push bx
          push cx
          push dx
          mov  cx,1
          call DoVoiceLower
          mov  ax,cx
          pop  dx
          pop  cx
          pop  bx
          cmp  cx,ax
          jnb  @31
          mov  ax,cx
          mov  cx,[di]
          jmp  @tpa
@31:      mov  cx,ax
          mov  [di],cx
@4:       cmp cx,dx      {END-Tonhöhe erreicht?}
          jbe @tpa
          mov cx,dx      {END-Tonhöhe erreicht!!}
@tpa:     mov dl,CurrVBlanc {Am Ende einer Notenzeile erreichten Ton merken!}
          inc dl
          cmp dl,Speed
          jne @tpb
          cmp GlissandoOn,0
          je  @5
          mov [bx],ax
          jmp @tpb
@5:       mov [di],cx
@tpb:     mov ax,cx
          sub ax,[di]
END;

{********************* 669/UNIS PORTAMENTO UP *******************************}
PROCEDURE AdjustFrequencyUp(VAR Toneh: word; Ops: byte); assembler;
CONST Scale: longint = 1522*4;
      Scale1:longint = 1592*4;
asm
          db 66h; XOR dx, dx    { EDX := 0 }
          db 66h; XOR ax, ax
          db 66h; dec  ax       { EAX := $FFFFFFFF }
          db 66h; XOR bx, bx
          les  di, Toneh
          mov  bx, es:[di]
          db 66h; DIV  bx       { EAX := EDX:EAX div Toneh }
          db 66h; mov bx, ax
          db 66h; XOR ax, ax
          mov al, Ops
          cmp al, 0
          je  @Exit
          add al,al
          dec al                { Ops := 2 * Ops - 1 }
          db 66h; mov cx, word Ptr Scale
          cmp Mi.FileType,f669
          jne @0
          db 66h; mov cx, word Ptr Scale1
  @0:     db 66h, 0fh, 0afh, 0c1h { imul eax, ecx }
          db 66h; add bx, ax    { NewFreq := Freq + const * Ops }
          jnc @1
          mov ax,gen_lolimit
          mov es:[di],ax
          jmp @Exit
  @1:     db 66h; XOR dx, dx    { EDX := 0 }
          db 66h; XOR ax, ax
          db 66h; dec  ax       { EAX := $FFFFFFFF }
          db 66h; DIV  bx       { EAX := EDX:EAX div new NewFreq }
          mov es:[di], ax
          db 66h; rol  ax, 16
          OR  ax, ax
          jz  @Exit
          mov ax, gen_hilimit
          mov es:[di], ax
  @Exit:
END;

{********************* 669/UNIS PORTAMENTO DOWN *****************************}
PROCEDURE AdjustFrequencyDown(VAR Toneh: word; Ops: byte); assembler;
CONST Scale: longint = 1522*4;
      Scale1:longint = 1592*4;
asm
          db 66h; XOR dx, dx    { EDX := 0 }
          db 66h; XOR ax, ax
          db 66h; dec  ax       { EAX := $FFFFFFFF }
          db 66h; XOR bx, bx
          les  di, Toneh
          mov  bx, es:[di]
          db 66h; DIV  bx       { EAX := EDX:EAX div Toneh }
          db 66h; mov bx, ax
          db 66h; XOR ax, ax
          mov al, Ops
          cmp al, 0
          je  @Exit
          add al,al
          dec al                { Ops := 2 * Ops - 1 }
          db 66h; mov cx, word Ptr Scale
          cmp Mi.FileType,f669
          jne @0
          db 66h; mov cx, word Ptr Scale1
  @0:     db 66h, 0fh, 0afh, 0c1h { imul eax, ecx }
          db 66h; cmp bx,ax
          jbe @2Big
          db 66h; sub bx, ax    { new Toneh := Toneh - const * Ops }
          db 66h; XOR dx, dx    { EDX := 0 }
          db 66h; XOR ax, ax
          db 66h; dec  ax       { EAX := $FFFFFFFF }
          db 66h; DIV  bx       { EAX := EDX:EAX div new Toneh }
          mov es:[di], ax
          db 66h; rol  ax, 16
          cmp  ax, 0
          jz  @Exit
  @2Big:  mov ax, gen_hilimit
          mov es:[di], ax
  @Exit:
END;

{***************** Get New Pointer for Vibrato/Tremolo **********************}
FUNCTION GetNewPointer (Ops: byte): word; assembler; asm
  {Berechnen des Zeigers auf die Sinustable}
  {Operands=XY Hex}
  { MOD: X*Speed/64 [Perioden/Line] ==> X/64 [Perioden/vBlanc]}
  { MOD: OldPointer+=(VBlank+1)             *  X * (256/64)}
  {                +=(VBlank+1)             *  X * 4       }
  { S3M: OldPointer+=(VBlank+1) * (Speed-1) *  X * (256/64) / Speed }
  {                +=(VBlank+1) * (Speed-1) *  X * 4 / Speed        }
  { MTM: OldPointer+=(VBlank+1) *  Speed    *  X * (256/64) / Speed * (125/Tempo) }
  {                +=(VBlank+1) *  125      *  X * 4 / Tempo                      }
  { 669: OldPointer+=(VBlank+1) *  Speed    * 16 * (256/64) / Speed }
  {                +=(VBlank+1)             * 64                    }
          cmp Mi.FileType,f669
          je  @1
          cmp Mi.FileType,UNIS
          je  @1
          jmp @2
@1:       {669}
          mov al,CurrVBlanc
          inc al
          SHL ax,6
          jmp @out
@2:       {MOD/STM/S3M/MTM}
          mov al,ops
          SHR al,2
          AND al,00111100b    { al = Ops shr 4 * 4 }
          mov bl,CurrVBlanc
          inc bl
          mul bl              { ax = (VBlank+1) * X * 4 }
          cmp Mi.FileType,S3M
          je  @3
          cmp Mi.FileType,STM
          je  @3
          cmp Mi.FileType,MTM
          je  @MTM1
        { jmp @out }          { do MOD like STM/S3M }
@3:       {STM/S3M}
          XOR bh,bh
          mov bl,Speed
          dec bl
          mul bx
          inc bl
          DIV bx              { S3M: ax = ax * (Speed-1) / Speed }
          jmp @out
@MTM1:    {MTM}
          mov bx,125
          mul bx
          mov bl,BPMSpeed
          DIV bx              { MTM: ax = ax * 125 / Tempo }
@out:     cmp ah,0            { sets ZF if AH = 0 }
END;

{**********************************Vibrato***********************************}
FUNCTION DoVibrato(Ops:BYTE; oTon,oVTini,VTable:Word; fine: boolean):Word; NEAR; ASSEMBLER;
ASM
          mov di,oton
          mov cx,[di]
          {DS:DI-->VTini}
          mov di,oVTini
          push word Ptr Ops
          call GetNewPointer   { if AX > 255 --> will be auto adjusted by period of sine wave }
          add al,[di]
          mov bl,CurrVBlanc
          inc bl
          cmp bl,Speed
          jne @vibc
          mov byte Ptr [di],al
@vibc:    XOR ah,ah            {in AL steht der Zeiger auf die Tabelle}
          {DS:SI-->Tabelle}
          mov si,Vtable
          add si,ax
          mov al,byte Ptr [si] {in AL steht der Wert aus der Sinustabelle}
          {Berechnen von 8*4*Y*table^+Ton}
          mov bl,ops
          AND bl,$0f
          imul bl
          sar ax,3             { * 8*4 / 256 }
          cmp fine,0
          je  @nofine
          sar ax,2             { fine vibrato is 1/4 as deep }
@nofine:  cmp Mi.FileType, MTM
          jne @MTM2
          sar ax,1             { MTM vibrates half as deep }
@MTM2:    {DS:SI-->Ton}
          mov si,oTon
          mov cx,[si]
          {FehlerCheck}
          OR  ax,ax
          jns @down     { test sign }
          sub cx,ax     { cx += -ax }
          jc  @Check
          mov cx,gen_hilimit
          jmp @Check
@down:    sub cx,ax
          jnc @Check
          mov cx,gen_lolimit
@Check:   mov ax,cx
END;
{**********************************Tremolo***********************************}
PROCEDURE DoTremolo(Ops:BYTE; oVolume,oVolsld,oVTini,TTable:WORD); NEAR; ASSEMBLER;
ASM
          {DS:DI-->VTini}
          mov di,oVTini
          {Berechnung des Zeigers auf die Sinustabelle (Start bei pi/4==64!!!)}
          push word Ptr Ops
          call GetNewPointer   { if AX > 255 --> will be auto adjusted by period of sine wave }
          add al,[di]
          mov bl,CurrVBlanc
          inc bl
          cmp bl,Speed
          jne @trec
          mov byte Ptr [di],al
@trec:    XOR ah,ah            {in AL steht der Zeiger auf die Sinustabelle}
          mov si,ttable
          add si,ax
          mov al,byte Ptr [si] {in AL steht der Wert aus der Sinustabelle}
          {Berechnung von 4*Y*table^+Volume1}
          mov bl,ops
          AND bl,$0f
          imul bl
          sar ax,6             { * 4 / 256 }
          cmp Mi.FileType, MTM
          jne @MTM2
          sar ax,1             { MTM vibrates half as deep }
@MTM2:    {DS:SI-->VolSld}
          XOR ch,ch
          mov si,oVolSld
          mov cl,[si]
          sub cx,ax               {in CX steht die neue Lautstärke}
@Check:   cmp ch,0                {Definitionsbereich überprüfen...}
          je  @trea
          XOR cx,cx
          jmp @treb
@trea:    cmp cx,65
          jb  @treb
          mov cx,64
@treb:    mov di,oVolume
          mov byte Ptr [di],cl          {Lautstärke setzen}
END;

{**********************************VolumeSlide*******************************}
PROCEDURE DoVolumeSlide(Ops:Byte; oVolume:Word); NEAR;
BEGIN
  IF (CurrVBlanc <> 0) OR FastSlides THEN
  ASM
          {DS:SI-->Volume}
          mov si,ovolume
          {Lautstärke um (X oder Y)*(Takt+1)*(Speed-1)/Speed Einheiten erhöhen oder verringern}
          mov al,ops
          cmp Mi.Filetype,STM
          je  @00
          cmp Mi.Filetype,S3M
          jne @01
@00:      { STM/S3M - ignore x }
          test al,$0F
          jz   @02
          jmp  @va
@01:      { MOD/MTM - ignore y }
          test al,$F0
          jz   @va
@02:      {Erhöhen!...}
          SHR al,4
          mov bl,[si]
          add bl,al
          cmp bl,$40 {Definitionsbereich prüfen}
          jbe @vb
          mov bl,$40
          jmp @vb
          {Verringern!...}
@va:      AND al,0fh
          mov bl,[si]
          sub bl,al
          jnb @vb
          mov bl,0
          jmp @vb
@vb:      mov byte Ptr [si],bl
  END;
END;

{**********************************PanningSlide*******************************}
PROCEDURE DoPanningSlide(Ops:Byte; oPanVal:Word); NEAR;
BEGIN
  IF (CurrVBlanc <> 0) OR FastSlides THEN
  ASM
          {DS:SI-->Volume}
          mov si,oPanVal
          {Panning um (X oder Y)*(Takt+1)*(Speed-1)/Speed Einheiten erhöhen oder verringern}
          mov al,ops
          { ignore LeftSpeed }
          test al,$F0
          jz   @va
@02:      {Erhöhen!...}
          SHR al,4
          mov bl,[si]
          add bl,al
          jnc @vb   {Definitionsbereich prüfen}
          mov bl,$FF
          jmp @vb
          {Verringern!...}
@va:      AND al,0fh
          mov bl,[si]
          sub bl,al
          jnb @vb
          mov bl,0
          jmp @vb
@vb:      mov byte Ptr [si],bl
  END;
END;

FUNCTION DoOktSldUp(Ops:BYTE; oTon:Word):Word; ASSEMBLER;
ASM
          mov di,oton
          mov cx,[di]
          mov cl,ops
          call DoVoiceHigher
          mov [di],cx
          mov ax,cx
END;

FUNCTION DoOktSldDwn(Ops:BYTE; oTon:Word):Word; ASSEMBLER;
ASM
          mov di,oton
          mov cx,[di]
          mov cl,ops
          call DoVoiceLower
          mov [di],cx
          mov ax,cx
END;

PROCEDURE DoPlayOffset(CONST Chn:tChannel); NEAR; ASSEMBLER;
ASM
  les di,Chn
  DB 26h,66h; mov ax,word Ptr tChannel([di]).IEnd
  DB 66h; or ax,ax
  je @1
  {IF Note is of, retrigger sample}
  DB 26h,66h; mov ax,word Ptr tChannel([di]).Note
  DB 66h; or ax,ax
  jne @0
  DB 66h; XOR ax,ax
  mov word ptr tChannel(es:[di]).Rest,0
  mov ah,tChannel(es:[di]).opnd
  DB 26h,66h; add ax,word Ptr tChannel([di]).IOffset
  jmp @01
@0:
  {normal SetOffset}
  DB 66h; XOR ax,ax
  mov word ptr tChannel(es:[di]).Rest,0
  mov ah,tChannel(es:[di]).opnd
  DB 26h,66h; add ax,word Ptr tChannel([di]).Note
@01:
  DB 26h,66h; cmp ax,word Ptr tChannel([di]).IEnd  { too large? }
  jbe @1
  DB 26h,66h; cmp word Ptr tChannel([di]).LEnd, 0  { is it a looped sample? }
  je @2
  DB 26h,66h; mov dx,word Ptr tChannel([di]).LLn   { yes, value to subtract is loop length }
  jmp @3
@2:
  DB 26h,66h; mov dx,word Ptr tChannel([di]).IEnd  { no, value to subtract is sample length }
  DB 26h,66h; sub dx,word Ptr tChannel([di]).IOffset
@3:
  DB 66h; sub ax, dx                               { subtract value until we're in the valid range }
  DB 26h,66h; cmp ax,word Ptr tChannel([di]).IEnd
  jae @3
@1:
  DB 26h,66h; mov word Ptr tChannel([di]).Note,ax
END;

CONST ForceBPM:Boolean=False;
PROCEDURE DoSetSpeed(Ops:byte); NEAR; ASSEMBLER;
ASM
          DB 66h; XOR cx,cx
          mov cl,ops
          OR cx,cx
          jnz @0
          cmp Mi.FileType,UNIS
          {jne @01}
          jne @Exit            { disregard speed 0 }
          {UNIS-Speed 0}
          mov cl,InitBPMSpeed
          mov ax,3
          mul cl
          mov cl,al
          mov byte Ptr Speed,1
          jmp @setbpm
{@01:      inc cx}
@0:       cmp Forcebpm,0
          jne @setBPM
          cmp Mi.FileType,UNIS
          je  @NoBPM
          cmp Mi.FileType,f669
          je  @NoBPM
          cmp Mi.FileType,S3M
          je  @setvblanc
          cmp soundsettings.NoBPM,0
          jne @NoBPM
          cmp cl,1fh
          jna  @setvblanc
@setBPM:  {BPMSpeed:=Ops}
          cmp cl,20h
          jae @1
          mov cl,20h
@1:       mov byte Ptr BPMSpeed,cl
          {TimeStep:=$9C40000 DIV BPMSpeed}
          XOR dx,dx
          mov ax,9C4h
          DIV cx
          mov word Ptr TimeStep+2,ax
          XOR ax,ax
          DIV cx
          mov word Ptr TimeStep,ax
          {NumSamples:=trunc(2.5*soundsettings.samplerate/BPMSpeed)}
          DB 66h; mov ax,word ptr soundsettings.samplerate
          {EBX:=EAX/2}
          DB 66h; mov bx,ax
          DB 66h; SHR bx,1
          {EAX:=EAX*2}
          DB 66h; SHL ax,1
          {EAX:=EAX+EBX}
          DB 66h; add ax,bx
@noadd:   {EAX:=EAX/BPM-Speed}
          DB 66h; xor dx,dx
          DB 66h; DIV cx
          mov word Ptr NumSamples,ax
          jmp @Exit
@NoBPM:   jcxz @Exit
          mov byte Ptr Speed,cl
          mov cl,InitBPMSpeed
          jmp @SetBPM
@setvblanc:
          jcxz @Exit
          mov byte Ptr Speed,cl
@Exit:
END;

(*
procedure DoSetSpeed (Ops: byte);
  begin
    if FileType = MTM then if Ops < 32 then begin
      DoSetSpeed1 (125);
      DoSetSpeed1 (Ops);
    end else begin
      DoSetSpeed1 (6);
      DoSetSpeed1 (Ops);
    end else DoSetSpeed1 (Ops);
  end;
*)

{
  New_BPM   128/speed + x   128 + x * speed
  ------- = ------------- = ---------------
  Def_BPM     128/speed           128
}

VAR FarSpeedCorr: integer;

PROCEDURE DoFineFarTempo(Spd: shortint);
VAR FarBPMSpeed: Longint;
BEGIN
  IF Spd=0 THEN FarSpeedCorr := 0 ELSE inc (FarSpeedCorr, Spd);
  FarBPMSpeed:=80 * longint (128+Speed*FarSpeedCorr) DIV 128;
  IF FarBPMSpeed < 1 THEN FarBPMSpeed := 1 ELSE
  IF FarBPMSpeed>255 THEN FarBPMSpeed:=255;
  ForceBPM:=True;
  DoSetSpeed(FarBPMSpeed);
  ForceBPM:=False;
END;
