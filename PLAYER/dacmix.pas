{$N+,E-}
UNIT DACMIx;
{$C FIXED PRELOAD PERMANENT}
INTERFACE
USES PlayDefs,WinProcs,modc;
     { 0 | x |   x   |   x   | x |   x x   | 0 }
     {   | 8 | 16Bit | Pitch | O | S,R,L,M |   }
     {     40   20      10     8     4 2       }
CONST fFPUCubic =$60;
      fCPUCubic =$40;
      fOverSamp =$20;
      f16Bit    =$10;
      fHiPitch  =$08;
      fPanned   =$06;
      fRight    =$04;
      fLeft     =$02;
      fMono     =$00;

VAR SamplesRemain,BufSamples:Word; {Anzahl der zu berechnenden Samples im nächsten Block}
    LFadeOut:Longint;
    RFadeOut:Longint;
    LSurFadeOut:Longint;
    RSurFadeOut:Longint;

FUNCTION ProcessSampleBlock(AmRepeating:Boolean): boolean;
PROCEDURE CalcSamplePositions;

IMPLEMENTATION

FUNCTION GetNewSmpVal(Channel,Vol:Longint):Longint;ASSEMBLER;
VAR c,d:Longint;
ASM
      DB 66h; xor di,di
      lea di,channels
      cmp PlayMemSel,0
      JE @Exit
      mov es,PlayMemSel
      mov ax,type tChannel
      mov cx,word ptr Channel
      dec cx
      mov bx,cx
      cmp [bx].DisAbled.byte,0
      jne @Exit {Channel Disabled -> Out}
      mul cx
      add di,ax
      DB 66h; mov si,word ptr tChannel([di]).Note
      DB 66h; or si,si
      jz @Exit  {No Sample playing ->Out}
      DB 66h; xor dx,dx
      mov dx,word ptr tChannel([di]).Rest
@@0:  DB 66h; cmp si,word Ptr tChannel([di]).IEnd
      jbe @@1
      DB 66h; sub si,word Ptr tChannel([di]).LLn
      jns @@0
      jmp @Exit {No Sample playing ->Out}

  @JmpTable: {SHR 3}
      DW Offset @NoOverSamp08
      DW Offset @NoOverSamp16
      DW Offset @OverSamp08
      DW Offset @OverSamp16
      DW Offset @Cubic08
      DW Offset @Cubic16
      DW Offset @fCubic08
      DW Offset @fCubic16

@@1:  mov bx,tChannel([di]).JmpFlags
      shr bx,4
      add bx,bx
      {mov fs,CodeSel}DB 8eh,26h; DW OFFSET CodeSel
      DB 64H; jmp word ptr [Offset @JmpTable+bx]
@NoOverSamp08:
      {mov fs,PlayMemSel}DB 8eh,26h; DW OFFSET PlayMemSel
      {mov bl,es:[esi]} DB 26h,67h,8ah,1eh
      mov bh,byte ptr Vol
      add bx,bx
      {movsx ebx,fs:[bx]} DB 64h,66h,0Fh,0BFh,1Fh;
      DB 66h; shl bx,8
      DB 66h; mov ax,bx
      DB 66h; mov dx,bx
      DB 66h; sar dx,16
      jmp @Success
@NoOverSamp16:
      {movsx ebx,es:[esi+esi]} DB 26h,67h,66h,0fh,0bfh,1Ch,36h
      DB 66h; mov ax,word ptr Vol
      {IMUL EAX,VolMaster}  DB 66h,0Fh,0AFh,06h; DW Offset VolMaster
      DB 66h; sar ax,9
      {imul ebx,eax} DB 66h,0Fh,0AFh,0D8h
      DB 66h; mov ax,bx
      DB 66h; mov dx,bx
      DB 66h; sar dx,16
      jmp @Success
@OverSamp08:
      {mov fs,PlayMemSel}DB 8eh,26h; DW OFFSET PlayMemSel
      {mov bl,es:[esi+1]} DB 26h,67h,8ah,5eh,01h
      mov bh,byte ptr Vol
      add bx,bx
      {movsx eax,fs:[bx]} DB 64h,66h,0Fh,0BFh,07h
      {mov bl,es:[esi]} DB 26h,67h,8ah,1eh
      mov bh,byte ptr Vol
      add bx,bx
      {movsx ebx,fs:[bx]} DB 64h,66h,0Fh,0BFh,1Fh;
      DB 66h; sub ax,bx
      {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
      DB 66h; sar ax,8
      DB 66h; shl bx,8
      DB 66h; add ax,bx
      DB 66h; mov dx,ax
      DB 66h; sar dx,16
      jmp @Success
@OverSamp16:
      {movsx ebx,es:[esi+esi]} DB 26h,67h,66h,0fh,0bfh,1Ch,36h
      {movsx eax,es:[esi+esi+2]} DB 26h,67h,66h,0fh,0bfh,44h,36h,02h
      DB 66h; sub ax,bx
      DB 66h; sar ax,1
      {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
      DB 66h; sar ax,15
      DB 66h; add bx,ax
      DB 66h; mov ax,word ptr Vol
      {IMUL EAX,VolMaster}  DB 66h,0Fh,0AFh,06h; DW Offset VolMaster
      DB 66h; sar ax,9
      {imul ebx,eax} DB 66h,0Fh,0AFh,0D8h
      DB 66h; mov ax,bx
      DB 66h; mov dx,bx
      DB 66h; sar dx,16
      jmp @Success
@Cubic08:
    DB 26h,67h,66h,0Fh,0BEh,4Eh,0FFh{movsx ecx,byte ptr es:[esi-1]}   {ecx=Ym1}
    DB 26h,67h,66h,0Fh,0BEh,1Eh     {movsx ebx,byte ptr es:[esi]  }   {ebx=Y0 }
    DB 26h,67h,66h,0Fh,0BEh,46h,01h {movsx eax,byte ptr es:[esi+1]}   {eax=Y1 }
    DB 66h; mov word ptr d,bx
    DB 66h; sub bx,ax           {ebx=Y0-Y1 }
    DB 66h; sub ax,cx           {eax=Y1-Ym1 = c }
    DB 66h; mov word ptr c,ax
    DB 26h,67h,66h,0Fh,0BEh,46h,02h {movsx eax,byte ptr es:[esi+2]}   {eax=Y2 }
    DB 66h; add ax,bx           {eax=(Y0-Y1)+Y2 }
    DB 66h; add ax,bx           {eax=2*(Y0-Y1)+Y2 }
    DB 66h; sub bx,cx           {ebx=Y0-Y1-Ym1 }
    DB 66h; add ax,bx           {eax=3*(Y0-Y1)+Y2-Ym1 = a }
    DB 66h; add bx,ax           {ebx=4*(Y0-Y1)+Y2-2*Ym1 = a+Y0-Y1-Ym1 }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=a*X }
    DB 66h; add bx,word ptr d   {a+Y0-Y1-Ym1+Yo = -b }
    DB 66h; sar ax,16           {eax=(a*X)>>16 }
    DB 66h; shl word ptr d,8    {d=356*d because of 8Bit->16Bit conversion }
    DB 66h; sub ax,bx           {eax= a*X-(a+Y0-Y1-Ym1)-Yo = a*X+b }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=(a*X+b)*X }
    DB 66h; sar ax,16           {eax=((a*X+b)*X)>>16 = (a*X+b)*X }
    DB 66h; add ax,word ptr c   {eax=(a*X+b)*X+Y1 }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=((a*X+b)*X+Y1-Ym1)*X }
    DB 66h; sar ax,9            {eax=(((a*X+b)*X+Y1-Ym1)*X)>>9 = ((a*X+b)*X+c)*X }
    DB 66h; add ax,word ptr d   {eax=((a*X+b)*X+c)*X+Y0 ...viola :-)) }
    jmp @Cubic_Vol
@Cubic16:
    DB 26h,67h,66h,0Fh,0BFh,4Ch,36h,0FEh{movsx ecx,word ptr[esi+esi-2]}   {ecx=Ym1}
    DB 26h,67h,66h,0Fh,0BFh,1Ch,36h     {movsx ebx,word ptr[esi+esi]  }   {ebx=Y0 }
    DB 26h,67h,66h,0Fh,0BFh,44h,36h,02h {movsx eax,word ptr[esi+esi+2]}   {eax=Y1 }
    DB 66h; mov word ptr d,bx
    DB 66h; sub bx,ax           {ebx=Y0-Y1 }
    DB 66h; sub ax,cx           {eax=Y1-Ym1 = c }
    DB 66h; mov word ptr c,ax
    DB 26h,67h,66h,0Fh,0BFh,44h,36h,04h {movsx eax,word ptr[esi+esi+4]}   {eax=Y2 }
    DB 66h; add ax,bx           {eax=(Y0-Y1)+Y2 }
    DB 66h; add ax,bx           {eax=2*(Y0-Y1)+Y2 }
    DB 66h; sub bx,cx           {ebx=Y0-Y1-Ym1 }
    DB 66h; add ax,bx           {eax=3*(Y0-Y1)+Y2-Ym1 = a }
    DB 66h; add bx,ax           {ebx=4*(Y0-Y1)+Y2-2*Ym1 = a+Y0-Y1-Ym1 }
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=a*X }
    DB 66h; add bx,word ptr d   {a+Y0-Y1-Ym1+Yo = -b }
    DB 66h; sar ax,13           {eax=(a*X)>>16 }
    DB 66h; sub ax,bx           {eax= a*X-(a+Y0-Y1-Ym1)-Yo = a*X+b }
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=(a*X+b)*X }
    DB 66h; sar ax,13           {eax=((a*X+b)*X)>>16 = (a*X+b)*X }
    DB 66h; add ax,word ptr c   {eax=(a*X+b)*X+Y1 }
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=((a*X+b)*X+Y1-Ym1)*X }
    DB 66h; sar ax,14           {eax=(((a*X+b)*X+Y1-Ym1)*X)>>9 = ((a*X+b)*X+c)*X }
    DB 66h; add ax,word ptr d   {eax=((a*X+b)*X+c)*X+Y0 ...viola :-)) }
@Cubic_Vol:
    { Volume }
    DB 66h; mov bx,word ptr Vol
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW VolMaster
    DB 66h; sar bx,9
    {imul eax,ebx}DB 66h,0Fh,0AFh,0C3h
    {shld edx,eax,10} DB 66h,0Fh,0A4h,0C2h,10h
    jmp @Success
@fCubic08:
    DB 66h; mov bx,word ptr Vol
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW VolMaster
    DB 66h; mov word ptr c,bx
    fild dword ptr c          {lVol}
    fmul OneBy4
    DB 26h,67h,0Fh,0BEh,46h,0FFh{movsx ax,byte ptr [esi-1]}; mov word ptr c,ax; fild word ptr c
    DB 26h,67h,0Fh,0BEh,06h{movsx ax,byte ptr [esi]}; mov word ptr c,ax; fild word ptr c
    fild tChannel([di]).Rest    {->X,Y0,Ym1,lVol }
    fmul OneBy65536
    DB 26h,67h,0Fh,0BEh,46h,01h{movsx ax,byte ptr [esi+1]}; mov word ptr c,ax; fild word ptr c
    fld  st(2)                  {->Y0,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(1)               {->Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    DB 26h,67h,0Fh,0BEh,46h,02h{movsx ax,byte ptr [esi+2]}; mov word ptr c,ax; fild word ptr c
    jmp @fCubicCore
@fCubic16:
    DB 66h; mov bx,word ptr Vol
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW VolMaster
    DB 66h; mov word ptr c,bx
    fild dword ptr c          {lVol}
    fmul OneBy1024
    DB 26h,67h,0DFh,44h,36h,0FEh{fild word ptr [esi+esi-2]   {->Ym1,lVol }
    DB 26h,67h,0DFh,04h,36h     {fild word ptr [esi+esi]     {->Y0,Ym1,lVol }
    fild tChannel([di]).Rest    {->X,Y0,Ym1,lVol }
    fmul OneBy65536
    DB 26h,67h,0DFh,44h,36h,02h {fild word ptr [esi+esi+2]   {->Y1,X,Y0,Ym1,lVol }
    fld  st(2)                  {->Y0,Y1,X,Y0,Ym1,lVol }
    fsub st,st(1)               {->Y0-Y1,Y1,X,Y0,Ym1,lVol }
    DB 26h,67h,0DFh,44h,36h,04h {fild word ptr [esi+esi+4]   {->Y2,Y0-Y1,Y1,X,Y0,Ym1,lVol }
@fCubicCore:
    fsub st,st(5)               {->Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,lVol }
    fadd st,st(1)               {->(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,lVol }
    fadd st,st(1)               {->2*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,lVol }
    fadd st,st(1)               {->3*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,lVol }
    fadd st(1),st               {->3*(Y0-Y1)+Y2-Ym1,4*(Y0-Y1)+Y2-Ym1,Y1,X,Y0,Ym1,lVol }
    fmul st,st(3)               {->a*X,a+Y0-Y1,Y1,X,Y0,Ym1,lVol }
    fsubrp st(1),st             {->a*X-(a+Y0-Y1),Y1,X,Y0,Ym1,lVol }
    fadd st,st(4)               {->a*X-(a+Y0-Y1)+Ym1,Y1,X,Y0,Ym1,lVol }
    fsub st,st(3)               {->a*X-(a+Y0-Y1)+Ym1-Y0,Y1,X,Y0,Ym1,lVol }
    fmul st,st(2)               {->(a*X+b)*X,Y1,X,Y0,Ym1,lVol }
    faddp st(1),st              {->(a*X+b)*X+Y1,X,Y0,Ym1,lVol }
    fsubrp st(3),st             {->X,Y0,(a*X+b)*X+Y1-Ym1,lVol }
    fmulp st(2),st              {->Y0,((a*X+b)*X+c)*X,lVol }
    fadd  st(1),st              {->Y0,((a*X+b)*X+c)*X+Y0,lVol }
    faddp st(1),st              {->((a*X+b)*X+c)*X+2*Y0,lVol }
    { Output }
    fmulp st(1),st               {->Y*lVol }
    fistp dword ptr c
    DB 66h; mov ax,word ptr c
    mov dx,word ptr c+2
    jmp @Success
@Exit:
      xor dx,dx
      xor ax,ax
@Success:
END;

PROCEDURE CalcSamplePositions;ASSEMBLER;
ASM
          mov cx,Mi.NumTracks
          add cx,NumEffTracks
          DB 66h; XOR bx,bx
          mov bx,NumSamples
@Start:   mov di,OFFSET Channels
          mov ax,TYPE tChannel
          mul cx
          sub ax,TYPE tChannel
          add di,ax
          DB 66h; mov si,word Ptr tChannel([DI]).note
          DB 66h; OR si,si
          jz @2
          DB 66h; mov ax,word Ptr tChannel([DI]).Plo
          DB 66h; mul bx
          {SHRD eax,edx,10h} DB 066h,00Fh,00ACh,0D0h,010h
          DB 66h; add si,ax
          DB 66h; cmp si,word Ptr tChannel([DI]).IEnd
          jbe @1
          DB 66h; mov si,word Ptr tChannel([DI]).IEnd{}
          DB 66h; sub si,word Ptr tChannel([DI]).LLn
          jns @1{}
          DB 66h; xor si,si
@1:       DB 66h; mov word Ptr tChannel([DI]).Note,si
@2:       dec cx
          jnz @Start
END;

FUNCTION ProcessSampleBlock(AmRepeating:Boolean): boolean;ASSEMBLER;
ASM
{******************Nächsten Sampleblock bearbeiten**********************************}
          {mov fs,CodeSel}DB 8eh,26h; DW OFFSET CodeSel
          mov ax,ds
          DB 64h; mov [offset @DS],ax
          DB 64h,66h; mov word ptr [Offset @EBP],bp
          DB 64h,66h; mov word ptr [Offset @ESP],sp
          jmp @KERNEL
  @DS:    DW 0000;
  @CX:    DW 0000;
  @DI:    DW 0000;
  @EBP:   DD 00000000;
  @ESP:   DD 00000000;
  @JmpTable:
          DW Offset @M08        {0000}
          DW Offset @L08        {0002}
          DW Offset @R08        {0004}
          DW Offset @S08        {0006}
          DW Offset @M081       {0008}
          DW Offset @L081       {000A}
          DW Offset @R081       {000C}
          DW Offset @S081       {000E}
          DW Offset @M16        {0010}
          DW Offset @L16        {0012}
          DW Offset @R16        {0014}
          DW Offset @S16        {0016}
          DW Offset @M161       {0018}
          DW Offset @L161       {001a}
          DW Offset @R161       {001c}
          DW Offset @S161       {001e}

          DW Offset @M08o       {0020}
          DW Offset @L08o       {0022}
          DW Offset @R08o       {0024}
          DW Offset @S08o       {0026}
          DW Offset @M08o1      {0028}
          DW Offset @L08o1      {002a}
          DW Offset @R08o1      {002c}
          DW Offset @S08o1      {002e}
          {BEGIN TEMP}
          DW Offset @M16o1      {0030}
          DW Offset @L16o1      {0032}
          DW Offset @R16o1      {0034}
          DW Offset @S16o1      {0036}
          {END TEMP}
          DW Offset @M16o1      {0038}
          DW Offset @L16o1      {003a}
          DW Offset @R16o1      {003c}
          DW Offset @S16o1      {003e}
          {CPU CUBIC Spline}
          (**)
          DW Offset @Cs08LoS    {0040}
          DW Offset @Cs08LoS    {0042}
          DW Offset @Cs08LoS    {0044}
          DW Offset @Cs08LoS    {0046}
          DW Offset @Cs08HiS    {0048}
          DW Offset @Cs08HiS    {004a}
          DW Offset @Cs08HiS    {004c}
          DW Offset @Cs08HiS    {004e}
          DW Offset @Cs16HiS    {0050}
          DW Offset @Cs16HiS    {0052}
          DW Offset @Cs16HiS    {0054}
          DW Offset @Cs16HiS    {0056}
          DW Offset @Cs16HiS    {0058}
          DW Offset @Cs16HiS    {005a}
          DW Offset @Cs16HiS    {005c}
          DW Offset @Cs16HiS    {005e}
          (**)
          {FPU CUBIC Spline}
          DW Offset @fCs08LoS   {0060}
          DW Offset @fCs08LoS   {0062}
          DW Offset @fCs08LoS   {0064}
          DW Offset @fCs08LoS   {0066}
          DW Offset @fCs08HiS   {0068}
          DW Offset @fCs08HiS   {006a}
          DW Offset @fCs08HiS   {006c}
          DW Offset @fCs08HiS   {006e}
          DW Offset @fCs16LoS   {0070}
          DW Offset @fCs16LoS   {0072}
          DW Offset @fCs16LoS   {0074}
          DW Offset @fCs16LoS   {0076}
          DW Offset @fCs16HiS   {0078}
          DW Offset @fCs16HiS   {007a}
          DW Offset @fCs16HiS   {007c}
          DW Offset @fCs16HiS   {007e}
@KERNEL:
          mov cx,SamplesRemain
          OR  cx,cx
          jnz @Ex0
          mov cx,NumSamples
          mov SamplesRemain,cx
  @Ex0:   {Austrittbedingungen}
          mov al,AmRepeating
          OR  al,al
          jnz @Exit
          {Limit Maximum Samples per MixBuffer...}
          cmp cx,MaxMixBufSamples
          jbe @Ex2
          mov cx,MaxMixBufSamples
  @Ex2:   {bei PeakSearch, WaveOfs und WaveEnd faken...}
          cmp search,0
          je  @Ex4
          xor ax,ax
          DB  66h; mov word Ptr WaveOfs,ax
          add ax,cx
          add ax,cx
          add ax,cx
          add ax,cx
          DB  66h; mov word Ptr WaveEnd,ax
  @Ex4:   DB  66h; mov ax,word Ptr WaveEnd
          DB  66h; sub ax,Word Ptr WaveOfs
          mov bx,WaveFormat.Wf.nBlockAlign
          XOR dx,dx
          DIV bx     {ax:=maximaler Samplebuffer}
          OR  ax,ax
          je  @Exit
  @Ex5:   cmp ax,cx
          jb  @Ex3
          mov SamplesRemain,0
          mov BufSamples,cx
          jmp @Kernel1
  @Ex3:   sub cx,ax
          mov BufSamples,ax
          mov SamplesRemain,cx
@Kernel1:
          {32Bit MixBuffer Füllen}
          DB 66h; xor di,di
          DB 66h; xor cx,cx
          mov di,MixBufferOfs
          mov cx,BufSamples
          inc cx
          DB 66h; add cx,cx
          mov es,PlayMemSel
          DB 66h; xor ax,ax
          DB 0F3h,66h,0ABh; {rep stosd}
          {32Bit SurBuffer Füllen}
          DB 66h; xor di,di
          DB 66h; xor cx,cx
          mov di,SurBufferOfs
          mov cx,BufSamples
          inc cx
          DB 66h; add cx,cx
          mov es,PlayMemSel
          DB 66h; xor ax,ax
          DB 0F3h,66h,0ABh; {rep stosd}
       {CX-Anzahl der zu mixenden Kanäle}
          mov cx,Mi.NumTracks
          add cx,NumEffTracks
       {OverSampling...}
          DB 66h; mov ax,word Ptr VolMaster
          DB 26h,66h; mov word Ptr [oVolMaster],ax
       {DS nach GS}
          mov ax,ds
          {mov gs,ax}DB 8Eh,0E8h
@start:
       {Ausgeschaltete Kanäle überspringen}
          mov bx,cx
          mov bl,[bx-1].DisAbled.byte
       {Channels[CX] nach AsmChn...}
          mov si,OFFSET Channels
          mov ax,TYPE tChannel
          mul cx
          sub ax,TYPE tChannel
          add si,ax
          DB 64h; mov [offset @cx],cx
          DB 64h; mov [offset @DI],si
          mov cx,Type tMixChannel
          shr cx,2
          mov di,oAsmChn
          {rep MovsD} DB 0F3h,66h,0A5h
       {CX-NumSamples}
          mov cx,BufSamples
          inc cx
       {PlayMemSel nach DS}
          mov ds,PlayMemSel
       {ESI-SampleOfs}
          DB 66h; mov si,word Ptr tChannel([oAsmChn]).note
       {Leere Kanäle überspringen}
          DB 66h; OR si,si
          jz @endmix
@@0:      DB 66h; cmp si,word Ptr tChannel([oAsmChn]).IEnd
          jbe @@1
          DB 66h; sub si,word Ptr tChannel([oAsmChn]).LLn
          jns @@0
          DB 66h; xor si,si
          jmp @WriteChnEnd
@@1:   {Ausgeschaltete Kanäle überspringen...}
          OR bl,bl{<-DisAbled}
          jnz @VOL0
       {VOL0-Optimierung}
          cmp tChannel([oAsmChn]).AvgVol,0
          jnz @NoVol0
@VOL0:    DB 66h; XOR bx,bx
          mov bx,cx
          DB 66h; mov ax,word Ptr tChannel([oAsmChn]).Plo
          DB 66h; mul bx
          {SHRD eax,edx,10h} DB 066h,00Fh,00ACh,0D0h,010h
          DB 66h; add si,ax
@0:       DB 66h; cmp si,word Ptr tChannel([oAsmChn]).IEnd
          jbe @Vol0_1
          {DB 66h; mov si,word Ptr tChannel([oAsmChn]).IEnd{}
          DB 66h; sub si,word Ptr tChannel([oAsmChn]).LLn
          jns @0
          DB 66h; xor si,si
@Vol0_1:  DB 66h; mov word Ptr tChannel([oAsmChn]).Note,si
          jmp @endmix {}
@NoVol0:
        {DI-DstOfs}
          mov di,word ptr tChannel([oAsmChn]).MixBuf
        {EDX: AsmChn.Plo}
          {movzx edx,ASMChn.Plo} DB 66h,0fh,0b7h,16h; DW tChannel(oAsmChn).Plo
        {AX<-Volumes}
          mov ax,word ptr tChannel([oAsmChn]).lVol
          mov bx,tChannel([oAsmChn]).JmpFlags
          DB 64H; jmp word ptr [Offset @JmpTable+bx]
{*****************************schnelles      KanalMixing********************************************}
{------------------------08 Bit Panning-------------------------------}
@S08:
          {Volumes}
          DB 64h; mov byte Ptr [offset @S08_3],ah {rVol}
          sub al,ah
          add al,al
          DB 64h; mov byte Ptr [offset @S08_4],al {lVol}
          {PLO/REST}
          mov bp,dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S08_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S08_7],ax
@S08GetSample:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,rVol} DB 0B7h; @S08_3: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          {movsx ebx,[bx+((lVol-rVol) SHL 1)SHL 8]} DB 66h,0Fh,0BFh,9Fh; DB 00h; @S08_4: DB 00h
          DB 66h; SHL ax,8
          DB 66h; SHL bx,8
@S08WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],eax}DB 66h,01h,45h,04h
          add di,8
          dec cx
          jz  @S0WriteChn
          add dx,bp
          jnc @S08WriteSample
          DB 66h; inc si
@S08GetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @S08_6: DD 12345678h
          jbe @S08GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @S08_7: DD 12345678h
          jns @S08GetSample2
          JMP @WriteChnSEnd
@S0WriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnS
          DB 66h; inc si
          JMP @WriteChnS
{------------------------08 Bit Middle--------------------------------}
@M08:
          {VOL}
          DB 64h; mov byte Ptr [offset @M08_3],al
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @M08_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M08_5],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@M08GetSample:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,AvgVol} DB 0B7h; @M08_3: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
@M08WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @M0WriteChn
          add ax,dx
          jnc @M08WriteSample
          DB 66h; inc si
@M08GetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @M08_4: DD 12345678h
          jbe @M08GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @M08_5: DD 12345678h
          jns @M08GetSample2
          JMP @WriteChnMiEnd
@M0WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnMi
          DB 66h; inc si
          JMP @WriteChnMi
{------------------------08 Bit Links--------------------------------}
@L08:
          {VOL}
          DB 64h; mov byte Ptr [offset @L08_2],al
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @L08_3],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L08_4],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@L08GetSample:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,lVol} DB 0B7h; @L08_2: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
@L08WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          dec cx
          jz  @L0WriteChn
          add ax,dx
          jnc @L08WriteSample
          DB 66h; inc si
@L08GetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @L08_3: DD 12345678h
          jbe @L08GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @L08_4: DD 12345678h
          jns @L08GetSample2
          JMP @WriteChnLEnd
@L0WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnL
          DB 66h; inc si
          JMP @WriteChnL
{------------------------08 Bit Rechts--------------------------------}
@R08:
          {VOL}
          DB 64h; mov byte Ptr [offset @R08_2],ah
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @R08_3],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R08_4],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@R08GetSample:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,rVol} DB 0B7h; @R08_2: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
@R08WriteSample:
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @R0WriteChn
          add ax,dx
          jnc @R08WriteSample
          DB 66h; inc si
@R08GetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @R08_3: DD 12345678h
          jbe @R08GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @R08_4: DD 12345678h
          jns @R08GetSample2
          JMP @WriteChnREnd
@R0WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnR
          DB 66h; inc si
          JMP @WriteChnR
{********************************langsames   KanalMixing********************************************}
{------------------------08 Bit Panning-------------------------------}
@S081:
          {Volumes}
          DB 64h; mov byte Ptr [offset @S081_3],ah {rVol}
          sub al,ah
          add al,al
          DB 64h; mov byte Ptr [offset @S081_4],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S081_5],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S081_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S081_7],ax
          {PHi}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).PHi
@S08GetSample1:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,rVol} DB 0B7h; @S081_3: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07;
          {movsx ebx,[bx+((lVol-rVol) SHL 1)SHL 8]} DB 66h,0Fh,0BFh,9Fh; DB 00h; @S081_4: DB 00h
          DB 66h; SHL ax,8
          DB 66h; SHL bx,8
          {add [di+4],eax} DB 66h,01h,45h,04h
          {add [di],ebx}   DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S081_5: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @S0WriteChn1
@S08GetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @S081_6: DD 12345678h
          jbe @S08GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @S081_7: DD 12345678h
          jns @S08GetSample12
          jmp @WriteChnSEnd
@S0WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnS
{------------------------08 Bit Middle--------------------------------}
@M081:
          {Volumes}
          DB 64h; mov byte Ptr [offset @M081_3],ah {AvgVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M081_4],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M081_6],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@M08GetSample1:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,avgVol} DB 0B7h; @M081_3: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
          {add [di],ebx}   DB 66h,01h,1Dh
          {add [di+4],ebx} DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M081_4: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @M0WriteChn1
@M08GetSample12:
          DB 66h; CMP SI,bp
          jbe @M08GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @M081_6: DD 12345678h
          jns @M08GetSample12
          JMP @WriteChnMiEnd
@M0WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnMi
{------------------------08 Bit Links--------------------------------}
@L081:
          {Volumes}
          DB 64h; mov byte Ptr [offset @L081_2],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L081_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L081_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@L08GetSample1:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,LVol} DB 0B7h; @L081_2: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
          {add [di],ebx}   DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L081_3: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @L0WriteChn1
@L08GetSample12:
          DB 66h; CMP SI,bp
          jbe @L08GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @L081_5: DD 12345678h
          jns @L08GetSample12
          JMP @WriteChnLEnd
@L0WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnL
{------------------------08 Bit Rechts--------------------------------}
@R081:
          {Volumes}
          DB 64h; mov byte Ptr [offset @R081_2],ah {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R081_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R081_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@R08GetSample1:
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,RVol} DB 0B7h; @R081_2: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; SHL bx,8
          {add [di+4],ebx} DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R081_3: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @R0WriteChn1
@R08GetSample12:
          DB 66h; CMP SI,bp
          jbe @R08GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @R081_5: DD 12345678h
          jns @R08GetSample12
          JMP @WriteChnREnd
@R0WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnR
          (**)
{------------------------08 Bit Panning--OverSampilg--------------------}
@S08O:
          mov word ptr [o_Tmp],cx
          {Volumes}
          DB 64h; mov byte Ptr [offset @S08o_3],ah {rVol}
          sub al,ah
          add al,al
          DB 64h; mov byte Ptr [offset @S08o_4],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S08o_5],dx
          DB 64h,66h; mov word Ptr [offset @S08o_10],dx
          DB 64h,66h; mov word Ptr [offset @S08o_11],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S08o_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S08o_7],ax
          {486-Bug...}
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {OldSamples initialisieren...}
          {mov bl,[esi]} DB 67h,8ah,1eh
          mov bh,tMixChannel([oAsmChn]).lVol
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov word Ptr [oOldSmpL],ax
          {mov bl,[esi]} DB 67h,8ah,1eh
          mov bh,tMixChannel([oAsmChn]).rVol
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov word Ptr [oOldSmpR],ax
@S08OGetSample:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,rVol} DB 0B7h; @S08o_3: DB 00
          add bx,bx
          {movsx eax,word ptr [bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov bp,word Ptr [oOldSmpR]
          DB 66h; mov word Ptr [oOldSmpR],ax
          DB 66h; sub ax,bp
          {imul ecx,eax,PLO} DB 66h,69h,0C8h; @S08o_10: DD 00000000h
          {imul eax,eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; sar cx,8
          DB 66h; shl bp,8
          DB 66h; add ax,bp
          DB 66h; mov word Ptr [oIncrR],ax
          {movsx ebx,[bx+((lVol-rVol) SHL 1)SHL 8]} DB 66h,0Fh,0BFh,9Fh; DB 00h; @S08o_4: DB 00h
          DB 66h; mov ax,word Ptr [oOldSmpL]
          DB 66h; mov word Ptr [oOldSmpL],bx
          DB 66h; sub bx,ax
          {imul ebp,ebx,PLO} DB 66h,69h,0EBh; @S08o_11: DD 00000000h
          {imul ebx,ebx,edx} DB 66h,0Fh,0AFh,0DAh
          DB 66h; sar bp,8
          DB 66h; sar bx,8
          DB 66h; shl ax,8
          DB 66h; add bx,ax
          DB 66h; mov ax,word Ptr [oIncrR]
@S08OWriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],eax}  DB 66h,01h,45h,04h
          add di,8
          dec word ptr [o_Tmp]
          jz  @S08OWriteChn
          DB 66h; add bx,bp
          DB 66h; add ax,cx
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S08o_5: DW 0000h
          jnc @S08OWriteSample
          DB 66h; inc si
@S08OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @S08o_6: DD 12345678h
          jbe @S08OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @S08o_7: DD 12345678h
          jns @S08OGetSample2
          DB 66h; sub bx,bp
          DB 66h; sub ax,cx
          mov cx,word ptr [o_Tmp]
          jmp @WriteChnSEnd
@S08OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnS
          DB 66h; inc si
          jmp @WriteChnS
{------------------------08 Bit Middle--OverSampilg--------------------}
@M08O:
          {Volumes}
          DB 64h; mov byte Ptr [offset @M08o_3],al {AvgVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M08o_4],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @M08o_5],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M08o_6],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @M08o_8],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {OldSamples initialisieren...}
          {mov bl,[esi]} DB 67h,8ah,1eh
          mov bh,tMixChannel([oAsmChn]).LVol
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov word Ptr [oOldSmpL],ax
@M08OGetSample:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,AvgVol} DB 0B7h; @M08o_3: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov bx,word Ptr [oOldSmpL]
          DB 66h; mov word Ptr [oOldSmpL],ax
          DB 66h; sub ax,bx
          {imul ebp,eax,PLO} DB 66h,69h,0E8h; @M08o_8: DD 00000000h
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar bp,8
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
@M08OWriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @M08OWriteChn
          DB 66h; add bx,bp
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M08o_4: DW 0000h
          jnc @M08OWriteSample
          DB 66h; inc si
@M08OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @M08o_5: DD 12345678h
          jbe @M08OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @M08o_6: DD 12345678h
          jns @M08OGetSample2
          DB 66h; sub bx,bp
          jmp @WriteChnMiEnd
@M08OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnMi
          DB 66h; inc si
          jmp @WriteChnMi
{------------------------08 Bit Links--OverSampilg--------------------}
@L08O:
          {Volumes}
          DB 64h; mov byte Ptr [offset @L08o_2],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L08o_3],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @L08o_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L08o_5],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @L08o_7],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {OldSamples initialisieren...}
          {mov bl,[esi]} DB 67h,8ah,1eh
          mov bh,tMixChannel([oAsmChn]).lVol
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov word Ptr [oOldSmpL],ax
@L08OGetSample:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,lVol} DB 0B7h; @L08o_2: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov bx,word Ptr [oOldSmpL]
          DB 66h; mov word Ptr [oOldSmpL],ax
          DB 66h; sub ax,bx
          {imul ebp,eax,PLO} DB 66h,69h,0E8h; @L08o_7: DD 00000000h
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar bp,8
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
@L08OWriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          dec cx
          jz  @L08OWriteChn
          DB 66h; add bx,bp
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L08o_3: DW 0000h
          jnc @L08OWriteSample
          DB 66h; inc si
@L08OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @L08o_4: DD 12345678h
          jbe @L08OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @L08o_5: DD 12345678h
          jns @L08OGetSample2
          DB 66h; sub bx,bp
          jmp @WriteChnLEnd
@L08OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnL
          DB 66h; inc si
          jmp @WriteChnL
{------------------------08 Bit Rechts--OverSampilg--------------------}
@R08O:
          {Volumes}
          DB 64h; mov byte Ptr [offset @R08o_2],ah {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R08o_3],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @R08o_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R08o_5],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @R08o_7],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {OldSamples initialisieren...}
          {mov bl,[esi]} DB 67h,8ah,1eh
          mov bh,tMixChannel([oAsmChn]).rVol
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov word Ptr [oOldSmpR],ax
@R08OGetSample:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,rVol} DB 0B7h; @R08o_2: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          DB 66h; mov bx,word Ptr [oOldSmpR]
          DB 66h; mov word Ptr [oOldSmpR],ax
          DB 66h; sub ax,bx
          {imul ebp,eax,PLO} DB 66h,69h,0E8h; @R08o_7: DD 00000000h
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar bp,8
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
@R08OWriteSample:
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @R08OWriteChn
          DB 66h; add bx,bp
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R08o_3: DW 0000h
          jnc @R08OWriteSample
          DB 66h; inc si
@R08OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @R08o_4: DD 12345678h
          jbe @R08OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @R08o_5: DD 12345678h
          jns @R08OGetSample2
          DB 66h; sub bx,bp
          jmp @WriteChnREnd
@R08OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnR
          DB 66h; inc si
          jmp @WriteChnR
{------------------------08 Bit Panning-------------------------------}
@S08o1:
          mov word ptr [o_Tmp],cx
          {Volumes}
          DB 64h; mov byte Ptr [offset @S08o1_3],ah {rVol}
          DB 64h; mov byte Ptr [offset @S08o1_33],ah {rVol}
          sub al,ah
          add al,al
          DB 64h; mov byte Ptr [offset @S08o1_4],al {lVol}
          DB 64h; mov byte Ptr [offset @S08o1_44],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S08o1_5],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S08o1_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S08o1_7],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
          DB 64h,66h; mov word Ptr [offset @S08O1_0],ax
@S08oGetSample1:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,rVol} DB 0B7h; @S08o1_3: DB 00
          add bx,bx
          {movsx ecx,word ptr [bx]} DB 66h,0Fh,0BFh,0Fh
          {movsx ebp,word ptr [bx+((lVol-rVol) SHL 1)SHL 8]} DB 66h,0Fh,0BFh,0AFh; DB 00h; @S08o1_4: DB 00h
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,rVol} DB 0B7h; @S08o1_33: DB 00
          add bx,bx
          {movsx eax,word ptr [bx]} DB 66h,0Fh,0BFh,07h;
          {movsx ebx,word ptr [bx+((lVol-rVol) SHL 1)SHL 8]} DB 66h,0Fh,0BFh,9Fh; DB 00h; @S08o1_44: DB 00h
          DB 66h; sub bp,bx
          DB 66h; sub cx,ax
          {imul ebp,edx} DB 66h,0Fh,0AFh,0EAh
          {imul ecx,edx} DB 66h,0Fh,0AFh,0CAh
          DB 66h; sar bp,8
          DB 66h; sar cx,8
          DB 66h; shl bx,8
          DB 66h; shl ax,8
          DB 66h; add bx,bp
          DB 66h; add ax,cx
          {add [di+0],ebx}  DB 66h,01h,1Dh
          {add [di+4],eax}  DB 66h,01h,45h,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S08o1_5: DW 0000h
          {adc esi,PHi} DB 66h,81h,0D6h; @S08o1_0: DD 00000000
          dec word ptr [o_Tmp]
          jz  @S08oWriteChn1
@S08oGetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @S08o1_6: DD 12345678h
          jbe @S08oGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @S08o1_7: DD 12345678h
          jns @S08oGetSample12
          mov cx,word ptr [o_Tmp]
          jmp @WriteChnSEnd
@S08oWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnS
{------------------------08 Bit Middle--------------------------------}
@M08o1:  {Volumes}
          DB 64h; mov byte Ptr [offset @M08o1_3],ah {AvgVol}
          DB 64h; mov byte Ptr [offset @M08o1_33],ah {AvgVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M08o1_4],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @M08o1_5],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M08o1_6],ax
          {PHi}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).PHi
@M08oGetSample1:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,avgVol} DB 0B7h; @M08o1_3: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,avgVol} DB 0B7h; @M08o1_33: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; sub ax,bx
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],ebx}DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M08o1_4: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @M08oWriteChn1
@M08oGetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @M08o1_5: DD 12345678h
          jbe @M08oGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @M08o1_6: DD 12345678h
          jns @M08oGetSample12
          JMP @WriteChnMiEnd
@M08oWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnMi
{------------------------08 Bit Links--------------------------------}
@L08o1:
          {Volumes}
          DB 64h; mov byte Ptr [offset @L08o1_2],al {lVol}
          DB 64h; mov byte Ptr [offset @L08o1_22],al {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L08o1_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @L08o1_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L08o1_5],ax
          {PHi}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).PHi
@L08oGetSample1:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,lVol} DB 0B7h; @L08o1_2: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,lVol} DB 0B7h; @L08o1_22: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; sub ax,bx
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L08o1_3: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @L08oWriteChn1
@L08oGetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @L08o1_4: DD 12345678h
          jbe @L08oGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @L08o1_5: DD 12345678h
          jns @L08oGetSample12
          JMP @WriteChnLEnd
@L08oWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnL
{------------------------08 Bit Rechts--------------------------------}
@R08o1:
          {Volumes}
          DB 64h; mov byte Ptr [offset @R08o1_2],ah {rVol}
          DB 64h; mov byte Ptr [offset @R08o1_22],ah {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R08o1_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @R08o1_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R08o1_5],ax
          {PHi}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).PHi
@R08oGetSample1:
          {mov bl,[esi+1]} DB 67h,8ah,5eh,01h
          {mov bh,rVol} DB 0B7h; @R08o1_2: DB 00
          add bx,bx
          {movsx eax,[bx]} DB 66h,0Fh,0BFh,07h
          {mov bl,[esi]} DB 67h,8ah,1eh
          {mov bh,rVol} DB 0B7h; @R08o1_22: DB 00
          add bx,bx
          {movsx ebx,[bx]} DB 66h,0Fh,0BFh,1Fh;
          DB 66h; sub ax,bx
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; shl bx,8
          DB 66h; add bx,ax
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R08o1_3: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @R08oWriteChn1
@R08oGetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @R08o1_4: DD 12345678h
          jbe @R08oGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @R08o1_5: DD 12345678h
          jns @R08oGetSample12
          JMP @WriteChnREnd
@R08oWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnR
{************ 16 BIT Mixing *************************************************************************}
{------------------------16 Bit Panning-------------------------------}
@S16:
         {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S16_3],bx {rVol}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S16_4],bx {lVol}
          {PLO/REST}
          mov bp,dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S16_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S16_7],ax
@S16GetSample:
          {movsx eax,[esi+esi]} DB 67h,66h,0fh,0bfh,04h,36h
          {imul ebx,eax,lVol} DB 66h,69h,0D8h; @S16_4: DD 00000000h
          {imul eax,eax,rVol} DB 66h,69h,0C0h; @S16_3: DD 00000000h
@S16WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],eax}DB 66h,01h,45h,04h
          add di,8
          dec cx
          jz  @S16WriteChn
          add dx,bp
          jnc @S16WriteSample
          DB 66h; inc si
@S16GetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @S16_6: DD 12345678h
          jbe @S16GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @S16_7: DD 12345678h
          jns @S16GetSample2
          JMP @WriteChnSEnd16
@S16WriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnS16
          DB 66h; inc si
          JMP @WriteChnS16
{------------------------16 Bit Middle--------------------------------}
@M16:
          {VOL}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @M16_3],bx {AvgVol}
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M16_5],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@M16GetSample:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,AvgVol} DB 66h,69h,0DBh; @M16_3: DD 00000000h
@M16WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],ebx}DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @M16WriteChn
          add ax,dx
          jnc @M16WriteSample
          DB 66h; inc si
@M16GetSample2:
          DB 66h; cmp si,bp
          jbe @M16GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @M16_5: DD 12345678h
          jns @M16GetSample2
          JMP @WriteChnMiEnd16
@M16WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnMi16
          DB 66h; inc si
          JMP @WriteChnMi16
{------------------------16 Bit Links--------------------------------}
@L16:
          {VOL}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @L16_2],bx {lVol}
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L16_4],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@L16GetSample:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,lVol} DB 66h,69h,0DBh; @L16_2: DD 00000000h
@L16WriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          dec cx
          jz  @L16WriteChn
          add ax,dx
          jnc @L16WriteSample
          DB 66h; inc si
@L16GetSample2:
          DB 66h; cmp si,bp
          jbe @L16GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @L16_4: DD 12345678h
          jns @L16GetSample2
          JMP @WriteChnLEnd16
@L16WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnL16
          DB 66h; inc si
          JMP @WriteChnL16
{------------------------16 Bit Rechts--------------------------------}
@R16:
          {VOL}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @R16_2],bx {rVol}
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R16_4],ax
          mov ax,word ptr tMixChannel([oAsmChn]).Rest
@R16GetSample:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,rVol} DB 66h,69h,0DBh; @R16_2: DD 00000000h
@R16WriteSample:
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @R16WriteChn
          add ax,dx
          jnc @R16WriteSample
          DB 66h; inc si
@R16GetSample2:
          DB 66h; cmp si,bp
          jbe @R16GetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @R16_4: DD 12345678h
          jns @R16GetSample2
          JMP @WriteChnREnd16
@R16WriteChn:
          add ax,dx
          mov word ptr tMixChannel([oAsmChn]).Rest,ax
          jnc @WriteChnR16
          DB 66h; inc si
          JMP @WriteChnR16
{********************************langsames   KanalMixing********************************************}
{------------------------16 Bit Panning-------------------------------}
@S161:
         {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S161_3],bx {rVol}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S161_4],bx {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S161_5],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S161_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S161_7],ax
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).pHi
@S16GetSample1:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul eax,ebx,lVol}   DB 66h,69h,0C3h; @S161_3: DD 00000000h
          {imul ebx,ebx,rVol}   DB 66h,69h,0DBh; @S161_4: DD 00000000h
          {add [di+4],eax}      DB 66h,01h,45h,04h
          {add [di],ebx}        DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S161_5: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @S16WriteChn1
@S16GetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @S161_6: DD 12345678h
          jbe @S16GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @S161_7: DD 12345678h
          jns @S16GetSample12
          jmp @WriteChnSEnd16
@S16WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnS16
{------------------------16 Bit Middle--------------------------------}
@M161:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @M161_3],bx {AvgVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M161_4],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M161_6],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@M16GetSample1:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,AvgVol}     DB 66h,69h,0DBh; @M161_3: DD 00000000h
          {add [di],ebx}        DB 66h,01h,1Dh
          {add [di+4],ebx}      DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M161_4: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @M16WriteChn1
@M16GetSample12:
          DB 66h; CMP SI,BP
          jbe @M16GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @M161_6: DD 12345678h
          jns @M16GetSample12
          JMP @WriteChnMiEnd16
@M16WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnMi16
{------------------------16 Bit Links--------------------------------}
@L161:
          {Volumes}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @L161_2],bx {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L161_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L161_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@L16GetSample1:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,rVol}       DB 66h,69h,0DBh; @L161_2: DD 00000000h
          {add [di],ebx}        DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L161_3: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @L16WriteChn1
@L16GetSample12:
          DB 66h; CMP SI,BP
          jbe @L16GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @L161_5: DD 12345678h
          jns @L16GetSample12
          JMP @WriteChnLEnd16
@L16WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnL16
{------------------------16 Bit Rechts--------------------------------}
@R161:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @R161_2],bx {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R161_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R161_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
@R16GetSample1:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {imul ebx,rVol}       DB 66h,69h,0DBh; @R161_2: DD 00000000h
          {add [di+4],ebx}      DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R161_3: DW 0000h
          DB 66h; adc si,ax
          dec cx
          jz  @R16WriteChn1
@R16GetSample12:
          DB 66h; CMP SI,BP
          jbe @R16GetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @R161_5: DD 12345678h
          jns @R16GetSample12
          JMP @WriteChnREnd16
@R16WriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnR16
{------------------------16 Bit Panning--OverSampilg--------------------}
@S16O:
          mov word ptr [o_Tmp],cx
          {Volumes}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {movsx EAX,ah} DB 66h,0Fh,0BEh,0C4h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          {IMUL EAX,VolMaster}  DB 66h,0Fh,0AFh,06h; DW oVolMaster
          DB 66h; sar bx,9
          DB 66h; sar ax,9
          DB 64h,66h; mov [offset @S16o_4],bx {lVol}
          DB 64h,66h; mov [offset @S16o_3],ax {rVol}
          DB 64h,66h; mov [offset @S16o_41],bx {lVol}
          DB 64h,66h; mov [offset @S16o_31],ax {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S16o_5],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S16o_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S16o_7],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @S16o_10],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
@S16OGetSample:
          {movsx ebx,[esi+esi]} DB 67h,66h,0fh,0bfh,1Ch,36h
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          DB 66h; sub ax,bx
          DB 66h; sar ax,1 {}
          {imul ecx,eax,PLO}DB 66h,69h,0C8h; @S16o_10: DD 00000000h
          {imul eax,eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,15
          DB 66h; sar cx,15
          DB 66h; add bx,ax
          {imul eax,ebx,rVol} DB 66h,69h,0C3h; @S16O_3:  DD 00000000h
          {imul ebx,ebx,lVol} DB 66h,69h,0DBh; @S16O_4:  DD 00000000h
          {imul ebp,ecx,lVol} DB 66h,69h,0E9h; @S16O_41: DD 00000000h
          {imul ecx,ecx,rVol} DB 66h,69h,0C9h; @S16O_31: DD 00000000h
@S16OWriteSample:
          {add [di+00],ebx}  DB 66h,01h,1Dh
          {add [di+04],eax}  DB 66h,01h,45h,04h
          add di,8
          dec word ptr [o_Tmp]
          jz  @S16OWriteChn
          DB 66h; add bx,bp
          DB 66h; add ax,cx
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S16o_5: DW 0000h
          jnc @S16OWriteSample
          DB 66h; inc si
@S16OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @S16o_6: DD 12345678h
          jbe @S16OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @S16o_7: DD 12345678h
          jns @S16OGetSample2
          DB 66h; sub bx,bp
          DB 66h; sub ax,cx
          mov cx,word ptr [o_Tmp]
          jmp @WriteChnSEnd16
@S16OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnS16
          DB 66h; inc si
          jmp @WriteChnS16
{------------------------16 Bit Middle--OverSampilg--------------------}
@M16O:
          {Volumes}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @M16o_3],bx {AvgVol}
          {[oOldSmpL] initialisieren}
          {movsx ebp,[esi+esi]} DB 67h,66h,0fh,0bfh,2Ch,36h
          {imul ebp,ebx} DB 66h,0Fh,0Afh,0EBh
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M16o_4],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @M16o_5],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M16o_6],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @M16o_8],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
@M16OGetSample:
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          {imul eax,AvgVol} DB 66h,69h,0C0h; @M16o_3: DD 00000000h
          DB 66h; mov bx,bp
          DB 66h; mov bp,ax
          DB 66h; sub ax,bx
          DB 66h; sar ax,8
          DB 66h; mov word Ptr [o_Tmp],ax
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; add bx,ax
          {imul eax,dword ptr [o_Tmp],PLO}DB 66h,69h,006h; DW o_Tmp; @M16o_8: DD 00000000h
          DB 66h; sar ax,8
@M16OWriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          {add [di+4],ebx}DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @M16OWriteChn
          DB 66h; add bx,ax
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M16o_4: DW 0000h
          jnc @M16OWriteSample
          DB 66h; inc si
@M16OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @M16o_5: DD 12345678h
          jbe @M16OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @M16o_6: DD 12345678h
          jns @M16OGetSample2
          DB 66h; sub bx,ax
          jmp @WriteChnMiEnd16
@M16OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnMi16
          DB 66h; inc si
          jmp @WriteChnMi16
{------------------------16 Bit Links--OverSampilg--------------------}
@L16O:
          {Volumes}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @L16o_2],bx {lVol}
          {[oOldSmpL] initialisieren}
          {movsx ebp,[esi+esi]} DB 67h,66h,0fh,0bfh,2Ch,36h
          {imul ebp,ebx} DB 66h,0Fh,0Afh,0EBh
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L16o_3],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @L16o_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L16o_5],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @L16o_7],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
@L16OGetSample:
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          {imul eax,LVol} DB 66h,69h,0C0h; @L16o_2: DD 00000000h
          DB 66h; mov bx,bp
          DB 66h; mov bp,ax
          DB 66h; sub ax,bx
          DB 66h; sar ax,8
          DB 66h; mov word Ptr [o_Tmp],ax
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; add bx,ax
          {imul eax,dword ptr [o_Tmp],PLO}DB 66h,69h,006h; DW o_Tmp; @L16o_7: DD 00000000h
          DB 66h; sar ax,8
@L16OWriteSample:
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          dec cx
          jz  @L16OWriteChn
          DB 66h; add bx,ax
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L16o_3: DW 0000h
          jnc @L16OWriteSample
          DB 66h; inc si
@L16OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @L16o_4: DD 12345678h
          jbe @L16OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh;  @L16o_5: DD 12345678h
          jns @L16OGetSample2
          DB 66h; sub bx,ax
          jmp @WriteChnLEnd16
@L16OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnL16
          DB 66h; inc si
          jmp @WriteChnL16
{------------------------16 Bit Rechts--OverSampilg--------------------}
@R16O:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @R16o_2],bx {rVol}
          {[oOldSmpR] initialisieren}
          {movsx ebp,[esi+esi]} DB 67h,66h,0fh,0bfh,2Ch,36h
          {imul ebp,ebx} DB 66h,0Fh,0Afh,0EBh
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R16o_3],dx
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @R16o_4],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R16o_5],ax
          {486-Bug...}
          DB 64h,66h; mov word Ptr [offset @R16o_7],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
@R16OGetSample:
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          {imul eax,RVol} DB 66h,69h,0C0h; @R16o_2: DD 00000000h
          DB 66h; mov bx,bp
          DB 66h; mov bp,ax
          DB 66h; sub ax,bx
          DB 66h; sar ax,8
          DB 66h; mov word Ptr [o_Tmp],ax
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,8
          DB 66h; add bx,ax
          {imul eax,dword ptr [o_Tmp],PLO}DB 66h,69h,006h; DW o_Tmp; @R16o_7: DD 00000000h
          DB 66h; sar ax,8
@R16OWriteSample:
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          dec cx
          jz  @R16OWriteChn
          DB 66h; add bx,ax
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R16o_3: DW 0000h
          jnc @R16OWriteSample
          DB 66h; inc si
@R16OGetSample2:
          {CMP ESI,IEnd}DB 66h, 81h, 0FEh; @R16o_4: DD 12345678h
          jbe @R16OGetSample
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @R16o_5: DD 12345678h
          jns @R16OGetSample2
          DB 66h; sub bx,ax
          jmp @WriteChnREnd16
@R16OWriteChn:
          add dx,tMixChannel([oAsmChn]).Plo
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jnc @WriteChnR16
          DB 66h; inc si
          jmp @WriteChnR16
{------------------------16 Bit Panning-------------------------------}
@S16O1:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S16O1_3],bx {rVol}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @S16O1_4],bx {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @S16O1_5],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 64h,66h; mov word Ptr [offset @S16O1_6],ax
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @S16O1_7],ax
          {PHi}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).PHi
@S16OGetSample1:
          {movsx ebx,[esi+esi]}   DB 67h,66h,0fh,0bfh,1Ch,36h
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          DB 66h; sub ax,bx
          DB 66h; sar ax,1 {}
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,15
          DB 66h; add bx,ax
          {imul eax,ebx,rVol} DB 66h,69h,0C3h; @S16O1_3: DD 00000000h
          {imul ebx,ebx,lVol} DB 66h,69h,0DBh; @S16O1_4: DD 00000000h
          {add [di+4],eax}  DB 66h,01h,45h,04h
          {add [di],ebx}  DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @S16O1_5: DW 0000h
          DB 66h; adc si,bp
          dec cx
          jz  @S16OWriteChn1
@S16OGetSample12:
          {CMP ESI,IEnd}DB 66h, 81h,0FEh; @S16O1_6: DD 12345678h
          jbe @S16OGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @S16O1_7: DD 12345678h
          jns @S16OGetSample12
          jmp @WriteChnSEnd16
@S16OWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnS16
{------------------------16 Bit Middle--------------------------------}
@M16O1:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @M16O1_3],bx {AvgVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @M16O1_4],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @M16O1_6],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
          DB 64h,66h; mov word Ptr [offset @M16O1_0],ax
@M16OGetSample1:
          {movsx ebx,[esi+esi]}   DB 67h,66h,0fh,0bfh,1Ch,36h
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          DB 66h; sub ax,bx
          DB 66h; sar ax,1 {}
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,15
          DB 66h; add bx,ax
          {imul ebx,AvgVol} DB 66h,69h,0DBh; @M16O1_3: DD 00000000h
          {add [di],ebx}    DB 66h,01h,1Dh
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @M16O1_4: DW 0000h
          {adc esi,PHi} DB 66h,81h,0D6h; @M16o1_0: DD 00000000
          dec cx
          jz  @M16OWriteChn1
@M16OGetSample12:
          DB 66h; CMP SI,BP
          jbe @M16OGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @M16O1_6: DD 12345678h
          jns @M16OGetSample12
          JMP @WriteChnMiEnd16
@M16OWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnMi16
{------------------------16 Bit Links--------------------------------}
@L16o1:
          {Volumes}
          {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @L16o1_2],bx {lVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @L16o1_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @L16o1_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
          DB 64h,66h; mov word Ptr [offset @L16O1_0],ax
@L16oGetSample1:
          {movsx ebx,[esi+esi]}   DB 67h,66h,0fh,0bfh,1Ch,36h
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          DB 66h; sub ax,bx
          DB 66h; sar ax,1 {}
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,15
          DB 66h; add bx,ax
          {imul ebx,lVol} DB 66h,69h,0DBh; @L16O1_2: DD 00000000h
          {add [di],ebx}    DB 66h,01h,1Dh
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @L16o1_3: DW 0000h
          {adc esi,PHi} DB 66h,81h,0D6h; @L16o1_0: DD 00000000
          dec cx
          jz  @L16oWriteChn1
@L16oGetSample12:
          DB 66h; CMP SI,BP
          jbe @L16oGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @L16o1_5: DD 12345678h
          jns @L16oGetSample12
          JMP @WriteChnLEnd16
@L16oWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnL16
{------------------------16 Bit Rechts--------------------------------}
@R16O1:
          {Volumes}
          {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
          {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
          DB 66h; sar bx,9
          DB 64h,66h; mov [offset @R16O1_2],bx {rVol}
          {PLO/REST}
          DB 64h; mov word Ptr [offset @R16O1_3],dx
          mov dx,word ptr tMixChannel([oAsmChn]).Rest
          {IEnd/LLn}
          DB 66h; mov bp,word Ptr tMixChannel([oAsmChn]).IEnd
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).LLn
          DB 64h,66h; mov word Ptr [offset @R16O1_5],ax
          {PHi}
          DB 66h; mov ax,word Ptr tMixChannel([oAsmChn]).PHi
          DB 64h,66h; mov word Ptr [offset @R16O1_0],ax
@R16OGetSample1:
          {movsx ebx,[esi+esi]}   DB 67h,66h,0fh,0bfh,1Ch,36h
          {movsx eax,[esi+esi+2]} DB 67h,66h,0fh,0bfh,44h,36h,02h
          DB 66h; sub ax,bx
          DB 66h; sar ax,1 {}
          {imul eax,edx} DB 66h,0Fh,0AFh,0C2h
          DB 66h; sar ax,15
          DB 66h; add bx,ax
          {imul ebx,rVol} DB 66h,69h,0DBh; @R16O1_2: DD 00000000h
          {add [di+4],ebx}  DB 66h,01h,5Dh,04h
          add di,8
          {add dx,AsmChn.Plo} DB 81h,0C2h; @R16O1_3: DW 0000h
          {adc esi,PHi} DB 66h,81h,0D6h; @R16o1_0: DD 00000000
          dec cx
          jz  @R16OWriteChn1
@R16OGetSample12:
          DB 66h; CMP SI,BP
          jbe @R16OGetSample1
          {SUB ESI,LLn}DB 66h, 081h, 0EEh; @R16O1_5: DD 12345678h
          jns @R16OGetSample12
          JMP @WriteChnREnd16
@R16OWriteChn1:
          mov word ptr tMixChannel([oAsmChn]).Rest,dx
          jmp @WriteChnR16
{**************************************** 8 Bit Lo Cubic Spline **************************************}
@Cs08LoS:
{**************************************** 8 Bit Hi Cubic Spline *************************************}
@Cs08HiS:
    mov word ptr [o_Tmp],cx
    {Volumes}
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; sar bx,9
    DB 64h,66h; mov [offset @Cs08HiS_r],bx {rVol}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; sar bx,9
    DB 64h,66h; mov [offset @Cs08HiS_l],bx {lVol}
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).Rest
    DB 66h; xor ax,ax
    DB 66h; xor bx,bx
@Cs08HiSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,word Ptr tMixChannel([oAsmChn]).IEnd
    jbe @Cs08HiSNoLoop
    DB 66h; sub si,word Ptr tMixChannel([oAsmChn]).LLn
    jns @Cs08HiSGetSample
    mov cx,word ptr [o_Tmp]
    jmp @WriteChnSEnd           {OneShot -> GoOut! }
@Cs08HiSNoLoop:
    DB 67h,66h,0Fh,0BEh,4Eh,0FFh{movsx ecx,byte ptr[esi-1]}   {ecx=Ym1}
    DB 67h,66h,0Fh,0BEh,1Eh     {movsx ebx,byte ptr[esi]  }   {ebx=Y0 }
    DB 67h,66h,0Fh,0BEh,46h,01h {movsx eax,byte ptr[esi+1]}   {eax=Y1 }
    DB 66h; mov word ptr [o_d],bx
    DB 66h; sub bx,ax           {ebx=Y0-Y1 }
    DB 66h; sub ax,cx           {eax=Y1-Ym1 = c }
    DB 66h; mov word ptr [o_c],ax
    DB 67h,66h,0Fh,0BEh,46h,02h {movsx eax,byte ptr[esi+2]}   {eax=Y2 }
    DB 66h; add ax,bx           {eax=(Y0-Y1)+Y2 }
    DB 66h; add ax,bx           {eax=2*(Y0-Y1)+Y2 }
    DB 66h; sub bx,cx           {ebx=Y0-Y1-Ym1 }
    DB 66h; add ax,bx           {eax=3*(Y0-Y1)+Y2-Ym1 = a }
    DB 66h; add bx,ax           {ebx=4*(Y0-Y1)+Y2-2*Ym1 = a+Y0-Y1-Ym1 }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=a*X }
    DB 66h; add bx,word ptr [o_d] {a+Y0-Y1-Ym1+Yo = -b }
    DB 66h; add ax,(1 SHL (16-1)); DW 0{}
    DB 66h; sar ax,16           {eax=(a*X)>>16 }
    DB 66h; shl word ptr [o_d],8 {d=356*d because of 8Bit->16Bit conversion }
    DB 66h; sub ax,bx           {eax= a*X-(a+Y0-Y1-Ym1)-Yo = a*X+b }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=(a*X+b)*X }
    DB 66h; add ax,(1 SHL (16-1)); DW 0{}
    DB 66h; sar ax,16           {eax=((a*X+b)*X)>>16 = (a*X+b)*X }
    DB 66h; add ax,word ptr [o_c]{eax=(a*X+b)*X+Y1 }
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=((a*X+b)*X+Y1-Ym1)*X }
    DB 66h; add ax,(1 SHL (9-1)); DW 0{}
    DB 66h; sar ax,9            {eax=(((a*X+b)*X+Y1-Ym1)*X)>>9 = ((a*X+b)*X+c)*X }
    DB 66h; add ax,[o_d]        {eax=((a*X+b)*X+c)*X+Y0 ...viola :-)) }
    { Volume }
    {imul ebx,eax,rVol} DB 66h,69h,0D8h; @Cs08HiS_r: DD 00000000h
    {imul eax,eax,lVol} DB 66h,69h,0C0h; @Cs08HiS_l: DD 00000000h
    {increment src pointer }
    add dx,tMixChannel([oAsmChn]).PLo
    DB 66h; adc si,word ptr tMixChannel([oAsmChn]).PHi
    { Output }
    DB 66h; add [di],ax
    DB 66h; add [di+4],bx
    {increment dst pointer }
    add di,8
    { exit condition }
    dec word ptr [o_Tmp]
    jnz @Cs08HiSGetSample
@Cs08HiSGoOut:
    { Store last sample for FadeOut }
    mov word ptr tMixChannel([oAsmChn]).Rest,dx
    jmp @WriteChnS
{****************************************16 Bit Cubic Spline*****************************************}
@Cs16HiS:
    mov word ptr [o_Tmp],cx
    {Volumes}
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; sar bx,9
    DB 64h,66h; mov [offset @Cs16HiS_r],bx {rVol}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; sar bx,9
    DB 64h,66h; mov [offset @Cs16HiS_l],bx {lVol}
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).Rest
    DB 66h; xor ax,ax
    DB 66h; xor bx,bx
@Cs16HiSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,word Ptr tMixChannel([oAsmChn]).IEnd
    jbe @Cs16HiSNoLoop
    DB 66h; sub si,word Ptr tMixChannel([oAsmChn]).LLn
    jns @Cs16HiSGetSample
    mov cx,word ptr [o_Tmp]
    jmp @WriteChnSEnd16         {OneShot -> GoOut! }
@Cs16HiSNoLoop:
    DB 67h,66h,0Fh,0BFh,4Ch,36h,0FEh{movsx ecx,word ptr[esi+esi-2]}   {ecx=Ym1}
    DB 67h,66h,0Fh,0BFh,1Ch,36h     {movsx ebx,word ptr[esi+esi]  }   {ebx=Y0 }
    DB 67h,66h,0Fh,0BFh,44h,36h,02h {movsx eax,word ptr[esi+esi+2]}   {eax=Y1 }
    DB 66h; mov word ptr [o_d],bx
    DB 66h; sub bx,ax           {ebx=Y0-Y1 }
    DB 66h; sub ax,cx           {eax=Y1-Ym1 = c }
    DB 66h; mov word ptr [o_c],ax
    DB 67h,66h,0Fh,0BFh,44h,36h,04h {movsx eax,word ptr[esi+esi+4]}   {eax=Y2 }
    DB 66h; add ax,bx           {eax=(Y0-Y1)+Y2 }
    DB 66h; add ax,bx           {eax=2*(Y0-Y1)+Y2 }
    DB 66h; sub bx,cx           {ebx=Y0-Y1-Ym1 }
    DB 66h; add ax,bx           {eax=3*(Y0-Y1)+Y2-Ym1 = a }
    DB 66h; add bx,ax           {ebx=4*(Y0-Y1)+Y2-2*Ym1 = a+Y0-Y1-Ym1 }
    DB 66h; add ax,(1 SHL (3-1)); DW 0{}
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=a*X }
    DB 66h; add bx,word ptr [o_d] {a+Y0-Y1-Ym1+Yo = -b }
    DB 66h; add ax,(1 SHL (13-1)); DW 0{}
    DB 66h; sar ax,13           {eax=(a*X)>>16 }
    DB 66h; sub ax,bx           {eax= a*X-(a+Y0-Y1-Ym1)-Yo = a*X+b }
    DB 66h; add ax,(1 SHL (3-1)); DW 0{}
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=(a*X+b)*X }
    DB 66h; add ax,(1 SHL (13-1)); DW 0{}
    DB 66h; sar ax,13           {eax=((a*X+b)*X)>>16 = (a*X+b)*X }
    DB 66h; add ax,word ptr [o_c]{eax=(a*X+b)*X+Y1 }
    DB 66h; add ax,(1 SHL (3-1)); DW 0{}
    DB 66h; sar ax,3
    DB 66h,0Fh,0AFh,0C2h        {imul eax,edx}                {eax=((a*X+b)*X+Y1-Ym1)*X }
    DB 66h; add ax,(1 SHL (14-1)); DW 0{}
    DB 66h; sar ax,14           {eax=(((a*X+b)*X+Y1-Ym1)*X)>>9 = ((a*X+b)*X+c)*X }
    DB 66h; add ax,[o_d]        {eax=((a*X+b)*X+c)*X+Y0 ...viola :-)) }
    { Volume }
    {imul ebx,eax,rVol} DB 66h,69h,0D8h; @Cs16HiS_r: DD 00000000h
    {imul eax,eax,lVol} DB 66h,69h,0C0h; @Cs16HiS_l: DD 00000000h
    {increment src pointer }
    add dx,tMixChannel([oAsmChn]).PLo
    DB 66h; adc si,word ptr tMixChannel([oAsmChn]).PHi
    { Output }
    DB 66h; add [di],ax
    DB 66h; add [di+4],bx
    {increment dst pointer }
    add di,8
    { exit condition }
    dec word ptr [o_Tmp]
    jnz @Cs16HiSGetSample
@Cs16HiSGoOut:
    { Store last sample for FadeOut }
    mov word ptr tMixChannel([oAsmChn]).Rest,dx
    jmp @WriteChnS16
{************************************FPU  8 Bit Cubic Spline*****************************************}
@fCs08LoS:
    {Volumes}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {lVol}
    fmul dword ptr [oOneBy4]
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {rVol}
    fmul dword ptr [oOneBy4]
    {Counter}
    mov word ptr [o_Tmp],cx
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).IEnd
    mov bx,tMixChannel([oAsmChn]).Plo
    DB 66h; mov cx,word ptr tMixChannel([oAsmChn]).PHi
    {LastSample's}
    DB 66h; xor ax,ax
    DB 66h; mov word ptr [o_d],ax
    DB 66h; mov word ptr [o_c],ax
@fCs08LoSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,dx                 {MP.LoopEnd }
    jbe @fCs08LoSNoLoop
    DB 66h; sub si,word ptr tMixChannel([oAsmChn]).LLn
    jns @fCs08LoSGetSample
    mov cx,word ptr [o_Tmp]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; mov bx,word ptr [o_c]
    fcompp
    jmp @WriteChnSEnd           {OneShot -> GoOut! }
@fCs08LoSNoLoop:
    DB 67h,0Fh,0BEh,46h,0FFh{movsx ax,byte ptr [esi-1]}; mov [o_c],ax; fild word ptr[o_c]
    DB 67h,0Fh,0BEh,06h     {movsx ax,byte ptr [esi]};   mov [o_c],ax; fild word ptr[o_c]
    DB 67h,0Fh,0BEh,46h,01h {movsx ax,byte ptr [esi+1]}; mov [o_c],ax; fild word ptr[o_c]
    DB 67h,0Fh,0BEh,46h,02h {movsx ax,byte ptr [esi+2]}; mov [o_c],ax;
    fld  st(1)
    fsub st,st(1)
    fild word ptr[o_c]
    fsub st,st(4)
    fadd st,st(1)
    fadd st,st(1)
    fadd st,st(1)
    fadd st(1),st
    fxch st(3)
    fadd st(1),st
    fadd st,st
    fxch st(4)
    fsub st(1),st
    fsubp st(2),st
@fCs08LoSWriteSample:
    fild dword ptr tMixChannel([oAsmChn]).Rest
    fmul dword ptr[oOneBy65536]
    fld st(3)
    fmul st,st(1)
    fsub st,st(2)
    fmul st,st(1)
    fadd st,st(3)
    fmulp st(1),st
    fadd st,st(4)
    { Output }
    fld st(0)
    fmul st,st(7)
    fistp dword ptr [o_d]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; add [di],ax
    fmul st,st(5)
    fistp dword ptr [o_c]
    DB 66h; mov ax,word ptr [o_c]
    DB 66h; add [di+4],ax
    {increment dst pointer }
    add di,8
    dec word ptr [o_tmp]
    jz @fCs08LoSGoOut0
    {increment src pointer}
    add word ptr tMixChannel([oAsmChn]).Rest,bx   {bx=PitchLo }
    jnc @fCs08LoSWriteSample
    DB 66h; inc si
    fcompp
    fcompp
    jmp @fCs08LoSGetSample
@fCs08LoSGoOut0:
    fcompp
    fcompp
    fcompp
    {increment src pointer }
    add word ptr tMixChannel([oAsmChn]).Rest,bx
    DB 66h; adc si,cx
    jmp @WriteChnS
{************************************FPU  8 Bit Cubic Spline*****************************************}
@fCs08HiS:
    {Volumes}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {lVol}
    fmul dword ptr [oOneBy4]
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {rVol}
    fmul dword ptr [oOneBy4]
    {Counter}
    mov word ptr [o_Tmp],cx
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).IEnd
    mov bx,tMixChannel([oAsmChn]).Plo
    DB 66h; mov cx,word ptr tMixChannel([oAsmChn]).PHi
    {LastSample's}
    DB 66h; xor ax,ax
    DB 66h; mov word ptr [o_d],ax
    DB 66h; mov word ptr [o_c],ax
@fCs08HiSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,dx                 {MP.LoopEnd }
    jbe @fCs08HiSNoLoop
    DB 66h; sub si,word ptr tMixChannel([oAsmChn]).LLn
    jns @fCs08HiSGetSample
    mov cx,word ptr [o_Tmp]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; mov bx,word ptr [o_c]
    fcompp
    jmp @WriteChnSEnd           {OneShot -> GoOut! }
@fCs08HiSNoLoop:
    DB 67h,0Fh,0BEh,46h,0FFh{movsx ax,byte ptr [esi-1]}; mov [o_c],ax; fild word ptr[o_c]
    DB 67h,0Fh,0BEh,06h     {movsx ax,byte ptr [esi]}; mov [o_c],ax; fild word ptr[o_c]
    fild dword ptr tMixChannel([oAsmChn]).Rest {->X,Y0,Ym1,rVol,lVol }
    fmul dword ptr[oOneBy65536]
    DB 67h,0Fh,0BEh,46h,01h {movsx ax,byte ptr [esi+1]}; mov [o_c],ax; fild word ptr[o_c]
    fld  st(2)                  {->Y0,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(1)               {->Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    DB 67h,0Fh,0BEh,46h,02h {movsx ax,byte ptr [esi+2]}; mov [o_c],ax; fild word ptr[o_c]
    fsub st,st(5)               {->Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->2*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->3*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st(1),st               {->3*(Y0-Y1)+Y2-Ym1,4*(Y0-Y1)+Y2-Ym1,Y1,X,Y0,Ym1,rVol,lVol }
    fmul st,st(3)               {->a*X,a+Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fsubrp st(1),st             {->a*X-(a+Y0-Y1),Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(4)               {->a*X-(a+Y0-Y1)+Ym1,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(3)               {->a*X-(a+Y0-Y1)+Ym1-Y0,Y1,X,Y0,Ym1,rVol,lVol }
    fmul st,st(2)               {->(a*X+b)*X,Y1,X,Y0,Ym1,rVol,lVol }
    faddp st(1),st              {->(a*X+b)*X+Y1,X,Y0,Ym1,rVol,lVol }
    fsubrp st(3),st             {->X,Y0,(a*X+b)*X+Y1-Ym1,rVol,lVol }
    fmulp st(2),st              {->Y0,((a*X+b)*X+c)*X,rVol,lVol }
    fadd  st(1),st              {->Y0,((a*X+b)*X+c)*X+Y0,rVol,lVol }
    faddp st(1),st              {->((a*X+b)*X+c)*X+2*Y0,rVol,lVol }
    {increment src pointer }
    add word ptr tMixChannel([oAsmChn]).Rest,bx   {bx=PitchLo }
    DB 66h; adc si,cx           {ecx=PitchHi }
    { Output }
    fld st(0)                   {->Y,Y,rVol,lVol }
    fmul st,st(3)               {->Y*lVol,Y,rVol,lVol }
    fistp dword ptr [o_d]       {->Y,rVol,lVol }
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; add [di],ax
    fmul st,st(1)               {->Y*rVol,rVol,lVol }
    fistp dword ptr [o_c]       {->rVol,lVol }
    DB 66h; mov ax,word ptr [o_c]
    DB 66h; add [di+4],ax
    {increment dst pointer }
    add di,8
    dec word ptr [o_tmp]
    jnz @fCs08HiSGetSample
    fcompp
    jmp @WriteChnS
{************************************FPU 16 Bit Cubic Spline*****************************************}
@fCs16LoS:
    {Volumes}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {lVol}
    fmul dword ptr [oOneBy1024]
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {rVol}
    fmul dword ptr [oOneBy1024]
    {Counter}
    mov word ptr [o_Tmp],cx
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).IEnd
    mov bx,tMixChannel([oAsmChn]).Plo
    DB 66h; mov cx,word ptr tMixChannel([oAsmChn]).PHi
    {LastSample's}
    DB 66h; xor ax,ax
    DB 66h; mov word ptr [o_d],ax
    DB 66h; mov word ptr [o_c],ax
@fCs16LoSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,dx                 {MP.LoopEnd }
    jbe @fCs16LoSNoLoop
    DB 66h; sub si,word ptr tMixChannel([oAsmChn]).LLn
    jns @fCs16LoSGetSample
    mov cx,word ptr [o_Tmp]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; mov bx,word ptr [o_c]
    fcompp
    jmp @WriteChnSEnd           {OneShot -> GoOut! }
@fCs16LoSNoLoop:
    DB 67h,0DFh,44h,36h,0FEh    {fild word ptr [esi+esi-2]   {->Ym1,rVol,lVol }
    DB 67h,0DFh,04h,36h         {fild word ptr [esi+esi]     {->Y0,Ym1,rVol,lVol }
    DB 67h,0DFh,44h,36h,02h     {fild word ptr [esi+esi+2]   {->Y1,X,Y0,Ym1,rVol,lVol }
    fld  st(1)
    fsub st,st(1)
    DB 67h,0DFh,44h,36h,04h     {fild word ptr [esi+esi+4]   {->Y2,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(4)
    fadd st,st(1)
    fadd st,st(1)
    fadd st,st(1)
    fadd st(1),st
    fxch st(3)
    fadd st(1),st
    fadd st,st
    fxch st(4)
    fsub st(1),st
    fsubp st(2),st
@fCs16LoSWriteSample:
    fild dword ptr tMixChannel([oAsmChn]).Rest
    fmul dword ptr[oOneBy65536]
    fld st(3)
    fmul st,st(1)
    fsub st,st(2)
    fmul st,st(1)
    fadd st,st(3)
    fmulp st(1),st
    fadd st,st(4)
    { Output }
    fld st(0)
    fmul st,st(7)
    fistp dword ptr [o_d]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; add [di],ax
    fmul st,st(5)
    fistp dword ptr [o_c]
    DB 66h; mov ax,word ptr [o_c]
    DB 66h; add [di+4],ax
    {increment dst pointer }
    add di,8
    dec word ptr [o_tmp]
    jz @fCs16LoSGoOut0
    {increment src pointer}
    add word ptr tMixChannel([oAsmChn]).Rest,bx   {bx=PitchLo }
    jnc @fCs16LoSWriteSample
    DB 66h; inc si
    fcompp
    fcompp
    jmp @fCs16LoSGetSample
@fCs16LoSGoOut0:
    fcompp
    fcompp
    fcompp
    {increment src pointer }
    add word ptr tMixChannel([oAsmChn]).Rest,bx
    DB 66h; adc si,cx
    jmp @WriteChnS
{************************************FPU 16 Bit Cubic Spline*****************************************}
@fCs16HiS:
    {Volumes}
    {movsx EBX,al} DB 66h,0Fh,0BEh,0D8h
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {lVol}
    fmul dword ptr [oOneBy1024]
    {movsx EBX,ah} DB 66h,0Fh,0BEh,0DCh
    {IMUL EBX,VolMaster}  DB 66h,0Fh,0AFh,1Eh; DW oVolMaster
    DB 66h; mov word ptr [o_tmp],bx
    fild dword ptr [o_tmp]          {rVol}
    fmul dword ptr [oOneBy1024]
    {Counter}
    mov word ptr [o_Tmp],cx
    DB 66h; mov dx,word ptr tMixChannel([oAsmChn]).IEnd
    mov bx,tMixChannel([oAsmChn]).Plo
    DB 66h; mov cx,word ptr tMixChannel([oAsmChn]).PHi
    {LastSample's}
    DB 66h; xor ax,ax
    DB 66h; mov word ptr [o_d],ax
    DB 66h; mov word ptr [o_c],ax
@fCs16HiSGetSample:
    { check for sample loops... }
    DB 66h; cmp si,dx                 {MP.LoopEnd }
    jbe @fCs16HiSNoLoop
    DB 66h; sub si,word ptr tMixChannel([oAsmChn]).LLn
    jns @fCs16HiSGetSample
    mov cx,word ptr [o_Tmp]
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; mov bx,word ptr [o_c]
    fcompp
    jmp @WriteChnSEnd16         {OneShot -> GoOut! }
@fCs16HiSNoLoop:
    DB 67h,0DFh,44h,36h,0FEh    {fild word ptr [esi+esi-2]   {->Ym1,rVol,lVol }
    DB 67h,0DFh,04h,36h         {fild word ptr [esi+esi]     {->Y0,Ym1,rVol,lVol }
    fild dword ptr tMixChannel([oAsmChn]).Rest {->X,Y0,Ym1,rVol,lVol }
    fmul dword ptr[oOneBy65536]
    DB 67h,0DFh,44h,36h,02h     {fild word ptr [esi+esi+2]   {->Y1,X,Y0,Ym1,rVol,lVol }
    fld  st(2)                  {->Y0,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(1)               {->Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    DB 67h,0DFh,44h,36h,04h     {fild word ptr [esi+esi+4]   {->Y2,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(5)               {->Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->2*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(1)               {->3*(Y0-Y1)+Y2-Ym1,Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fadd st(1),st               {->3*(Y0-Y1)+Y2-Ym1,4*(Y0-Y1)+Y2-Ym1,Y1,X,Y0,Ym1,rVol,lVol }
    fmul st,st(3)               {->a*X,a+Y0-Y1,Y1,X,Y0,Ym1,rVol,lVol }
    fsubrp st(1),st             {->a*X-(a+Y0-Y1),Y1,X,Y0,Ym1,rVol,lVol }
    fadd st,st(4)               {->a*X-(a+Y0-Y1)+Ym1,Y1,X,Y0,Ym1,rVol,lVol }
    fsub st,st(3)               {->a*X-(a+Y0-Y1)+Ym1-Y0,Y1,X,Y0,Ym1,rVol,lVol }
    fmul st,st(2)               {->(a*X+b)*X,Y1,X,Y0,Ym1,rVol,lVol }
    faddp st(1),st              {->(a*X+b)*X+Y1,X,Y0,Ym1,rVol,lVol }
    fsubrp st(3),st             {->X,Y0,(a*X+b)*X+Y1-Ym1,rVol,lVol }
    fmulp st(2),st              {->Y0,((a*X+b)*X+c)*X,rVol,lVol }
    fadd  st(1),st              {->Y0,((a*X+b)*X+c)*X+Y0,rVol,lVol }
    faddp st(1),st              {->((a*X+b)*X+c)*X+2*Y0,rVol,lVol }
    {increment src pointer }
    add word ptr tMixChannel([oAsmChn]).Rest,bx   {bx=PitchLo }
    DB 66h; adc si,cx           {ecx=PitchHi }
    { Output }
    fld st(0)                   {->Y,Y,rVol,lVol }
    fmul st,st(3)               {->Y*lVol,Y,rVol,lVol }
    fistp dword ptr [o_d]       {->Y,rVol,lVol }
    DB 66h; mov ax,word ptr [o_d]
    DB 66h; add [di],ax
    fmul st,st(1)               {->Y*rVol,rVol,lVol }
    fistp dword ptr [o_c]       {->rVol,lVol }
    DB 66h; mov ax,word ptr [o_c]
    DB 66h; add [di+4],ax
    {increment dst pointer }
    add di,8
    dec word ptr [o_tmp]
    jnz @fCs16HiSGetSample
    fcompp
    jmp @WriteChnS16
{********************************************KanalGeMixed********************************************}
@WriteChnREnd:
@WriteChnREnd16:
          DB 66h; mov ax,bx
          DB 66h; xor bx,bx
          jmp @WriteChnSEnd16
@WriteChnLEnd:
@WriteChnLEnd16:
          DB 66h; xor ax,ax
          jmp @WriteChnSEnd16
@WriteChnMiEnd:
@WriteChnMiEnd16:
          DB 66h; mov ax,bx
          jmp @WriteChnSEnd16
@WriteChnSEnd:
@WriteChnSEnd16:
          {add [di],ebx}       DB 66h,01h,1Dh
          {add [di+4],eax}     DB 66h,01h,45h,04h
          add di,8
          dec cx
          jnz @WriteChnSEnd16
          DB 66h; xor si,si
          jmp @WriteChnEnd
@WriteChnR:
@WriteChnR16:
@WriteChnL:
@WriteChnL16:
@WriteChnMi:
@WriteChnMi16:
@WriteChnS:
@WriteChnS16:
@WriteChn:
          mov dx,word Ptr tMixChannel([oAsmChn]).Plo
          sub word Ptr tMixChannel([oAsmChn]).Rest,dx
          DB 66h; sbb si,word Ptr tMixChannel([oAsmChn]).pHi
@WriteChnEnd:
          DB 66h; mov word Ptr tMixChannel([oAsmChn]).note,si
@EndMix:
       {ASMChn nach Channels.CH[CX]}
          DB 64h; mov di,[offset @DI]
          mov cx,5 { 5 longs=Note...Rest}
          mov si,oAsmChn
          DB 64h; mov ax,[offset @DS]
          mov es,ax
          {rep MovsD} DB 0F3h,66h,0A5h
          DB 64h; mov cx,[offset @CX]
          mov ds,ax
          mov es,PlayMemSel
          dec cx
          jnz @start
{32Bit Buffer aussliden}
@SlideOut:
          DB 66h; xor di,di
          DB 66h; xor cx,cx
          mov di,MixBufferOfs
          mov cx,NumSamples
          DB 26h,66h; mov ax,[di]
          DB 26h,66h; mov bx,[di+4]
          DB 66h; sub word ptr LFadeOut,ax
          DB 66h; sub word ptr RFadeOut,bx
          {Left FadeOut}
          DB 66h; mov ax,word ptr LFadeOut
          {CDQ} DB 66h,99h
          DB 66h; idiv cx
          DB 66h; mov si,ax
          {Right FadeOut}
          DB 66h; mov ax,word ptr RFadeOut
          {CDQ} DB 66h,99h
          DB 66h; idiv cx
          DB 66h; mov dx,ax
          {FadeOut}
          DB 66h; mov ax,word ptr LFadeOut
          DB 66h; mov bx,word ptr RFadeOut
          mov cx,bufsamples
     @FadeLoop:
          DB 26h,66h; add [di],ax
          DB 26h,66h; add [di+4],bx
          DB 66h; sub ax,si
          DB 66h; sub bx,dx
          add di,8
          dec cx
          jnz @FadeLoop
          DB 26h,66h; add ax,word ptr [di]
          DB 26h,66h; add bx,word ptr [di+4]
          DB 66h; mov word ptr LFadeOut,ax
          DB 66h; mov word ptr RFadeOut,bx
{32Bit SurroundBuffer in MixBuffer mixen...}
@Surround:
          cmp UseSurround,0
          je @MixOut
          {FadeOut Surround Buffer}
          DB 66h; xor di,di
          DB 66h; xor cx,cx
          mov di,SurBufferOfs
          mov cx,NumSamples
          DB 26h,66h; mov ax,[di]
          DB 26h,66h; mov bx,[di+4]
          DB 66h; sub word ptr LSurFadeOut,ax
          DB 66h; sub word ptr RSurFadeOut,bx
          {Left FadeOut}
          DB 66h; mov ax,word ptr LSurFadeOut
          {CDQ} DB 66h,99h
          DB 66h; idiv cx
          DB 66h; mov si,ax
          {Right FadeOut}
          DB 66h; mov ax,word ptr RSurFadeOut
          {CDQ} DB 66h,99h
          DB 66h; idiv cx
          DB 66h; mov dx,ax
          {FadeOut}
          DB 66h; mov ax,word ptr LSurFadeOut
          DB 66h; mov bx,word ptr RSurFadeOut
          mov cx,bufsamples
     @SurFadeLoop:
          DB 26h,66h; add [di],ax
          DB 26h,66h; add [di+4],bx
          DB 66h; sub ax,si
          DB 66h; sub bx,dx
          add di,8
          dec cx
          jnz @SurFadeLoop
          DB 26h,66h; add ax,word ptr [di]
          DB 26h,66h; add bx,word ptr [di+4]
          DB 66h; mov word ptr LSurFadeOut,ax
          DB 66h; mov word ptr RSurFadeOut,bx
          {Surround...}
          mov bx,0001h
          DB 66h; SHL bx,16
          DB 66h; mov dx,bx
          mov bx,BufEnd    {EBX->SurBufDst (store Current Sample there)}
          mov dx,BufStart  {EDX->SurBufSrc (get AddVal from there)     }
          mov si,SurBufferOfs
          mov di,MixBufferOfs
          mov cx,BufSamples
@SurrLoop:
          DB 26h,66h; mov ax,[si]
          DB 26h,67h,66h,89h,43h,04h {mov es:[ebx+4],eax}
          DB 26h,67h,66h,03h,02h     {add eax,es:[edx]}
          DB 26h,66h; add [di],ax
          DB 26h,66h; mov ax,[si+4]
          DB 26h,67h,66h,89h,03h     {mov es:[ebx],eax}
          add si,8
          DB 26h,67h,66h,03h,42h,04h {add eax,es:[edx+4]}
          add bx,8
          DB 26h,66h; add [di+4],ax
          add dx,8
          AND bx,SurrBufferLen-1
          add di,8
          AND dx,SurrBufferLen-1
          loop @SurrLoop
          {store pointers}
          mov BufEnd,bx
          mov BufStart,dx
{******************************  32 Bit in Zielbuffer mixen  ******************************}
@MixOut:
          mov cx,BufSamples
          DB 66h; mov dx,word Ptr WaveOfs
          mov si,MixBufferOfs
          cmp SoundSettings.preamp,$7f
          jna @1Q
          cmp SoundSettings.bits,8
          je  @2Qm8
          cmp SoundSettings.stereo,1
          je @2Q16m
{ --------------------------------- 16 Bit stereo --------------------------------- }
@2Q16S:   add cx,cx
          cmp search,0
          jne @2Q16c
          mov ds,PlayMemSel
          DB 66h; mov di, 32767; DW $0000
          DB 66h; mov bx,-32768; DW $FFFF
@2Qm16S:  DB 66h; mov ax,[si];  DB 66h; sar ax,8;   add si,4
          DB 66h; cmp ax,bx;    jl @2Qm16SL
          DB 66h; cmp ax,di;    jg @2Qm16SG
          {mov [edx],ax}        DB 67h,89h,02h;     DB 66h; add dx,2;   dec cx; jnz @2Qm16s;   jmp @MixEnd
@2Qm16SL: {mov [edx],bx}        DB 67h,89h,1Ah;     DB 66h; add dx,2;   dec cx; jnz @2Qm16s;   jmp @MixEnd
@2Qm16SG: {mov [edx],di}        DB 67h,89h,3Ah;     DB 66h; add dx,2;   dec cx; jnz @2Qm16s;   jmp @MixEnd
{ --------------------------------- 16 Bit stereo compare --------------------------------- }
@2Q16c:   db 66h; mov dx,word ptr MaxLevel
          mov ds,PlayMemSel
@2Qm16c:  DB 66h; mov ax,[si];  add si,4; DB 66h; sar ax,8
          jns @2Qm16c3
          DB 66h; neg ax
@2Qm16c3: DB 66h; cmp ax,dx
          jbe @2Qm16c2
          DB 66h; mov dx,ax
@2Qm16c2: dec cx; jnz @2Qm16c
          jmp @MixEnd
{ --------------------------------- 16 Bit mono --------------------------------- }
@2Q16m:
          cmp search,0
          jne @2Q16mc
          mov ds,PlayMemSel
          DB 66h; mov di, 32767; DW $0000
          DB 66h; mov bx,-32768; DW $FFFF
@2Qm16m:  DB 66h; mov ax,[si+4]
          DB 66h; add ax,[si];  DB 66h; sar ax,8;   add si,8
          DB 66h; cmp ax,bx;    jl @2Qm16mL
          DB 66h; cmp ax,di;    jg @2Qm16mG
          {mov [edx],ax}        DB 67h,89h,02h;     DB 66h; add dx,2;   dec cx; jnz @2Qm16m;   jmp @MixEnd
@2Qm16mL: {mov [edx],bx}        DB 67h,89h,1Ah;     DB 66h; add dx,2;   dec cx; jnz @2Qm16m;   jmp @MixEnd
@2Qm16mG: {mov [edx],di}        DB 67h,89h,3Ah;     DB 66h; add dx,2;   dec cx; jnz @2Qm16m;   jmp @MixEnd
{ --------------------------------- 16 Bit mono compare --------------------------------- }
@2Q16mc:  db 66h; mov dx,word ptr MaxLevel
          mov ds,PlayMemSel
@2Qm16mc: DB 66h; mov ax,[si+4]
          DB 66h; add ax,[si];
          add si,8
          DB 66h; sar ax,8;
          jns @2Qm16mc3
          DB 66h; neg ax
@2Qm16mc3:DB 66h; cmp ax,dx
          jbe @2Qm16mc2
          DB 66h; mov dx,ax
@2Qm16mc2:dec cx; jnz @2Qm16mc
          jmp @MixEnd
{ ################################# 8 Bit ################################# }
@2Qm8:    cmp SoundSettings.Stereo,1
          je  @2Q8m
{ --------------------------------- 8 Bit stereo --------------------------------- }
@2Q8s:    add cx,cx
          mov ds,PlayMemSel
          mov di, 127
          mov bx,-128
          add si,2
@2Qm8s:   mov ax,[si];    add si,4
          cmp ax,bx;      jl @2Qm8sL
          cmp ax,di;      jg @2Qm8sG
          sub al,80h
          {mov [edx],al}  DB 67h,88h,02h;        DB 66h; inc dx;     dec cx; jnz @2Qm8s;    jmp @MixEnd
@2Qm8sL:  {mov [edx],00h} DB 67h,0c6h,02h,000h;  DB 66h; inc dx;     dec cx; jnz @2Qm8s;    jmp @MixEnd
@2Qm8sG:  {mov [edx],FFh} DB 67h,0c6h,02h,0FFh;  DB 66h; inc dx;     dec cx; jnz @2Qm8s;    jmp @MixEnd
{ --------------------------------- 8 Bit mono --------------------------------- }
@2Q8m:    mov ds,PlayMemSel
          mov di, 127
          mov bx,-128
          add si,2
@2Qm8m:   mov ax,[si+4]
          add ax,[si];    add si,8
          cmp ax,bx;      jl @2Qm8mL
          cmp ax,di;      jg @2Qm8mG
          sub al,80h
          {mov [edx],al}  DB 67h,88h,02h;        DB 66h; inc dx;     dec cx; jnz @2Qm8m;    jmp @MixEnd
@2Qm8mL:  {mov [edx],00h} DB 67h,0c6h,02h,000h;  DB 66h; inc dx;     dec cx; jnz @2Qm8m;    jmp @MixEnd
@2Qm8mG:  {mov [edx],ffh} DB 67h,0c6h,02h,0ffh;  DB 66h; inc dx;     dec cx; jnz @2Qm8m;    jmp @MixEnd
{ --------------------------------- Ohne Clipping --------------------------------- }
@1Q:      cmp SoundSettings.bits,8
          je  @1Qm8
          cmp SoundSettings.stereo,1
          je  @1Q16m
{ --------------------------------- 16 Bit stereo --------------------------------- }
@1Q16S:   add cx,cx
          mov ds,PlayMemSel
          inc si
@1Qm16S:  DB 66h; mov ax,[si];  add si,4
          {mov [edx],ax}        DB 67h,89h,02h;     DB 66h; add dx,2;   dec cx; jnz @1Qm16s;   jmp @MixEnd
{ --------------------------------- 16 Bit mono --------------------------------- }
@1Q16m:
          mov ds,PlayMemSel
          inc si
@1Qm16m:  DB 66h; mov ax,[si+4]
          DB 66h; add ax,[si];  add si,8
          {mov [edx],ax}        DB 67h,89h,02h;     DB 66h; add dx,2;   dec cx; jnz @1Qm16m;   jmp @MixEnd
{ ################################# 8 Bit ################################# }
@1Qm8:    cmp SoundSettings.Stereo,1
          je  @1Q8m
{ --------------------------------- 8 Bit stereo --------------------------------- }
@1Q8s:    add cx,cx
          mov ds,PlayMemSel
          add si,2
@1Qm8s:   mov ax,[si];    add si,4
          sub al,80h
          {mov [edx],al}  DB 67h,88h,02h;        DB 66h; inc dx;     dec cx; jnz @1Qm8s;    jmp @MixEnd
{ --------------------------------- 8 Bit mono --------------------------------- }
@1Q8m:    mov ds,PlayMemSel
          add si,2
@1Qm8m:   mov ax,[si+4]
          add ax,[si];    add si,8
          sub al,80h
          {mov [edx],al}  DB 67h,88h,02h;        DB 66h; inc dx;     dec cx; jnz @1Qm8m;    jmp @MixEnd
{ ################################# ENDE ################################# }
@MixEnd:  DB 64h; mov ax,[offset @DS]
          mov ds,ax
          DB 64h,66h; mov bp,word ptr [Offset @EBP]
          DB 64h,66h; mov sp,word ptr [Offset @ESP]
          cmp Search,0
          je @NoSearch
          DB 66h; mov word ptr MaxLevel,dx
@NoSearch:
          DB 66h; mov ax, word Ptr WaveOfs
          DB 66h; mov Word Ptr WaveOfs,dx
          DB 66h; sub dx,ax
          DB 66h; add word Ptr WaveBytePos,dx
          cmp SamplesRemain,0 {Falls noch Samples in den nächsten buffer müssen...RAUS!}
          jnz @Exit
{******************Nächster Sampleblock bearbeitet**********************************}
          mov ax,1
          jmp @Out
@Exit:    xor ax,ax
@Out:
END;

BEGIN
  ASM
    push cs
    call AllocCSToDSAlias
    mov CodeSel,ax
    push ax
    call GlobalPageLock
    {push ds
    call GlobalPageLock{}
  END;
END.