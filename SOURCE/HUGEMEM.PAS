UNIT HugeMEM;
{$IFDEF WINDOWS}
{$C Fixed Preload Permanent}
INTERFACE
USES CORONER;
{$ELSE}
INTERFACE
{$ENDIF}

PROCEDURE CleanUpHugeMem;

FUNCTION HugeAlloc(MemSize:Longint):Word;
FUNCTION HugeReAlloc(Sel:Word;MemSize:Longint):Word;
FUNCTION HugeFree(Sel:Word):Word;
FUNCTION HugeSize(Sel:Word):LongInt;
FUNCTION GetFreeHugeMem:Longint;

PROCEDURE FillMemB(Sel:Word;Ofst,NumBytes:Longint;Pattern:Byte);
PROCEDURE FillMemW(Sel:Word;Ofst,NumWords:Longint;Pattern:Word);
PROCEDURE FillMemD(Sel:Word;Ofst,NumLongs:Longint;Pattern:Longint);

FUNCTION IncPtr(P:Pointer;Ofs:Longint):Pointer;

FUNCTION GetLong(Sel:Word;Ofst:Longint):Longint;
  INLINE ($66/$58/             {pop    eax         }
          $07/                 {pop    es          }
          $26/$67/$66/$8B/$00/ {mov    eax,es:[eax]}
          $66/$0F/$A4/$C2/$10);{shld   edx,eax,10  }

FUNCTION GetWord(Sel:Word;Ofst:Longint):Word;
  INLINE ($66/$58/             {pop    eax         }
          $07/                 {pop    es          }
          $26/$67/$8B/$00);    {mov    ax,es:[eax] }

FUNCTION GetByte(Sel:Word;Ofst:Longint):Byte;
  INLINE ($66/$58/             {pop    eax         }
          $07/                 {pop    es          }
          $26/$67/$8A/$00);    {mov    al,es:[eax] }

PROCEDURE PutLong(Sel:Word;Ofst,What:Longint);
  INLINE ($66/$58/             {pop    eax         }
          $66/$5B/             {pop    ebx         }
          $07/                 {pop    es          }
          $26/$67/$66/$89/$03);{mov    es:[ebx],eax}

PROCEDURE PutWord(Sel:Word;Ofst:Longint;What:Word);
  INLINE ($58/                 {pop    ax          }
          $66/$5B/             {pop    ebx         }
          $07/                 {pop    es          }
          $26/$67/$89/$03);    {mov    es:[ebx],ax }

PROCEDURE PutByte(Sel:Word;Ofst:Longint;What:Byte);
  INLINE ($58/                 {pop    ax          }
          $66/$5B/             {pop    ebx         }
          $07/                 {pop    es          }
          $26/$67/$88/$03);    {mov    es:[ebx],al }

{}

FUNCTION GetLongL(Sel:Word;Ofst:Longint):Longint;
  INLINE ($66/$58/             {pop    eax         }
          $07/                 {pop    es          }
          $66/$C1/$E0/$02/     {shl    eax,2       }
          $26/$67/$66/$8B/$00/ {mov    eax,es:[eax]}
          $66/$0F/$A4/$C2/$10);{shld   edx,eax,10  }

FUNCTION GetWordW(Sel:Word;Ofst:Longint):Word;
  INLINE ($66/$58/             {pop    eax         }
          $07/                 {pop    es          }
          $66/$D1/$E0/         {shl    eax,1       }
          $26/$67/$8B/$00);    {mov    ax,es:[eax] }

PROCEDURE PutLongL(Sel:Word;Ofst,What:Longint);
  INLINE ($66/$58/             {pop    eax         }
          $66/$5B/             {pop    ebx         }
          $07/                 {pop    es          }
          $66/$C1/$E3/$02/     {shl    ebx,2       }
          $26/$67/$66/$89/$03);{mov    es:[ebx],eax}

PROCEDURE PutWordW(Sel:Word;Ofst:Longint;What:Word);
  INLINE ($58/                 {pop    ax          }
          $66/$5B/             {pop    ebx         }
          $07/                 {pop    es          }
          $66/$D1/$E3/         {shl    ebx,1       }
          $26/$67/$89/$03);    {mov    es:[ebx],ax }

IMPLEMENTATION

FUNCTION IncPtr(P:Pointer;Ofs:Longint):Pointer; ASSEMBLER;
ASM
  {Segment berechnen}
  DB 66h; xor ax,ax
  mov ax, word ptr P
  DB 66h; add ax,word Ptr Ofs
  DB 66h; mov bx,ax
  DB 66h; shr ax,16
  mul selectorinc
  mov dx,word ptr p+2
  add dx,ax
  DB 66h; xor ax,ax
  mov ax,bx
  {Ergebnis steht in DS:AX}
END;

TYPE pDPMIMem=^tDPMIMem;
     tDPMIMem=RECORD
       hMem    : Longint;
       LinAddr : Longint;
       Sel0    : Word;
       Size    : Longint;
       Next    : pDPMIMem;
     END;

CONST FirstMem:pDPMIMem=NIL;

FUNCTION DPMIAllocSelectors(NUMSelectors:Word): Word;  ASSEMBLER;
ASM
  mov cx,NumSelectors
  mov ax,0000h
  INT 31h
  jnc @Exit
  xor ax,ax
@Exit:
END;

FUNCTION DPMIFreeSelector(Selector:Word): Word;  ASSEMBLER;
ASM
  mov bx,Selector
  mov ax,0001h
  INT 31h
  jnc @Exit
  xor ax,ax
@Exit:
END;

FUNCTION DPMISelectorInc:Word;ASSEMBLER;
ASM
  mov ax,0003h
  INT 31h
END;

PROCEDURE DPMILockSelector(Sel:Word);ASSEMBLER;
ASM
  mov bx,Sel
  mov ax,0004h
  INT 31h
END;

PROCEDURE DPMIUnLockSelector(Sel:Word);ASSEMBLER;
ASM
  mov bx,Sel
  mov ax,0005h
  INT 31h
END;

FUNCTION DPMIGetSelectorBase(Sel:Word;Base:Longint):Longint;ASSEMBLER;
ASM
  mov bx,Sel
  mov ax,0006h
  INT 31h
  jc  @Error
  mov ax,dx
  mov dx,cx
  jmp @Exit
@Error:
  xor ax,ax
  xor dx,dx
@Exit:
END;

FUNCTION DPMISetSelectorBase(Sel:Word;Base:Longint):Word;ASSEMBLER;
ASM
  mov bx,Sel
  mov cx,word ptr Base+2
  mov dx,word ptr Base
  mov ax,0007h
  INT 31h
  jc @Error
  xor ax,ax
@Error:
END;

FUNCTION DPMISetSelectorLimit(Sel:Word;Limit:Longint):Word;ASSEMBLER;
ASM
  mov bx,Sel
  mov cx,word ptr Limit+2
  mov dx,word ptr Limit
  mov ax,0008h
  INT 31h
  jc @Error
  xor ax,ax
@Error:
END;

FUNCTION DPMISetDSSelectorAccess(Sel:Word):Word;ASSEMBLER;
ASM
  mov bx,Sel
  mov cx,ds
  lar cx,cx
  ror cx,8
  mov ax,0009h
  INT 31h
  jc @Error
  xor ax,ax
@Error:
END;

FUNCTION GetFreeHugeMem:Longint;ASSEMBLER;
VAR BUF:ARRAY[0..128]OF Byte;
ASM
  mov ax,0500h
  mov di,ss
  mov es,di
  lea di,Buf
  INT 31h
  mov ax,0
  mov dx,0
  jc @Error
  mov ax,es:[di+08h+0]
  mov dx,es:[di+08h+2]
  DB $0F,$A4,$C2,$0C {shld   dx,ax,0C}
  SHL AX,$0C
@Error:
END;

FUNCTION DPMIAlloc(Size:Longint;VAR Mem:tDPMIMem): Word;  ASSEMBLER;
ASM
  mov bx,word ptr size+2
  mov cx,word ptr size
  mov ax,0501h
  INT 31h
  jc @Error
  mov ax,si
  les si,Mem
  mov word ptr tDPMIMem(es:[si]).LinAddr+2,bx
  mov word ptr tDPMIMem(es:[si]).LinAddr,cx
  mov word ptr tDPMIMem(es:[si]).hMem+2,ax
  mov word ptr tDPMIMem(es:[si]).hMem,di
  mov bx,word ptr size+2
  mov cx,word ptr size
  mov word ptr tDPMIMem(es:[si]).Size+2,bx
  mov word ptr tDPMIMem(es:[si]).Size,cx
  xor ax,ax
@Error:
END;

FUNCTION DPMIFree(VAR Mem:tDPMiMem): Word;  ASSEMBLER;
ASM
  les si,Mem
  mov cx,word ptr tDPMIMem(es:[si]).LinAddr
  mov bx,word ptr tDPMIMem(es:[si]).LinAddr+2
  mov di,word ptr tDPMIMem(es:[si]).hMem
  mov si,word ptr tDPMIMem(es:[si]).hMem+2
  mov ax,0502h
  INT 31h
  jc @Exit
  xor ax,ax
  les si,Mem
  mov word ptr tDPMIMem(es:[si]).LinAddr,ax
  mov word ptr tDPMIMem(es:[si]).LinAddr+2,ax
  mov word ptr tDPMIMem(es:[si]).hMem,ax
  mov word ptr tDPMIMem(es:[si]).hMem+2,ax
@Exit:
END;

FUNCTION DPMIReAlloc(NewSize:Longint;VAR Mem:tDPMIMem): Word;  ASSEMBLER;
ASM
  les si,Mem
  mov cx,word ptr NewSize
  mov bx,word ptr NewSize+2
  mov di,word ptr tDPMIMem(es:[si]).hMem
  mov si,word ptr tDPMIMem(es:[si]).hMem+2
  mov ax,0503h
  INT 31h
  jc @Error
  mov ax,si
  les si,Mem
  mov word ptr tDPMIMem(es:[si]).LinAddr+2,bx
  mov word ptr tDPMIMem(es:[si]).LinAddr,cx
  mov word ptr tDPMIMem(es:[si]).hMem+2,ax
  mov word ptr tDPMIMem(es:[si]).hMem,di
  mov bx,word ptr NewSize+2
  mov cx,word ptr NewSize
  mov word ptr tDPMIMem(es:[si]).Size+2,bx
  mov word ptr tDPMIMem(es:[si]).Size,cx
  xor ax,ax
@Error:
END;

FUNCTION DPMILock(Mem:tDPMIMem):Word;ASSEMBLER;
ASM
  les si,Mem
  mov cx,word ptr tDPMIMem(es:[si]).LinAddr
  mov bx,word ptr tDPMIMem(es:[si]).LinAddr+2
  mov di,word ptr tDPMIMem(es:[si]).Size
  mov si,word ptr tDPMIMem(es:[si]).Size+2
  mov ax,0600h
  INT 31h
  jc @Error
  xor ax,ax
@Error:
END;

FUNCTION DPMIUnLock(Mem:tDPMIMem):Word;ASSEMBLER;
ASM
  les si,Mem
  mov cx,word ptr tDPMIMem(es:[si]).LinAddr
  mov bx,word ptr tDPMIMem(es:[si]).LinAddr+2
  mov di,word ptr tDPMIMem(es:[si]).Size
  mov si,word ptr tDPMIMem(es:[si]).Size+2
  mov ax,0601h
  INT 31h
  jc @Error
  xor ax,ax
@Error:
END;

function MakeLong(A, B: Word): LongInt;
inline(
  $5A/    { POP DX }
  $58);   { POP AX }

function LoWord(A: LongInt): Word;
inline(
  $58/    { POP AX }
  $5A);   { POP DX }

function HiWord(A: LongInt): Word;
inline(
  $5A/    { POP DX }
  $58);   { POP AX }

FUNCTION GetSelectorLimit(Sel:Word):Longint;ASSEMBLER;
ASM
  DB 66h; xor ax,ax
  mov ax,sel
  DB 66h; lsl ax,ax
  DB 66h; mov dx,ax
  DB 66h; shr dx,16
END;

PROCEDURE DPMIFreeSelectors(VAR DPMIMem:tDPMIMem);
VAR SelInc : Longint;
    NSels  : Longint;
    Sel    : Word;
    I      : Word;
BEGIN
  Sel    := DPMIMem.Sel0;
  SelInc := DPMISelectorInc;
  NSels  := HiWord(DPMIMem.Size);
  FOR I:=0 TO NSels-1 DO
  BEGIN
    DPMIUnLockSelector(Sel);
    DPMIFreeSelector(Sel);
    Inc(Sel,SelInc);
  END;
END;

PROCEDURE CleanUpDPMIMem(VAR DPMIMem:tDPMIMem);
BEGIN
  DPMIFreeSelectors(DPMIMem);
  DPMIUnLock(DPMIMem);
  DPMIFree(DPMIMem);
END;

FUNCTION InitSelectors(VAR DPMIMem:tDPMIMem):Word;
VAR Limit  : Longint;
    Base   : Longint;
    SelInc : Longint;
    NSels  : Longint;
    Sel    : Word;
    I      : Word;
BEGIN
  InitSelectors:=0;
  NSels:=HiWord(DPMIMem.Size);
  DPMiMem.Sel0:=DPMIAllocSelectors(NSels);
  IF DPMiMem.Sel0=0 THEN
  BEGIN
    DPMIUnLock(DPMIMem);
    DPMIFree(DPMIMem);
    Exit;
  END;
  Limit  := DPMIMem.Size;
  Base   := DPMIMem.LinAddr;
  Sel    := DPMIMem.Sel0;
  SelInc := DPMISelectorInc;
  FOR I:=0 TO NSels-1 DO
  BEGIN
    IF (DPMISetSelectorBase(Sel,Base)<>0)
    OR (DPMISetDSSelectorAccess(Sel)<>0)
    OR (DPMISetSelectorLimit(Sel,(Limit-1)OR($FFF))<>0)
    THEN {CleanUp!} BEGIN CleanUpDPMIMem(DPMIMem); Exit; END;
    DPMILockSelector(Sel);
    Inc(Sel,SelInc);
    Inc(Base,65536);
    Dec(Limit,65536);
  END;
  InitSelectors:=1;
END;

FUNCTION HugeAlloc(MemSize:Longint):Word;
VAR DPMIMem:tDPMIMem;
    CurrMem:pDPMIMem;
BEGIN
  HugeAlloc:=0;
  IF MemSize=0 THEN Exit;
  IF LoWord(MemSize)<>0 THEN MemSize:=MakeLong(0,HiWord(MemSize)+1);
  IF GetFreeHugeMem<MemSize THEN Exit;
  IF DPMIAlloc(MemSize,DPMIMem)<>0 THEN Exit;
  IF DPMILock(DPMIMem)<>0 THEN BEGIN DPMIFree(DPMIMem); Exit; END;
  IF InitSelectors(DPMIMem)=0 THEN Exit;
  IF FirstMem=NIL THEN
  BEGIN
    FirstMem:=New(pDPMIMem);
    CurrMem:=FirstMem;
  END ELSE
  BEGIN
    CurrMem:=FirstMem;
    WHILE CurrMem^.Next<>NIL DO CurrMem:=CurrMem^.Next;
    CurrMem^.Next:=New(pDPMIMem);
    CurrMem:=CurrMem^.Next;
  END;
  CurrMem^:=DPMIMem;
  CurrMem^.Next:=NIL;
  CurrMem^.Size:=GetSelectorLimit(DPMIMem.Sel0)+1;
  HugeAlloc:=DPMIMem.Sel0;
END;

FUNCTION HugeFree(Sel:Word):Word;
VAR DPMIMem,PrevMem:pDPMIMem;
BEGIN
  HugeFree:=0;
  IF FirstMem=NIL THEN Exit;
  DPMImem:=FirstMem;
  PrevMem:=NIL;
  WHILE DPMImem<>NIL DO
  BEGIN
    IF DPMImem^.Sel0=Sel THEN
    BEGIN
      IF PrevMem=NIL THEN FirstMem:=DPMIMem^.Next ELSE PrevMem^.Next:=DPMIMem^.Next;
      Break;
    END;
    PrevMem:=DPMImem;
    DPMImem:=DPMImem^.Next;
  END;
  IF DPMIMem=NIL THEN Exit;
  CleanUpDPMIMem(DPMIMem^);
  Dispose(DPMIMem);
END;

FUNCTION HugeReAlloc(Sel:Word;MemSize:Longint):Word;
VAR DPMIMem:pDPMIMem;
    Success:Boolean;
BEGIN
  HugeReAlloc:=0;
  IF MemSize=0 THEN
  BEGIN
    HugeFree(Sel);
    Exit;
  END;
  IF FirstMem=NIL THEN Exit;
  DPMImem:=FirstMem;
  WHILE DPMImem<>NIL DO
  BEGIN
    IF DPMImem^.Sel0=Sel THEN Break;
    DPMImem:=DPMImem^.Next;
  END;
  IF DPMIMem=NIL THEN Exit;
  IF LoWord(MemSize)<>0 THEN MemSize:=MakeLong(0,HiWord(MemSize)+1);
  IF (GetFreeHugeMem+DPMIMem^.Size)<MemSize THEN Exit;
  DPMIFreeSelectors(DPMIMem^);
  DPMIUnLock(DPMIMem^);
  Success:=DPMIReAlloc(MemSize,DPMIMem^)=0;
  IF DPMILock(DPMIMem^)<>0 THEN BEGIN DPMIFree(DPMIMem^); Exit; END;
  IF InitSelectors(DPMIMem^)=0 THEN Exit;
  IF Success THEN HugeReAlloc:=DPMiMem^.Sel0;
END;

FUNCTION HugeSize(Sel:Word):LongInt;
BEGIN
  IF GetSelectorLimit(Sel)<>0 THEN HugeSize:=GetSelectorLimit(Sel)+1
                              ELSE HugeSize:=0;
END;

PROCEDURE FillMemB(Sel:Word;Ofst,NumBytes:Longint;Pattern:Byte);ASSEMBLER;
ASM
  DB 66h; mov di,word ptr ofst
  DB 66h; mov cx,word ptr NumBytes
  mov es,sel
  mov al,Pattern
  DB 67h; rep stosb
END;

PROCEDURE FillMemW(Sel:Word;Ofst,NumWords:Longint;Pattern:Word);ASSEMBLER;
ASM
  DB 66h; mov di,word ptr ofst
  DB 66h; mov cx,word ptr NumWords
  mov es,sel
  mov ax,Pattern
  DB 67h; rep stosw
END;

PROCEDURE FillMemD(Sel:Word;Ofst,NumLongs:Longint;Pattern:Longint);ASSEMBLER;
ASM
  DB 66h; mov di,word ptr ofst
  DB 66h; mov cx,word ptr NumLongs
  mov es,sel
  DB 66h; mov ax,word ptr Pattern
  {rep stosd} DB $67,$F3,$66,$AB
END;

PROCEDURE CleanUpHugeMem;
BEGIN
  WHILE FirstMem<>NIL DO HugeFree(Firstmem^.Sel0);
END;

VAR OldExit:Pointer;
PROCEDURE SafeExit;FAR;
BEGIN
  ExitProc:=OldExit;
  CleanUpHugeMem;
END;


{$IFDEF WINDOWS}
VAR OldCriticalExit:Pointer;
PROCEDURE SafeExit1;FAR;
BEGIN
  @CriticalExit:=OldCriticalExit;
  CleanUpHugeMem;
END;
{$ENDIF WINDOWS}

BEGIN
  OldExit:=ExitProc;
  ExitProc:=@SafeExit;
  {$IFDEF WINDOWS}
  OldCriticalExit:=@CriticalExit;
  @CriticalExit:=@SafeExit1;
  {$ENDIF WINDOWS}
END.