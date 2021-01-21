UNIT HyperApi;
INTERFACE
{ HyperDisk installation check.
; Call and return register descriptions for Interrupt 2Fh:
; Calling Registers:
;	AH = Trial Multiplex Number 	; Initially 0DFh
;	AL = 00h		  	; Installed?? request code
;	BX ='DH'			; Disk, HyperWare
;	CX = 00h			; Clear to comfirm return codes
;	DX = 00h			; Clear to comfirm return codes
; Return Registers, Case 1 nothing installed using the Trial Multiplex Number:
;	AL = 00h			; Nothing install
; Return Registers, Case 2 something install, not HyperWare product:
;	AL = 0FFh			; Multiplex Number in use
;	CX = not 'YH'			; not a HyperWare Product!
;					; increment Trial Multiplex Number
;					; and try again until either a free
;					; number found, or a HyperWare product
;					; found, or Multiplex Number > 0FFh.
; Return Registers, Case 3 HyperWare product, but not HyperDisk
;	AL = 0FFh			; Multiplex Number in use
;	CX = 'YH'			; HyperWare Product selected by BX
;					; is installed.
;					;
;	DX = 00h			; zero indicates selected product not
;					; found
; Return Registers, Case 4 HyperDisk installed!
;	AH = HyperWare Multiplex Number	; result of multiplex # search
;	AL = 0FFh		  	; Number in use
;	BX = CS segment of HyperDisk	; Valid if DX not zero
;	CX = 'YH'		  	; HyperWare Product selected by BX
;					; is installed.
;	DX = HyperDisk Local Data Version # (Not HyperDisk Product Version)}

FUNCTION HyperdskInstalled:Boolean;

{ Get Current HyperDisk Cache State
; Call and return register descriptions for Interrupt 2Fh:
; Calling Registers:
;	AH = Multiplex Number	; Value determined by Install Check
;	AL = 01h		; Get current HyperDisk Cache State
;	BX ='DH'		; Product = Disk, HyperWare
; Return Registers:
;	AX = 0000h		; function supported
;	BX = UsedBuffers	; number of cache buffers in use
;	CX = ModifiedBuffers	; number of modified buffers
;	DL = Current Cache values: 0 = Disabled, 1 = Enabled
;	     Bit 0..StagedFloppy: Stage Write floppy drive write operations
;	     Bit 1....StagedHard: Stage Write hard drive write operations
; 	     Bit 2..VerifyFloppy: Verify floppy drive write operations
; 	     Bit 3....VerifyHard: Verify hard drive write operations
; 	     Bit 4.....Reserved0: Reserved always 0
; 	     Bit 5.....Reserved1: Reserved always 0
; 	     Bit 6..FloppyEnable: Enable floppy caching
; 	     Bit 7..CacheEnabled: Enable all caching functions}

FUNCTION HyperGetState:Byte;

{ Set HyperDisk Cache State, return previous Cache State
; Call and return register descriptions for Interrupt 2Fh:
; Calling Registers:
;	AH = Multiplex Number	; Value determined by Install Check
;	AL = 01h		; Get current HyperDisk Cache State
;	BX ='DH'		; Product = Disk, HyperWare
;	DL = New Cache values: 0 = Disabled, 1 = Enabled
;	     Bit 0..StagedFloppy: Stage Write floppy drive write operations
;	     Bit 1....StagedHard: Stage Write hard drive write operations
; 	     Bit 2..VerifyFloppy: Verify floppy drive write operations
; 	     Bit 3....VerifyHard: Verify hard drive write operations
; 	     Bit 4.....Reserved0: Reserved always 0
; 	     Bit 5.....Reserved1: Reserved always 0
; 	     Bit 6..FloppyEnable: Enable floppy caching
; 	     Bit 7..CacheEnabled: Enable all caching functions
; Return Registers:
;	AX = 0000h		; function supported and asynchronously queued
;	BX = UsedBuffers	; number of cache buffers in use
;	CX = ModifiedBuffers	; number of modified buffers
;	DH = Previous Cache values: 0 = Disabled, 1 = Enabled
;	     Bit 0..StagedFloppy: Stage Write floppy drive write operations
;	     Bit 1....StagedHard: Stage Write hard drive write operations
; 	     Bit 2..VerifyFloppy: Verify floppy drive write operations
; 	     Bit 3....VerifyHard: Verify hard drive write operations
; 	     Bit 4.....Reserved0: Reserved always 0
; 	     Bit 5.....Reserved1: Reserved always 0
; 	     Bit 6..FloppyEnable: Enable floppy caching
; 	     Bit 7..CacheEnabled: Enable all caching functions}

PROCEDURE HyperSetState(State:Byte);

IMPLEMENTATION

CONST HyperCallOut:Byte=$DF;
      HyperInstalled:Boolean=False;

FUNCTION HyperdskInstalled:Boolean; ASSEMBLER;
ASM
        cmp     HyperInstalled,1
        je      @SearchHyAPIFound
        mov	ax,352fh
        Int	21h
        mov	cx,es
        jcxz	@SearchHyAPIRet
@SearchHyAPILp:
        XOR	cx,cx
        XOR	dx,dx
        mov	bx,'DH'
        mov	ah,HyperCallOut
        XOR	al,al
        push	ds
        Int	2fh
        pop	ds
        OR	al,al
        je	@SearchHyAPIRet
        cmp	al,-1
        jne	@SearchHyAPINxt
        cmp	cx,'YH'
        jne	@SearchHyAPINxt
        OR	dx,dx
        jz	@SearchHyAPIRet
@SearchHyAPIFound:
        mov     ax,1
        mov HyperInstalled,al
        jmp     @ret
@SearchHyAPINxt:
        inc	HyperCallOut
        jnz	@SearchHyAPILp
@SearchHyAPIRet:
        XOR ax,ax
@ret:
END;

FUNCTION HyperGetState:Byte; ASSEMBLER;
ASM
        cmp     HyperInstalled,1
        jne     @OUT
        mov	ah,HyperCallOut
        mov	bx,'DH'
        mov	al,01h
        Int	2fh
        mov     al,dl
@Out:
END;

PROCEDURE HyperSetState(State:Byte); ASSEMBLER;
ASM
        cmp     HyperInstalled,1
        jne     @OUT
        mov	ah,HyperCallOut
        mov	bx,'DH'
        mov	al,02h
        mov     dl,State
        Int	2fh
@Out:
END;



BEGIN
END.
