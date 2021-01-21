USES Strings;

CONST Start_Tag='The following shareware has been registered by:'#0;
      END_TAG='Total registrations to date:'#0;
      ID1='Registration ID:      7270'#0;
      ID2='Registration ID:      4138'#0;

CONST NAMES:ARRAY[0..10]OF pChar=
       ('Product        : ',
        'Last Name      : ',
        'First Name     : ',
        'Company Name   : ',
        'Address        : ',
        'City           : ',
        'Country        : ',
        'Phone Number   : ',
        'Fax Number     : ',
        'Email Address  : ',
        'Payment        : ');

FUNCTION UpStrPos(What,InWhat:pChar;MaxLen:Word):Word;ASSEMBLER;
ASM
     push ds
     xor ax,ax
     lds si,What
     les di,InWhat
     mov dx,MaxLen
     add dx,di
     mov cx,di
@1:  {Quellstring zu Ende?}
     mov bl,byte ptr [si]
     or bl,bl
     je @Ex
     {Zielstring zu Ende?}
     cmp di,dx
     je @0
     mov bh,byte ptr es:[di]
     or bh,bh
     jne @2
@0:  xor ax,ax {Zielstring zu Ende==> keine Übereinstimmung}
     jmp @Ex
@2:  {Ziel=Quelle?}
     cmp bl,bh
     je @3
     {Quelle='?' ?}
     cmp bl,'?'
     je @3
     {UPCASE: 'a'-'z' und 'à'-'ö'['÷']'ø'-'þ'['ÿ']}
     cmp bl,'a'; jb @a; cmp bl,'z'; jbe @b; cmp bl,'à'; jb @a; cmp bl,'÷'; je @a; cmp bl,'ÿ'; je @a
@b:  sub bl,32
     cmp bl,bh
     je @3
     jmp @d
@a:  cmp bh,'a'; jb @d; cmp bh,'z'; jbe @c; cmp bh,'à'; jb @d; cmp bh,'÷'; je @d; cmp bh,'ÿ'; je @d
@c:  sub bh,32
     cmp bl,bh
     je @3
     {Keine Übereinstimmung!}
@d:  or ax,ax
     je @4
     lds si,What
     mov di,cx
     add di,ax
     xor ax,ax
     jmp @1
@3:  inc si
     or ax,ax {AX=0 ? ==> Adresse merken}
     jne @4
     mov ax,di
     sub ax,cx
     inc ax
@4:  inc di
     jmp @1
@Ex: pop ds
END;

FUNCTION GetEntry(MainStream,SubString,Result:pChar):Boolean;
VAR P:pChar;
    i:Word;
    s:STRING;
BEGIN
  GetEntry:=False;
  Result^:=#0;
  i:=UpStrPos(SubString,MainStream,StrLen(MainStream));
  IF i=0 THEN Exit;
  P:=MainStream;
  Inc(P,i-1+StrLen(SubString));
  s:='';
  WHILE (P^<>#13)AND(P^<>#0) DO BEGIN S:=S+P^; INC(p); END;
  StrPCopy(Result,S);
  GetEntry:=True;
END;

FUNCTION GetAddress(MainStream,Street,City:pChar):Boolean;
VAR P:pChar;
    i:Word;
    s:STRING;
BEGIN
  GetAddress:=False;
  Street^:=#0;
  City^:=#0;
  i:=UpStrPos('Address:              ',MainStream,StrLen(MainStream));
  IF i=0 THEN Exit;
  P:=MainStream;
  Inc(P,i+21);
  s:='';
  WHILE (P^<>#13)AND(P^<>#0) DO BEGIN S:=S+P^; INC(p); END;
  Inc(P,25);
  WHILE (P^<>#13)AND(P^<>#0) DO BEGIN S:=S+P^; INC(p); END;
  StrPCopy(Street,S);
  Inc(P,25);
  S:='';
  WHILE (P^<>#13)AND(P^<>#0) DO BEGIN S:=S+P^; INC(p); END;
  StrPCopy(City,S);
  GetAddress:=True;
END;

PROCEDURE DecodeRegBuffer(Buf,Version,FName,LName,Company,Street,City,Country,Phone,eMail,PayMent:pChar);
VAR P:PChar;
BEGIN
  GetEntry(Buf,'Registration ID:      ',Version);
  GetEntry(Buf,'Member''s Name:        ',FName);
  GetEntry(Buf,'Company Name:         ',Company);
  GetAddress(Buf,Street,City);
  GetEntry(Buf,'Country:              ',Country);
  GetEntry(Buf,'Phone Number:         ',Phone);
  GetEntry(Buf,'CompuServe ID:        [',eMail);
  GetEntry(Buf,'Total Amount (US$):   ',PayMent);
  LName^:=#0;
  P:=StrRScan(FName,' ');
  IF (P<>NIL)THEN
  BEGIN
    P^:=#0;
    Inc(P);
    StrCopy(LName,P);
  END ELSE
  BEGIN
    StrCopy(LName,FName);
    FName^:=#0;
    P:=StrRScan(LName,',');
    IF P<>NIL THEN
    BEGIN
      P^:=#0;
      Inc(P);
      StrCopy(FName,P);
    END;
  END;
  StrScan(eMail,']')^:=#0;
  StrScan(eMail,',')^:='.';
  StrCat(eMail,'@compuserve.com');
  StrCat(Payment,' USD [Compuserve]');
  IF StrComp(Version,'4138')=0
  THEN StrCopy(Version,'Mod4Win Code 2.30')
  ELSE StrCopy(Version,'Mod4Win Update Code 2.30');
END;

FUNCTION ExtractRegs:Longint;
VAR F,F1:FILE;
    P:pChar;
    Buf:ARRAY[0..4095]OF Char;
    NumRead:Word;
    I:Longint;
    Result:Word;
    AllowWrite:Boolean;
    Version : ARRAY[0..80]OF Char;
    FName   : ARRAY[0..80]OF Char;
    LName   : ARRAY[0..80]OF Char;
    Company : ARRAY[0..80]OF Char;
    Street  : ARRAY[0..80]OF Char;
    City    : ARRAY[0..80]OF Char;
    Country : ARRAY[0..80]OF Char;
    Phone   : ARRAY[0..80]OF Char;
    eMail   : ARRAY[0..80]OF Char;
    PayMent : ARRAY[0..80]OF Char;
LABEL Next;
BEGIN
  ExtractRegs:=0;
  Result:=0;
  Assign(F,ParamStr(1));
  InOutRes:=0;
  FileMode:=0;
  Reset(F,1);
  FileMode:=2;
  IF IOResult<>0 THEN Exit;
  Assign(F1,'CIS_REGS.TXT');
  ReWrite(F1,1);
Next:
  P:=Start_Tag;
  AllowWrite:=False;
  WHILE NOT EOF(F) DO
  BEGIN
    BlockRead(F,Buf,SizeOf(Buf),NumRead);
    IF NumRead>StrLen(P) THEN
    BEGIN
      FOR I:=0 TO NumRead-StrLen(P) DO
      BEGIN
        IF StrlIComp(@Buf[i],P,StrLen(P))=0 THEN
        BEGIN
          Seek(F,FilePos(F)-NumRead+i+StrLen(P));
          P:=End_Tag;
          BlockRead(F,Buf,SizeOf(Buf),NumRead);
          FOR i:=0 TO NumRead-StrLen(P)-1 DO
          BEGIN
            IF (StrlIComp(@Buf[i],ID1,StrLen(ID1))=0)
            OR (StrlIComp(@Buf[i],ID2,StrLen(ID2))=0)THEN AllowWrite:=True;
            IF StrlIComp(@Buf[i],P,StrLen(P))=0 THEN
            BEGIN
              IF AllowWrite THEN
              BEGIN
                Inc(Result);
                DecodeRegBuffer(Buf,Version,FName,LName,Company,Street,City,Country,Phone,eMail,PayMent);
                StrCopy(Buf,'----------------------------------------------'#13#10);
                StrCat(Buf,Names[00]); StrCat(Buf,Version); StrCat(Buf,#13#10);
                StrCat(Buf,Names[01]); StrCat(Buf,LName);   StrCat(Buf,#13#10);
                StrCat(Buf,Names[02]); StrCat(Buf,FName);   StrCat(Buf,#13#10);
                StrCat(Buf,Names[03]); StrCat(Buf,Company); StrCat(Buf,#13#10);
                StrCat(Buf,Names[04]); StrCat(Buf,Street);  StrCat(Buf,#13#10);
                StrCat(Buf,Names[05]); StrCat(Buf,City);    StrCat(Buf,#13#10);
                StrCat(Buf,Names[06]); StrCat(Buf,Country); StrCat(Buf,#13#10);
                StrCat(Buf,Names[07]); StrCat(Buf,Phone);   StrCat(Buf,#13#10);
                StrCat(Buf,Names[08]); {StrCat(Buf,Fax);{}  StrCat(Buf,#13#10);
                StrCat(Buf,Names[09]); StrCat(Buf,eMail);   StrCat(Buf,#13#10);
                StrCat(Buf,Names[10]); StrCat(Buf,PayMent); StrCat(Buf,#13#10);
                StrCat(Buf,#0);
                BlockWrite(F1,Buf,Word(StrEnd(Buf))-Word(@Buf));
              END;
              Seek(F,FilePos(F)-NumRead+i+StrLen(P));
              GOTO Next;
            END;
          END;
        END;
      END;
      Seek(F,FilePos(F)-StrLen(P));
    END;
  END;
  Close(F);
  Close(F1);
  ExtractRegs:=Result;
END;


BEGIN
  Writeln(ExtractRegs,' Compuserve eMails converted.');
END.