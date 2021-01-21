USES Strings;

CONST Start_Tag='**************************'#0;

CONST NAMES:ARRAY[0..11]OF pChar=
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
        'Payment        : ',
        'Date           : ');

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

PROCEDURE DecodeRegBuffer(Buf,Date,Version,FName,LName,Company,Street,City,Country,Phone,Fax,eMail,PayMent:pChar);
VAR Disk:ARRAY[0..80] OF Char;
BEGIN
  GetEntry(Buf,'**************************'#13#10,Date);
  GetEntry(Buf,'Product            :',Version);
  GetEntry(Buf,'Delivery           :',Disk);
  GetEntry(Buf,'Last Name          :',LName);
  GetEntry(Buf,'First Name         :',FName);
  GetEntry(Buf,'Company Name       :',Company);
  GetEntry(Buf,'Address            :',Street);
  GetEntry(Buf,'City               :',City);
  GetEntry(Buf,'Country            :',Country);
  GetEntry(Buf,'Phone Number       :',Phone);
  GetEntry(Buf,'Fax Number         :',Fax);
  GetEntry(Buf,'Email Address      :',eMail);
  GetEntry(Buf,'Payment            :',PayMent);
  IF Disk[0]<>#0 THEN StrCopy(Version,'Mod4Win Disk 2.30');
  IF StrComp(PayMent,'Credit Card')=0 THEN
  BEGIN
    GetEntry(Buf,'Credit Card Type   :',PayMent);
    GetEntry(Buf,'Credit Card Number :',Disk);
    StrCat(Payment,' ;');
    StrCat(Payment,Disk);
    GetEntry(Buf,'Expiration Date    :',Disk);
    StrCat(Payment,' - ');
    StrCat(Payment,Disk);
  END;
END;

FUNCTION ExtractRegs:Longint;
VAR F,F1:FILE;
    Buf:ARRAY[0..4095]OF Char;
    NumRead:Word;
    I:Longint;
    Result:Word;
    Date    : ARRAY[0..80]OF Char;
    Version : ARRAY[0..80]OF Char;
    FName   : ARRAY[0..80]OF Char;
    LName   : ARRAY[0..80]OF Char;
    Company : ARRAY[0..80]OF Char;
    Street  : ARRAY[0..80]OF Char;
    City    : ARRAY[0..80]OF Char;
    Country : ARRAY[0..80]OF Char;
    Phone   : ARRAY[0..80]OF Char;
    Fax     : ARRAY[0..80]OF Char;
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
  Assign(F1,'AKM_REGS.TXT');
  ReWrite(F1,1);
Next:
  WHILE NOT EOF(F) DO
  BEGIN
    BlockRead(F,Buf,SizeOf(Buf),NumRead);
    IF NumRead>StrLen(Start_Tag) THEN
    BEGIN
      FOR I:=0 TO NumRead-StrLen(Start_Tag) DO
      BEGIN
        IF StrlIComp(@Buf[i],Start_Tag,StrLen(Start_Tag))=0 THEN
        BEGIN
          Seek(F,FilePos(F)-NumRead+i+StrLen(Start_Tag));
          BlockRead(F,Buf,SizeOf(Buf),NumRead);
          DecodeRegBuffer(Buf,Date,Version,FName,LName,Company,Street,City,Country,Phone,Fax,eMail,PayMent);
          StrCopy(Buf,'----------------------------------------------'#13#10);
          StrCat(Buf,Names[11]); StrCat(Buf,Date);    StrCat(Buf,#13#10);
          StrCat(Buf,Names[00]); StrCat(Buf,Version); StrCat(Buf,#13#10);
          StrCat(Buf,Names[01]); StrCat(Buf,LName);   StrCat(Buf,#13#10);
          StrCat(Buf,Names[02]); StrCat(Buf,FName);   StrCat(Buf,#13#10);
          StrCat(Buf,Names[03]); StrCat(Buf,Company); StrCat(Buf,#13#10);
          StrCat(Buf,Names[04]); StrCat(Buf,Street);  StrCat(Buf,#13#10);
          StrCat(Buf,Names[05]); StrCat(Buf,City);    StrCat(Buf,#13#10);
          StrCat(Buf,Names[06]); StrCat(Buf,Country); StrCat(Buf,#13#10);
          StrCat(Buf,Names[07]); StrCat(Buf,Phone);   StrCat(Buf,#13#10);
          StrCat(Buf,Names[08]); StrCat(Buf,Fax);     StrCat(Buf,#13#10);
          StrCat(Buf,Names[09]); StrCat(Buf,eMail);   StrCat(Buf,#13#10);
          StrCat(Buf,Names[10]); StrCat(Buf,PayMent); StrCat(Buf,#13#10);
          StrCat(Buf,#0);
          BlockWrite(F1,Buf,Word(StrEnd(Buf))-Word(@Buf));
          Seek(F,FilePos(F)-NumRead);
          Inc(Result);
          GOTO Next;
        END;
      END;
      Seek(F,FilePos(F)-StrLen(Start_Tag));
    END;
  END;
  Close(F);
  Close(F1);
  ExtractRegs:=Result;
END;


BEGIN
  Writeln(ExtractRegs,' AK-Mail eMails converted.');
END.