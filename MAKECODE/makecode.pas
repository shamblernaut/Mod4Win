{$N+,E-}

{ $DEFINE DLL}

{$IFDEF DLL}
LIBRARY MakeCode;
{$C Fixed,PreLoad,Permanent}

USES VK_KEYS,Win31,Strings,WinAPI,WinTypes,Winprocs;
{$ELSE}
UNIT MakeCode;
INTERFACE
{$IFDEF WINDOWS}
USES VK_KEYS,WinProcs,WinTypes,WinAPI,Win31,Strings;
{$ELSE}
USES VK_KEYS,Strings,HugeMem;
{$ENDIF}

PROCEDURE GenerateCode(Version,pname,pcomp,pinfo,Result:pChar);

{$IFDEF WINDOWS}
PROCEDURE MakeRegFile(FileName,Name,Company,PayMent,Code:pChar);
{$ENDIF}

PROCEDURE MakeRegText(Result,Name,Company,PayMent,Code:pChar);
PROCEDURE MakeEMailRegText(Result,Name,Company,PayMent,Code:pChar);
FUNCTION DecodeRegBuffer(ClipData,Version,Name,Company,PayMent,Code:pChar):Word;

PROCEDURE MakeCISRegText(Result,Name,Company,PayMent,Code:pChar);
{$IFDEF WINDOWS}
PROCEDURE MakeCISCode;
{$ENDIF}
IMPLEMENTATION
{$ENDIF}

PROCEDURE GenerateCode(Version,pname,pcomp,pinfo,Result:pChar); {$IFDEF DLL}EXPORT; {$ENDIF}
VAR accu: extended;
    Name,compn,info:STRING[50];
    Res:STRING[50];
    i:Word;
    ver:Word;
BEGIN
  IF (Result=NIL)OR(Version=NIL) THEN Exit;
  Result^:=#0;
  IF pName<>NIL THEN Name:=StrPas(pName) ELSE Name:='';
  IF pComp<>NIL THEN compn:=StrPas(pComp)ELSE compn:='';
  IF pInfo<>NIL THEN info:=StrPas(pInfo) ELSE info:='';
  WHILE (NOT((Version^='.')OR(Version^=#0))) DO inc(Version);
  IF Version^=#0 THEN Exit;
  Dec(Version);
  IF Version^ IN ['0'..'9'] THEN Ver:=10*(Ord(Version^)-Ord('0')) ELSE Exit;
  Inc(Version);
  Inc(Version);
  IF Version^ IN ['0'..'9'] THEN Ver:=Ver+(Ord(Version^)-Ord('0')) ELSE Exit;
  IF ver>=23
  THEN RandSeed := (ver-1) * $74E691BA
  ELSE RandSeed := ver * $74E691BA;
  accu := Random;
  IF Length(Name)<>0  THEN FOR i := 1 TO Length(Name)  DO accu := accu * Ord (Name[i])/256 * Random;
  IF Length(compn)<>0 THEN FOR i := 1 TO Length(compn) DO accu := accu * Ord (compn[i])/256 * Random;
  IF Length(info)<>0  THEN FOR i := 1 TO Length(info)  DO accu := accu * Ord (info[i])/256 * Random;
  Str (accu:28, res);
  dec (res[0], 6);
  WHILE NOT (res[1] IN ['0'..'9']) DO Delete (res, 1, 1);
  Delete (res, 2, 1);
  Insert('-',res,4);
  Insert('-',res,8);
  Insert('-',res,12);
  Insert('-',res,16);
  Insert('-',res,20);
  StrPCopy(Result,Res);
END;

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

PROCEDURE MakeCisRegText(Result,Name,Company,PayMent,Code:pChar);
BEGIN
  StrCopy(Result,'Thank you very much for your MOD4WIN 2.30 registration.'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'If you don''t have MOD4WIN 2.30 yet, please download it'#13#10);
  StrCat (Result,'immedately from http://www.mod4win.com.'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'You can unlock your MOD4WIN 2.3x demo version if you type'#13#10);
  StrCat (Result,'the following information into the shareware registration'#13#10);
  StrCat (Result,'box that appears if you click on the "Register" button on'#13#10);
  StrCat (Result,'the MOD4WIN startup shareware notification screen:'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Name     : ');StrCat(Result,Name);StrCat(Result,#13#10);
  StrCat (Result,'Company  : ');StrCat(Result,Company);StrCat(Result,#13#10);
  StrCat (Result,'Payment  : ');StrCat(Result,PayMent);StrCat(Result,#13#10);
  StrCat (Result,'Reg. Code: ');StrCat(Result,Code);StrCat(Result,#13#10);
  StrCat (Result,'THE LETTERS MUST BE CASE SENSITIVE!'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'We will automaticly inform you about major MOD4WIN updates.'+#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Have much fun with MOD4WIN!'+#13#10);
  StrCat (Result,'Very best Regards'+#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Kay Bruns'+#13#10);
END;

PROCEDURE MakeEMailRegText(Result,Name,Company,PayMent,Code:pChar);
BEGIN
  StrCopy(Result,'Thank you very much for your MOD4WIN 2.30 registration.'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'If you don''t have MOD4WIN 2.30 yet, please download the'#13#10);
  StrCat (Result,'latest version immedately from the MOD4WIN home page at'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,#9'http://www.mod4win.com.'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'You can unlock your MOD4WIN 2.3x demo version if you type'#13#10);
  StrCat (Result,'the following information into the shareware registration'#13#10);
  StrCat (Result,'box that appears if you click on the "Register" button on'#13#10);
  StrCat (Result,'the MOD4WIN startup shareware notification screen:'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Name     : ');StrCat(Result,Name);StrCat(Result,#13#10);
  StrCat (Result,'Company  : ');StrCat(Result,Company);StrCat(Result,#13#10);
  StrCat (Result,'Payment  : ');StrCat(Result,PayMent);StrCat(Result,#13#10);
  StrCat (Result,'Reg. Code: ');StrCat(Result,Code);StrCat(Result,#13#10);
  StrCat (Result,'THE LETTERS MUST BE CASE SENSITIVE!'#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'We will automaticly inform you about major MOD4WIN updates.'+#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Have much fun with MOD4WIN!'+#13#10);
  StrCat (Result,'Very best Regards'+#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,#13#10);
  StrCat (Result,'Kay Bruns'+#13#10);
END;

{$IFDEF WINDOWS}
PROCEDURE MakeRegFile(FileName,Name,Company,PayMent,Code:pChar);
VAR F:Text;
    ClipData:pChar;
BEGIN
  InOutRes:=0;
  Assign(F,FileName);
  Rewrite(F);
  IF IOResult<>0 THEN Exit;
  Writeln(F,'REGEDIT');
  Write(F,'HKEY_CLASSES_ROOT\ModulePlayer\data\registration\user = ');    Writeln(F,Name);
  Write(F,'HKEY_CLASSES_ROOT\ModulePlayer\data\registration\company = '); Writeln(F,Company);
  Write(F,'HKEY_CLASSES_ROOT\ModulePlayer\data\registration\info = ');    Writeln(F,PayMent);
  Write(F,'HKEY_CLASSES_ROOT\ModulePlayer\data\registration\code = ');    Writeln(F,Code);
  Close(F);
  ClipData:=GlobalAllocPtr(gmem_share,4096);
  StrCopy(ClipData,'Thank you very much for your MOD4WIN 2.30 registration.'#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'If you don''t have MOD4WIN 2.30 yet, please download the'#13#10);
  StrCat (ClipData,'latest version immedately from the MOD4WIN home page at'#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,#9'http://www.mod4win.com.'#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'You can unlock your MOD4WIN 2.3x demo version if double click'#13#10);
  StrCat (ClipData,'on the file attached to this eMail MOD4WIN or if you type'#13#10);
  StrCat (ClipData,'the following information into the shareware registration'#13#10);
  StrCat (ClipData,'box that appears if you click on the "Register" button on'#13#10);
  StrCat (ClipData,'the MOD4WIN startup shareware notification screen:'#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'Name     : ');StrCat(ClipData,Name);StrCat(ClipData,#13#10);
  StrCat (ClipData,'Company  : ');StrCat(ClipData,Company);StrCat(ClipData,#13#10);
  StrCat (ClipData,'Payment  : ');StrCat(ClipData,PayMent);StrCat(ClipData,#13#10);
  StrCat (ClipData,'Reg. Code: ');StrCat(ClipData,Code);StrCat(ClipData,#13#10);
  StrCat (ClipData,'THE LETTERS MUST BE CASE SENSITIVE!'#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'I will automaticly inform you about major MOD4WIN updates.'+#13#10);
  StrCat (ClipData,'Please keep a copy of the attached registration file in'+#13#10);
  StrCat (ClipData,'case you loose this eMail.'+#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'Have much fun with MOD4WIN!'+#13#10);
  StrCat (ClipData,'Very best Regards'+#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,#13#10);
  StrCat (ClipData,'Kay Bruns'+#13#10);
  OpenClipBoard(GetActiveWindow);
  EmptyClipBoard;
  SetClipBoardData(CF_TEXT,SelectorOf(ClipData));
  CloseClipBoard;
END;
{$ENDIF}

PROCEDURE MakeRegText(Result,Name,Company,PayMent,Code:pChar);
BEGIN
  StrCopy(Result,'Name     : ');StrCat(Result,Name);StrCat(Result,#13#10);
  StrCat(Result,'Company  : ');StrCat(Result,Company);StrCat(Result,#13#10);
  StrCat(Result,'Payment  : ');StrCat(Result,PayMent);StrCat(Result,#13#10);
  StrCat(Result,'Reg. Code: ');StrCat(Result,Code);StrCat(Result,#13#10);
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
  WHILE (P^<>#13)AND(P^<>#0) DO
  BEGIN
    IF (P^=':')OR(P^=';')THEN S:='' ELSE S:=S+P^;
    INC(p);
  END;
  StrPCopy(Result,S);
  GetEntry:=True;
END;

FUNCTION DecodeRegBuffer(ClipData,Version,Name,Company,PayMent,Code:pChar):Word;
VAR Name1:pChar;
BEGIN
  DecodeRegBuffer:=0;
  IF NOT GetEntry(ClipData,'Name     : ',Name)THEN
  IF NOT GetEntry(ClipData,' Member''s Name:        ',Name)THEN
  BEGIN
    GetEntry(ClipData,'First Name',Name);
    StrCat(Name,' ');
    Name1:=StrEnd(Name);
    GetEntry(ClipData,'Last Name',Name1);
    DecodeRegBuffer:=1;
  END;
  IF NOT GetEntry(ClipData,'Company  : ',Company)THEN
  IF NOT GetEntry(ClipData,' Company Name:         ',Company)
  THEN GetEntry(ClipData,'Company Name',Company);
  IF NOT GetEntry(ClipData,'Payment  : ',PayMent) THEN
  BEGIN
    IF GetEntry(ClipData,' CompuServe ID:        ',Code) THEN
    BEGIN
      StrCopy(Payment,'Compuserve ');
      StrCat(Payment,Code);
    END ELSE
    IF GetEntry(ClipData,'Credit Card Number',PayMent) THEN
    BEGIN
      GetEntry(ClipData,'Credit Card Type',Payment);
    END ELSE GetEntry(ClipData,'Payment',Payment);
  END;
  Code^:=#0;
  GenerateCode(Version,Name,Company,PayMent,Code);
END;

{$IFDEF WINDOWS}
PROCEDURE MakeCISCode;{$IFDEF DLL}EXPORT; {$ENDIF}
VAR Name,Company,PayMent,Code:ARRAY[0..50]OF Char;
    Result:pChar;
    ClipData:ARRAY[0..1024]OF Char;
    hClipBoard:Word;
BEGIN
  OpenClipBoard(GetActiveWindow);
  hClipBoard:=GetClipBoardData(CF_TEXT);
  CloseClipBoard;
  IF hClipBoard=0 THEN
  BEGIN
    Exit;
  END;
  StrCopy(ClipData,Ptr(hClipBoard,0));
  Result:=GlobalAllocPtr(gmem_ddeshare,4096);
  IF DecodeRegBuffer(ClipData,'MOD4WIN 2.30',Name,Company,PayMent,Code)=0 THEN
  BEGIN
    MakeCisRegText(Result,Name,Company,PayMent,Code);
    OpenClipBoard(GetActiveWindow);
    EmptyClipBoard;
    SetClipBoardData(CF_TEXT,SelectorOf(Result));
    CloseClipBoard;
  END ELSE
  BEGIN
    MakeRegFile('g:\net_temp\M4W.REG',Name,Company,PayMent,Code);
  END;
END;
{$ENDIF}

{$IFDEF DLL}
EXPORTS
  GenerateCode index 1,
  MakeCISCode  index 2;
{$ENDIF}
BEGIN
END.
