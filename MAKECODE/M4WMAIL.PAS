PROGRAM M4WMail;

VAR SrcF,DstF,RestF:FILE;
    SrcBuffer,RestBuffer,DstBuffer:ARRAY[0..16383]OF Char;
    SrcOfs,RestOfs,DstOfs:Word;
    SrcCH,DstCH:Char;
    CH1,CH2:Char;
    I:Word;
CONST ENDE:STRING='Send+Registration+Form';
      START:STRING='Users+Name';
      Started:Boolean=False;
      StartPtr:Word=1;
      EndePtr:Word=1;


FUNCTION SrcRead:Word;
VAR NumRead:Word;
BEGIN
  BlockRead(SrcF,SrcBuffer,SizeOf(SrcBuffer),NumRead);
  SrcOfs:=0;
  SrcRead:=NumRead;
END;

FUNCTION DstWrite:Word;
VAR NumWritten:Word;
BEGIN
  BlockWrite(DstF,DstBuffer,DstOfs,NumWritten);
  DstOfs:=0;
  DstWrite:=NumWritten;
END;

FUNCTION RestWrite:Word;
VAR NumWritten:Word;
BEGIN
  BlockWrite(RestF,RestBuffer,RestOfs,NumWritten);
  RestOfs:=0;
  RestWrite:=NumWritten;
END;

CONST LastRead:Word=0;
FUNCTION GetSrcChar:Char;
BEGIN
 GetSrcChar:=#0;
 IF SrcOfs>=LastRead THEN
 BEGIN
   LastRead:=SrcRead;
   IF LastRead=0 THEN Exit;
 END;
 GetSrcChar:=SrcBuffer[SrcOfs];
 Inc(SrcOfs);
END;

FUNCTION MakeHexChar(Ch1,Ch2:Char):Char;
  FUNCTION GetNumber(CH:Char):Byte;
  BEGIN
    CASE Ch OF
      '0'..'9':GetNumber:=Ord(CH)-Ord('0');
      'a'..'f':GetNumber:=10+Ord(CH)-Ord('a');
      'A'..'F':GetNumber:=10+Ord(CH)-Ord('A');
      ELSE GetNumber:=$FF;
    END;
  END;
BEGIN
  MakeHexChar:=Char(GetNumber(Ch1)*16+GetNumber(Ch2));
END;

PROCEDURE WriteDstChar(CH:Char);
BEGIN
  IF DstOfs>=SizeOf(DstBuffer)THEN DstWrite;
  DstBuffer[DstOfs]:=CH;
  Inc(DstOfs);
END;

PROCEDURE WriteRestChar(CH:Char);
VAR I:Word;
BEGIN
  IF DstOfs<>0 THEN
  FOR i:=0 TO DstOfs-1 DO
  BEGIN
    IF RestOfs>=SizeOf(RestBuffer)THEN RestWrite;
    RestBuffer[RestOfs]:=DstBuffer[i];
    Inc(RestOfs);
  END;
  DstOfs:=0;
  IF RestOfs>=SizeOf(RestBuffer)THEN RestWrite;
  RestBuffer[RestOfs]:=CH;
  Inc(RestOfs);
END;

BEGIN
  IF(ParamCount<1)OR(ParamCount>2)THEN
  BEGIN
    Writeln('Usage: ',ParamStr(0),' MailFile [OutFile]');
  END ELSE
  BEGIN
    Assign(SrcF, ParamStr(1));
    Reset(SrcF,1);
    IF ParamCount=2
    THEN Assign(DstF, ParamStr(2))
    ELSE Assign(DstF, 'REGISTER.TXT');
    Reset(DstF,1);
    IF IOResult<>0 THEN Rewrite(DstF,1);
    Seek(DstF, FileSize(DstF));
    Assign(RestF, 'REST.TXT');
    Reset(RestF,1);
    IF IOResult<>0 THEN Rewrite(RestF,1);
    Seek(RestF, FileSize(RestF));
    DstOfs:=0;
    SrcOfs:=$ffff;
    WHILE True DO
    BEGIN
      SrcCH:=GetSrcChar;
      IF (SrcCh=#0) THEN Break;
      IF Started THEN
      BEGIN
        CASE SrcCH OF
          '%':BEGIN
                CH1:=GetSrcChar;
                CH2:=GetSrcChar;
                IF (CH1=#0)OR(CH2=#0) THEN Break;
                WriteDstChar(MakeHexChar(CH1,CH2));
              END;
          '+':WriteDstChar(' ');
          '&':BEGIN WriteDstChar(#13);WriteDstChar(#10);END;
          ELSE WriteDstChar(SrcCH);
        END;
        IF SrcCH=Ende[EndePtr] THEN Inc(EndePtr) ELSE EndePtr:=1;
        IF EndePtr>Length(Ende) THEN
        BEGIN
          Started:=False;
          WriteDstChar(#13);WriteDstChar(#10);
          FOR I:=1 TO 79 DO WriteDstChar('*');
          WriteDstChar(#13);WriteDstChar(#10);
          IF DstOfs<>0 THEN DstWrite;
          StartPtr:=1;
          EndePtr:=1;
        END;
      END ELSE
      BEGIN
        IF SrcCH=Start[StartPtr] THEN
        BEGIN
          IF (SrcCH='+')
          THEN WriteDstChar(' ')
          ELSE WriteDstChar(SrcCH);
          Inc(StartPtr);
          IF StartPtr>Length(Start) THEN Started:=True;
        END ELSE
        BEGIN
          WriteRestChar(SrcCH);
          StartPtr:=1;
          DstOfs:=0;
        END;
      END;
    END;
    IF DstOfs<>0 THEN DstWrite;
    IF RestOfs<>0 THEN RestWrite;
    Close(DstF);
    Close(SrcF);
    Close(RestF);
  END;
END.