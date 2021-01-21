UNIT Language;
{ $C Fixed Preload Permanent}
INTERFACE
USES modc,winprocs,win31,wintypes,strings,windos,Coroner,WinUnit;

var LangStr: ARRAY[tLang,(_res,_hlp,_name)] OF STRING[12];
    CurrLanguage: TLang;

PROCEDURE ReleaseLanguage;
PROCEDURE LoadLanguage;

FUNCTION GetNextLangID:tLang;

IMPLEMENTATION

FUNCTION GetNextLangID:tLang;
VAR i:tLang;
BEGIN
  i:=CurrLanguage;
  REPEAT
    Inc(i);
    IF i > high (TLang) THEN i := low (TLang);
  UNTIL i IN LangPresent;
  GetNextLangID:=i;
END;

PROCEDURE ReleaseLanguage;
BEGIN
  WHILE GetModuleUsage(hLanguage) > 1 DO FreeLibrary(hLanguage);
  FreeLibrary(hLanguage);
END;

PROCEDURE LoadLanguage;

VAR s:STRING;
    ch:ARRAY[1..30]OF Char;
    olderrmode:word;
    i:byte;
    Lang: TLang;

BEGIN
  s := m4wdir;
  Languages:=0;
  LangPresent := [];
  { initialize LangStrings }
  for Lang := low (TLang) to high (TLang) do begin
    LangStr[Lang, _res]  := RES_Pref + '_' + LangCodes[Lang] + '.DLL';
    LangStr[Lang, _hlp]  := HLP_Pref + '_' + LangCodes[Lang] + '.HLP';
    LangStr[Lang, _name] := LangNames[Lang] + #0;
  end;
  FOR Lang := Low (TLang) TO High (TLang) DO
    IF FilePresent(s+LangStr[Lang,_RES])THEN
      BEGIN Inc(Languages); Include(LangPresent,Lang); END;
  IF Languages=0 THEN
  BEGIN
    MessageBox(0,'No language files (RES_xxx.DLL) found.','Fatal Error',mb_ok OR mb_iconstop);
    PostQuitMessage(0);
    Exit;
  END;
  IF NOT(CurrLanguage IN LangPresent) THEN
  BEGIN
    RES:=LangStr[Currlanguage,_RES];
    CurrLanguage:=GetNextLangID;
    s:='Language file '+RES+' not found.'+#10#10+ProjectName+' will use '+LangStr[Currlanguage,_RES]+' instead.'+#0;
    MessageBox(0,@s[1],errorver,MB_IconAsterisk OR MB_OK);
  END;
  HLP:=LangStr[Currlanguage,_HLP];
  RES:=LangStr[Currlanguage,_RES];
  CoronerLanguage:=Coroner.tCoronerLang(CurrLanguage);
  WinLanguage:=CurrLanguage;
  Str(Byte(CurrLanguage),s);
  {WriteIniString('Language','Language'+#0,s+#0);}
  s:=m4wdir+RES+#0;
  olderrmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  hLanguage:=GetModuleHandle(@s[1]);
  IF hLanguage<=32 THEN hLanguage:=loadMylibrary(@s[1]);
  SetErrorMode(olderrmode);
  {IST HANDLE VALID!}
  IF hLanguage<=32 THEN
  BEGIN
    s:='Could not load '+RES+'.'+#0;
    MessageBox(0,@s[1],'Fatal Error',mb_ok OR mb_iconstop);
    PostQuitMessage(0);
    Exit;
  END;
END;

VAR OldExit: pointer;
    OldCriticalExit: pointer;
{$IFOPT S+} {$DEFINE StackCheck} {$ENDIF} {$S-}

PROCEDURE SafeExit; far;
BEGIN
  ExitProc := OldExit;
  @CriticalExit := OldExit;
  ReleaseLanguage;
END;

{$IFDEF StackCheck} {$S+} {$ENDIF}

BEGIN
  IF HPrevInst = 0 THEN
  BEGIN
    OldExit := ExitProc;
    ExitProc := @SafeExit;
    OldCriticalExit := @CriticalExit;
    CriticalExit := SafeExit;
    CurrLanguage:=tLang(GetPrivateProfileInt('Language','Language',Byte(_USA),IniDir));
    LoadLanguage;
    IF hLanguage<=32 THEN Halt(0);
  END;
END.
