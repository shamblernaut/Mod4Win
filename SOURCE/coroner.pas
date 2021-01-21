UNIT Coroner;
{$C Fixed Preload Permanent}
{ $DEFINE Debug}
INTERFACE
USES ToolHelp,WinProcs,WinTypes;

CONST CriticalExit:PROCEDURE=NIL; {Works like ExitProc...just for Critical Sections that
                                   have to be cleaned up before terminating...}
      CatchBuf:pCatchBuf=NIL;
      {String to display if an Error occurs}
      CriticalErrorName:ARRAY[0..255]OF CHAR='The program has been closed.'+#0;

{Adds Owner of the given Selector to our App Exception Handler}
PROCEDURE AddCriticalSeg(NewSeg:Word);

TYPE  tCoronerLang=(c_ger,c_usa,c_fre,c_spa,c_grk,c_ita,c_nl,c_cze);
CONST CoronerLanguage:tCoronerLang=c_USA;

IMPLEMENTATION
CONST Coroning:Boolean=False; {For ReEntrance}
      CriticalDataSegs:ARRAY[0..15]OF Word=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0); {up to 16 Owners (DLL's,EXE's)
                                                               to check on Faults}
VAR GE:tGlobalEntry;
    ME:tModuleEntry;
    MainTask:Word;
    i:Longint;
    Proc:PROCEDURE;
    ErrorName:pChar;

PROCEDURE AddCriticalSeg(NewSeg:Word);
BEGIN
  IF NewSeg=0 THEN Exit;
  FOR i:=0 TO 15 DO IF CriticalDataSegs[i]=0 THEN
  BEGIN
    ge.dwSize:=SizeOf(ge);
    GlobalEntryHandle(@ge,NewSeg);
    CriticalDataSegs[i]:=ge.hOwner;
    Exit;
  END;
END;

VAR ERRORString,FaultName:ARRAY[0..512]OF Char;
    ERRARR:RECORD
      ExptModule:pChar;
      ExptLogSeg:Word;
      ExptLogOfs:Word;
    END;
PROCEDURE DoExit(FaultSeg,FaultOfs,Exception:Word);
BEGIN
  IF (Exception=1)OR(Exception=3)OR(Exception>14)THEN Exit;
  ge.dwSize:=SizeOf(ge);
  IF (NOT GlobalEntryHandle(@ge,FaultSeg))OR(ge.wType<>GT_Code) THEN Exit;
  me.dwSize:=SizeOf(me);
  IF ModuleFindHandle(@me,ge.hOwner)=0 THEN Exit;
  lstrcpy(FaultName,me.szModule);
  FOR i:=0 TO 15 DO IF CriticalDataSegs[i]=ge.hOwner THEN
  BEGIN
    {}
    CASE CoronerLanguage OF
    c_ITA:
       CASE Exception OF
           0:ErrorName:='Si θ tentata una divisione per zero nel modulo %s all'' indirizzo [%04X:%04X]!';
           6:ErrorName:='Si θ tentata una esecuzione invalida di opcode nel modulo %s all'' indirizzo [%04X:%04X]!';
          12:ErrorName:='Si θ causato un overflow di memoria nel modulo %s all'' indirizzo [%04X:%04X]!';
          13:ErrorName:='Si θ tentato di accedere a memoria non disponibile nel modulo %s  all'' indirizzo [%04X:%04X]!';
          14:ErrorName:='Si θ causato un errore nella pagina di memoria nel modulo %s all'' indirizzo [%04X:%04X]!';
         ELSE Exit;
       END;
    c_SPA:
       CASE Exception OF
           0:ErrorName:='Hemos intentado dividir algo por cero en el mσdulo %s direcciσn [%04X:%04X]!'+#0;
           6:ErrorName:='Hemos intentado ejecutar un opcode invαlido en el modulo %s direcciσn [%04X:%04X]!'+#0;
          12:ErrorName:='Hemos causado un desbordamiento de la pila en el mαdulo %s direcciσn [%04X:%04X]!'+#0;
          13:ErrorName:='Hemos accedido a memoria que no nos pertenece en el mσdulo %s direcciσn [%04X:%04X]!'+#0;
          14:ErrorName:='Hemos causado un fallo de paginaciσn de memoria en el mσdulo %s direcciσn [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    c_GRK:
       CASE Exception OF
           0:ErrorName:='Προσπαθήσαμε να διαιρέσουμε κάτι με μηδέν στο module %s και διεύθυνση [%04X:%04X]!'+#0;
           6:ErrorName:='Προσπαθήσαμε να εκτελέσουμε άκυρο opcode στο module %s και διεύθυνση [%04X:%04X]!'+#0;
          12:ErrorName:='Προκαλέσαμε υπερχείλιση στοίβας στη μνήμη στο module %s και διεύθυνση [%04X:%04X]!'+#0;
          13:ErrorName:='Προσπελάσαμε μνήμη που δε μας ανήκε στο module %s και διεύθυνση [%04X:%04X]!'+#0;
          14:ErrorName:='Προκαλέσαμε memory page fault στο module %s και διεύθυνση [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    c_FRE:
       CASE Exception OF
           0:ErrorName:='Division par zero dans le module %s a l''adresse [%04X:%04X]!'+#0;
           6:ErrorName:='Execution d''un opcode invalide dans le module %s a l''adresse [%04X:%04X]!'+#0;
          12:ErrorName:='Depassement de pile memoire dans le module %s a l''adresse [%04X:%04X]!'+#0;
          13:ErrorName:='Acces a de la memoire deja reservee dans le module %s a l''adresse [%04X:%04X]!'+#0;
          14:ErrorName:='Erreur de page memoire dans le module %s a l''adresse [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    c_GER:
       CASE Exception OF
           0:ErrorName:='Wir haben in %s bei [%04X:%04X] versucht, etwas durch NULL zu teilen!'+#0;
           6:ErrorName:='Wir wollten in %s bei [%04X:%04X] einen ungόltigen Opcode ausfόhren!'+#0;
          12:ErrorName:='Wir verursachten in %s bei [%04X:%04X] einen Stapelspeicherόberlauf!'+#0;
          13:ErrorName:='Wir griffen in %s bei [%04X:%04X] auf Speicher zu, der nicht uns gehφrt!'+#0;
          14:ErrorName:='Wir verursachten einen Seitenfehler in %s bei [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    c_NL:
       CASE Exception OF
           0:ErrorName:='We probeerden iets door nul te delen in module %s op adres [%04X:%04X]!'+#0;
           6:ErrorName:='We probeerden een ongeldige opcode uit te voeren in module %s op adres [%04X:%04X]!'+#0;
          12:ErrorName:='We veroorzaakten een geheugen stack overflow in module %s op adres [%04X:%04X]!'+#0;
          13:ErrorName:='We gebruikten geheugen wat eigenlijk niet toegankelijk was in module %s op adres [%04X:%04X]!'+#0;
          14:ErrorName:='We veroorzaakten en geheugen pagina fout in module %s op adres [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    c_CZE:
       CASE Exception OF
           0:ErrorName:='Pokusili jsme se delit nulou v modulu %s na adrese [%04X:%04X]!'+#0;
           6:ErrorName:='Pokusili jsme se vykonat neplatnou operaci v modulu %s na adrese [%04X:%04X]!'+#0;
          12:ErrorName:='Zpusobili jsme preteceni zasobniku v modulu %s na adrese [%04X:%04X]!'+#0;
          13:ErrorName:='Pristupovali jsme do pameti, ktera nam nepatrila, v modulu %s na adrese [%04X:%04X]!'+#0;
          14:ErrorName:='Zpusobili jsme neplatnost stranky pameti v modulu %s na adrese [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    ELSE
       CASE Exception OF
           0:ErrorName:='We tried to divide something by zero in module %s at address [%04X:%04X]!'+#0;
           6:ErrorName:='We tried to execute an invalid opcode in module %s at address [%04X:%04X]!'+#0;
          12:ErrorName:='We caused a memory stack overflow in module %s at address [%04X:%04X]!'+#0;
          13:ErrorName:='We accessed memory we didn''t actually own in "%s" at address [%04X:%04X]!'+#0;
          14:ErrorName:='We caused a memory page fault in module %s at address [%04X:%04X]!'+#0;
         ELSE Exit;
       END;
    END;
    {}
    ERRARR.ExptModule:=@FaultName;
    ERRARR.ExptLogSeg:=ge.wData;
    ERRARR.ExptLogOfs:=FaultOfs;
    wvsprintf(@ERRORString,ErrorName,ErrArr);
    WHILE @CriticalExit<>NIL DO
    BEGIN
      @Proc:=@CriticalExit;
      @CriticalExit:=NIL;
      Proc;
      IF CatchBuf<>NIL THEN Break;
    END;
    MessageBox(0,CriticalErrorName,@ERRORString,mb_systemModal OR mb_IconStop);
    IF CatchBuf=NIL THEN
    BEGIN
      IF GetCurrentTask<>MainTask THEN TerminateApp(MainTask,NO_UAE_BOX);
      TerminateApp(0,NO_UAE_BOX);
    END ELSE
    BEGIN
      PostAppMessage(MainTask,WM_Quit,0,0);
      Throw(CatchBuf^,1);
    END;
  END;
END;

PROCEDURE DoCoroner; EXPORT; ASSEMBLER;
ASM
  pusha
  push es
  cmp coroning,0
  jne @Out
  mov coroning,1
  push word Ptr ss:[bp+$0E] {Fault-CS}
  push word Ptr ss:[bp+$0C] {Fault-IP}
  push word Ptr ss:[bp+$08] {Fault-ERROR}
  Call DoExit
  mov coroning,0
@Out:
  pop es
  popa
END;

VAR OrgExitProc:Pointer;
    CoronerProc:tIntCallBack;
PROCEDURE ExitDll; FAR;
BEGIN
  ExitProc:=OrgExitProc;
  IF NOT Coroning THEN
  BEGIN
    InterruptUnregister(0);
    FreeProcInstance(@CoronerProc);
  END;
END;

{$IFNDEF Debug}

BEGIN
  OrgExitProc:=ExitProc;
  ExitProc:=@ExitDLL;
  @CoronerProc:=MakeProcInstance(@DoCoroner,hInstance);
  InterruptRegister(0,CoronerProc);
  AddCriticalSeg(hInstance);
  MainTask:=GetCurrentTask;{}
{$ENDIF}

END.