program ValidateMod4WinCopy;
{$DEFINE MSDOS}
{$M 8192,0,0}

uses strings,crt,dos,HyperApi;

TYPE  TLang = (_usa,_ger,_fre,_spa,_grk,_ita,_nl,_cze);

CONST bpc='bpc';
const Username:string='2.40 Beta Demo';
      VerMaj=2;
      VerMin=40;
      INSTALLShare:Boolean=True;
      MOD4WINShare:Boolean=True;
      PLAYER='player\';
      FirstLang = low (TLang);
      LastLang = high (TLang);
      Languages: set of TLang = [];
      LangCode:array[TLang]of string[3]=('usa','ger','fre','spa','grk','ita','nl','cze');
      TextFile:array[TLang]of NameStr = ('readme','liesmich','lisezmoi','readspa','readgk','readita','readnl','readcze');
      HelpStrings: array[TLang, TLang] of string[30] = (
        ('English Help',
         'Englische Hilfe',
         'Aide en Anglais',
         'Ayuda en InglÈs',
         '¡„„ÎÈÍﬁ ¬ÔﬁËÂÈ·',
         'Guida inglese',
         'Engelse Help',
         'Anglick· n·povÏda'),
        ('German Help',
         'Deutsche Hilfe',
         'Aide en Allemand',
         'Ayuda en Alem·n',
         '√ÂÒÏ·ÌÈÍﬁ ¬ÔﬁËÂÈ·',
         'Guida tedesca',
         'Duitse Help',
         'NÏmeck· n·povÏda'),
        ('French Help',
         'Franzˆsische Hilfe',
         'Aide en FranÁais',
         'Ayuda en FrancÈs',
         '√·ÎÎÈÍﬁ ¬ÔﬁËÂÈ·',
         'Guida francese',
         'Franse Help ',
         'Francouzsk· n·povÏda'),
        ('Spanish Help',
         'Spanische Hilfe',
         'Aide en Espagnol',
         'Ayuda en EspaÒol',
         '…Û·ÌÈÍﬁ ¬ÔﬁËÂÈ·',
         'Guida spagnola',
         'Spaanse Help',
         'äpanÏlsk· n·povÏda'),
        ('Greek Help',
         'Griechische Hilfe',
         'Aide en Grec',
         'Ayuda greka',
         '≈ÎÎÁÌÈÍﬁ ¬ÔﬁËÂÈ·',
         'Guida greca',
         'Ayuda en Griego',
         'ÿeck· n·povÏda'),
        ('Italian Help',
         'Italienische Hilfe',
         'Aide en Italiano????',
         'Ayuda (Italian)',
         '?? Italian Help ??',
         'Guida italiana',
         'Italian Help',
         'Italian Help'),
        ('Dutch Help',
         'Niederl‰ndische Hilfe',
         'Aide en ????',
         'Ayuda (Dutch)',
         '?? Dutch Help ??',
         '?? Dutch Help ??',
         'Nederlandse Help',
         'Dutch Help'),
        ('Czech Help',
         'Tschechische Hilfe',
         'Aide en ????',
         'Ayuda (Czech)',
         '?? Czech Help ??',
         '?? Czech Help ??',
         'Chech Help',
         '»esk· n·povÏda'));

VAR  x:array[0..32767] of char;
VAR  f:file;
     sr: SearchRec;
     test:word;
     i:word;
     j:longint;
     k, l: TLang;
     okay:boolean;
     command: PathStr;
     target,temp,root: DirStr;
     s: string;
     ProjectName: string[8];
     RES_Pref, HLP_Pref, INST_Pref: string[4];
     Defines: string[30];
     {Hyperdisk}
     HyperState:Byte;

procedure ChDir (path: string);
  begin
    if path[length (path)] = '\' then dec (path[0]);
    system.ChDir (path);
  end;

function InstFile (i: TLang): PathStr;
begin InstFile := INST_Pref + '_' + LangCode[i]; end;

function ResFile (i: TLang): PathStr;
begin ResFile := RES_Pref + '_' + LangCode[i]; end;

function HelpFile (i: TLang): PathStr;
begin HelpFile := HLP_Pref + '_' + LangCode[i]; end;

procedure writeinf;
VAR f:text;
BEGIN
  inoutres:=0;
  assign(f,target+'install.inf');
  rewrite(f);
  IF IOResult<>0 THEN
  BEGIN
    writeln('Could not create INSTALL.INF!');
    HALT(1);
  END;
  writeln(f,'[INSTALL]');
  write  (f,'APPNAME=', ProjectName, ' ', VerMaj, '.', VerMin);
  IF MOD4WINShare
    THEN writeln(f,' - Demo Version')
    ELSE writeln(f,' - Full Version');
  writeln(f,'GRPNAME=' + ProjectName);
  writeln(f,'DEF_DIR=C:\' + ProjectName);
  writeln(f);
  writeln(f,'[REQFILES]');
  writeln(f,'1=' + ProjectName + '.ex_,' + ProjectName + '.exe');
  writeln(f,'2=player32.dl_,player32.dll');
  writeln(f,'3=trlplay.386,trlplay.386');
  writeln(f,'4=trlplay.VXD,trlplay.VXD');
  writeln(f,'5=trlplay.SYS,trlplay.SYS');
  i := 5;
  for l := FirstLang to LastLang do if l in Languages then begin
    inc (i);
    writeln(f, i, '=' + ResFile(l) + '.dl_,' + ResFile(l) + '.dll');
  end;
  writeln(f);
  writeln(f,'[OPTFILES1]');
  writeln(f,'Name1=Sample Modules');
  writeln(f,'Name2=Beispiel Module');
  writeln(f,'Name3=Modules DÈmonstration');
  writeln(f,'Name4=Modulos de ejemplo');
  writeln(f,'Name5=ƒÂﬂ„Ï·Ù· Modules');
  writeln(f,'Name6=Moduli di esempio');
  writeln(f,'Name7=Voorbeeld Modules');
  writeln(f,'Name8=Uk·zkovÈ moduly');
  writeln(f,'1=*.ns_,*.nst');
  writeln(f,'2=*.mo_,*.mod');
  writeln(f,'3=*.wo_,*.wow');
  writeln(f,'4=*.ok_,*.okt');
  writeln(f,'5=*.st_,*.stm');
  writeln(f,'6=*.s3_,*.s3m');
  writeln(f,'7=*.66_,*.669');
  writeln(f,'8=*.fa_,*.far');
  writeln(f,'9=*.mt_,*.mtm');
  writeln(f,'10=*.ul_,*.ult');
  writeln(f,'11=*.dm_,*.dmf');
  writeln(f,'12=*.xm_,*.xm ');
  writeln(f,'13=*.ml_,*.mol');
  writeln(f,'path=MODULES');
  writeln(f,'size=');
  writeln(f);
  i := 1;
  for l := FirstLang to LastLang do if l in Languages then begin
    inc (i);
    writeln(f,'[OPTFILES', i, ']');
    for k := FirstLang to LastLang do writeln(f,'Name', succ (ord (k)), '=', HelpStrings[l, k]);
    writeln(f,'1=' + HelpFile(l) + '.hl_,' + HelpFile(l) + '.hlp');
    writeln(f,'2=' + TextFile[l] + '.txt,' + TextFile[l] + '.txt');
    writeln(f,'path=.');
    writeln(f,'size=');
    writeln(f);
  end;
  close(f);
END;

procedure setdt(name:string);
VAR dt:datetime;
    ftime:longint;
    cf:file;
BEGIN
  writeln('Setting Date/Time for ',name);
  assign(cf,name);
  InOutRes:=0;
  Filemode:=2;
  reset(cf,1);
  IF iOResult<>0 THEN
  BEGIN
    writeln(name,' not found!!!');
    halt(1);
  END;
  with dt do
  BEGIN
    getdate(year,month,day,hour);
    hour:=VerMaj;
    min:=VerMin;
    sec:=0;
  END;
  packtime(dt,ftime);
  setftime(cf,ftime);
  close(cf);
END;

function FilePresent(fil:String):Boolean;
VAR sr:searchrec;
BEGIN
  FindFirst(fil,0,sr);
  FilePresent:=DosError=0;
END;

procedure GetCheckSum (name: PathStr; var siz, checks: longint);
VAR cf:file;
    i,numread:word;
BEGIN
  writeln('Calculating checksum for ',name,'...');
  InOutRes:=0;
  Filemode:=2;
  assign(cf,name);
  reset(cf,1);
  IF iOResult<>0 THEN
  BEGIN
    writeln(name,' not found!');
    halt(1);
  END;
  {Checksummen berechnen}
  checks:=0;
  REPEAT
   blockread(cf,x,32768,numread);
   IF numread>0 THEN FOR i:=0 to numread-1
   do checks:=checks+byte(x[i]);
  UNTIL numread=0;
  checks:=checks-ORD('B')-ORD('L')-ORD('O')-ORD('E')-ORD('D')-ORD('I')-ORD('E');
  siz:=filesize(cf);
  close(cf);
END;

procedure TransferCheckSum (name: PathStr);
var
  f: file;
  size,checksum: longint;
  i: word;
begin
  GetCheckSum (name, size, checksum);
  writeln('Searching checksum sign for ', name, '...');
  assign (f, name);
  reset(f,1);
  {search Checkpos}
  okay:=false;
  REPEAT
   blockread(f,x,32768,test);
   FOR i:=0 to test-8 do
   BEGIN
    IF ((x[i]='B')and(x[i+1]='L')and(x[i+2]='O')and(x[i+3]='E')and
      (x[i+4]='D')and(x[i+5]='I')and(x[i+6]='E')and(x[i+7]=#0))THEN
    BEGIN
      okay:=true;
      seek(f,(filepos(f)-test)+i);
    END;
   END;
   IF NOT okay THEN seek(f,filepos(f)-8);
  UNTIL okay OR (test<32768);
  IF okay THEN
  BEGIN
    writeln('Checksum sign found and data transferred.');
    blockwrite(f,size,4);
    blockwrite(f,checksum,4);
  END else writeln ('No checksum sign found!');
  close(f);
  writeln;
end;

procedure TransferUserName (name: PathStr);
begin
  writeln('Searching user sign for ', name, '...');
  assign(f, name);
  Filemode:=2;
  reset(f,1);
  {search user name}
  okay:=false;
  REPEAT
   blockread(f,x,32768,test);
   FOR i:=0 to test-14 do
   BEGIN
    IF ((x[i]='D')and(x[i+1]='O')and(x[i+2]='O')and(x[i+3]='O')
    and(x[i+4]='V')and(x[i+5]='I')and(x[i+6]='E'))
    and ((x[i+7]='O')and(x[i+8]='F')and(x[i+9]='F')and(x[i+10]='L')
    and(x[i+11]='I')and(x[i+12]='N')and(x[i+13]='E')and(x[i+14]=#0)) THEN
    BEGIN
      okay:=true;
      seek(f,(filepos(f)-test)+i);
    END;
   END;
   IF NOT okay THEN seek(f,filepos(f)-20);
  UNTIL okay OR (test<32768);
  IF okay THEN
  BEGIN
    writeln('User sign found.  Registering user as "',username, '".');
    {OEM-->ANSI converter...}
    FOR i:=1 to length(username) do x[i-1]:=chr(NOT(byte(username[i])));
    FOR i:=length(username)+1 to 30 do x[i-1]:=#0;
    blockwrite(f,x,30);
  END else writeln ('No user sign found!');
  close(f);
  writeln;
end;

PROCEDURE Usage;
BEGIN
 writeln('Syntax: SETUP.EXE [options]');
 writeln('Options:');
 writeln('  -p<path>        = target directory');
 writeln('    not specified = current directory');
 writeln('  -ufull          = user name will be included by INSTALL.EXE');
 writeln('  -uwin           = user name is taken from the Windows 3.1 clipboard');
 writeln('  -u"<user name>" = user name is taken from the command line');
 writeln('    not specified = create demo version without user name');
 writeln('  -l<lang code>   = include only specified languages');
 writeln('    not specified = include all available languages');
 writeln('Language codes:');
 writeln('  USA - US Engl.  GER - German    FRE - French    SPA - Spanish');
 writeln('  GRK - Greek     ITA - Italian   NL - Dutch      CZE - Czech');
 writeln;
 halt(0);
END;

procedure ErrorMessage (s: string);
begin
  writeln ('Error: ', s);
  writeln;
  Usage;
end;

function GetClipBoard: string;
BEGIN
  ASM
    mov ax,1600h
    int 2fh
    mov test,ax
  END;
  IF (Lo(TEST)=0)OR(Lo(TEST)=$80)THEN
  BEGIN
    Writeln('OOPs...No Windows in 386 MODE??? BYE BYE!!!');
    HALT(1);
  END ELSE Writeln('Windows in Enhanced MODE Version ',Lo(TEST),'.',Hi(Test),' detected!');
  ASM
    mov ax,1700h
    int 2fh
    mov test,ax
  END;
  IF TEST=$1700 THEN
  BEGIN
    Writeln('OOPs...WinOldAp does not support a ClipBoard??? BYE BYE!!!');
    HALT(1);
  END ELSE Writeln('WindOldAp Version ',Lo(TEST),'.',Hi(Test),' detected!');
  ASM
    mov ax,1701h
    int 2fh
    mov ax,1705h
    mov bx,seg x
    mov es,bx
    mov bx,offset x
    mov dx,1
    int 2fh
    mov test,ax
    mov ax,1708h
    int 2fh
  END;
  IF TEST=0 THEN
  BEGIN
    Writeln('OOPs...No ClipBoard-data??? BYE BYE!!!');
    HALT(1);
  END;
  GetClipBoard:=StrPas(@x);
END;

function GetExt (share: boolean): ExtStr;
begin
  if share then GetExt := '.sxe' else GetExt := '.fxe';
end;

procedure CreateFile (cr_file, cm_line, cm_arg: string; rename: boolean);
var Dir: DirStr; Name: NameStr; Ext: ExtStr;
begin
  IF NOT(FilePresent(cr_file))THEN
  BEGIN
    writeln(#13#10,'Creating: ',cr_file);
    {writeln('("',cm_line,' ',cm_arg,'")');}
    exec(cm_line, cm_arg);
    if rename then begin
      FSplit (cr_file, Dir, Name, Ext);
      exec(command,'/C ren '+Dir+Name+'.exe *'+Ext);
    end;
    IF NOT(FilePresent(cr_file))THEN
    begin
      writeln('ERROR! press a key to abort');
      readln;
      Halt(255);
     end;
  END ELSE Writeln(cr_file, ' already exists.');
end;

procedure OEMToANSI (var s: string);
var i: byte;
begin
  FOR i:=1 to length(s) do
  BEGIN
    CASE s[i] OF
     'Ñ':s[i]:=chr(228);
     'é':s[i]:=chr(196);
     'î':s[i]:=chr(246);
     'ô':s[i]:=chr(214);
     'Å':s[i]:=chr(252);
     'ö':s[i]:=chr(220);
     '·':s[i]:=chr(223);
     'Ê':s[i]:='u';
     '˝':s[i]:=chr(178);
     '¸':s[i]:=chr(179);
     '':s[i]:=chr(167);
     '¯':s[i]:=chr(176);
    END;
  END;
end;

LABEL Search;
BEGIN
  {if ParamCount = 0 then Usage;}
  target := '.';
  { interpret options }
  for i := 1 to ParamCount do begin
    s := ParamStr (i);
    if s[1] in ['/', '-'] then begin
      if length (s) = 1 then ErrorMessage ('Mission option. (' + ParamStr (i) + ')');
      if (upcase (s[2]) in ['P', 'U','L']) and (length (s) = 2)
        then ErrorMessage ('Missing parameter. (' + ParamStr (i) + ')');
      case upcase (s[2]) of
        'P': begin delete (s, 1, 2); target := s; end;
        'U': begin
               if s[3] = '"' then
               begin
                 delete (s, 1, 3);
                 UserName := s;
                 while (s[length (s)] <> '"') and (i < ParamCount) do begin
                   inc (i);
                   s := ParamStr (i);
                   UserName := UserName + ' ' + s;
                 end;
                 if UserName[Length(UserName)]='"' THEN DEC(Byte(UserName[0]));
                 if s[length (s)] = '"' then dec (s[0])
                   else ErrorMessage ('End of string not found. (' + UserName + ')');
                 OEMToANSI (UserName);
               end else begin
                 delete (s, 1, 2);
                 s[succ (length (s))] := #0;
                 StrLower (@s[1]);
                 if s = 'full' then InstallShare := false else
                 if s = 'win'  then UserName := GetClipBoard
                   else ErrorMessage ('Invalid option. (' + ParamStr (i) + ')');
               end;
               MOD4WINShare := false;
             end;
        'L': begin
               delete (s, 1, 2);
               s[succ (length (s))] := #0;
               StrLower (@s[1]);
               for l := FirstLang to LastLang do begin
                 if s = LangCode[l] then begin include (Languages, l); break; end;
                 if l = LastLang then ErrorMessage ('Not a language. (' + s + ')');
               end;
             end;
        'H', '?': Usage;
        else ErrorMessage ('Unknown option. (' + ParamStr (i) + ')');
      end;
    end else ErrorMessage ('Not an option. (' + ParamStr (i) + ')');
  end;
  { fix languages }
  if Languages = [] then Languages := [FirstLang..LastLang];
  ProjectName := 'MOD4WIN';
  RES_Pref    := 'res';
  HLP_Pref    := 'm4w';
  INST_Pref   := 'inst';
  {Get Target-Dir...}
  target := FExpand (target);
  IF target[Length(target)]<>'\' THEN target:=target+'\';
  {Get ROOT-Dir...}
  ROOT:=FExpand(Paramstr(0));
  WHILE ROOT[Length(ROOT)]<>'\' DO Dec(ROOT[0]);
  if ROOT = Target then ErrorMessage ('You better specify a target directory...');
  chdir(ROOT);i:=IOResult;InOutRes:=0;
  { show status }
  for i := 1 to 79 do write ('-'); writeln;
  writeln ('target directory: ':19, target);
  writeln ('user name: ':19,        UserName);
  writeln ('creating project: ':19, ProjectName, ' v. ', VerMaj, '.', VerMin);
  write   ('languages used: ':19);
  for l := FirstLang to LastLang do if l in Languages then write (LangCode[l], '  ');
  writeln;
  for i := 1 to 79 do write ('-'); writeln;
  {Find Command.COM...}
  command := Fexpand(FSearch('Command.com',GetEnv('PATH')));
  IF command = '' THEN
  BEGIN
    WriteLn('Command.COM not found!');
    halt(2);
  END;
  IF HyperDskInstalled THEN
  BEGIN
    HyperState:=HyperGetState;
    WRITELN('HYPERDISK detected. Using HYPERAPI!');
    HyperSetState($c3);
  END;
  {Format Disk...}
  IF(target[2]=':')AND (upcase (target[1]) in ['A', 'B']) then begin
Search:
    FindFirst(target[1]+':\*.*',anyfile,sr);
    CASE DosError OF
      18:;
      152,2,3:BEGIN
           Writeln('No disk in Drive '+target[1]+'. Insert and hit a key!');
           readkey;
           GOTO Search;
         END;
      162:BEGIN
            HyperSetState(0);
            Exec(Root+'compiler\format.com',target[1]+': /autotest /F:1.44');
            HyperSetState($c3);
            GOTO Search;
          END;
      ELSE exec(Root+'compiler\xdel.exe',target[1]+':\*.* /sndr');
    END;
  {Label disk}
    Temp[0] := #3;
    Temp[1] := chr(ord('0')+VerMaj);
    Temp[2] := chr(ord('0')+VerMin div 10);
    Temp[3] := chr(ord('0')+VerMin mod 10);
    exec(command, '/C label.exe '+target[1]+': MOD4WIN '+Temp);
  END;
  {Make Target-Dir...}
  s:=target;
  Temp:='';
  while (pos('\',s)<>0) and (inoutres=0) do
  begin
    Temp:=Temp+copy(s,1,pos('\',s)-1);
    chdir(Temp);
    if ioresult<>0 then mkdir(Temp);
    delete(s,1,pos('\',s));
    Temp:=Temp+'\';
  END;
  chdir(ROOT);i:=IOResult;InOutRes:=0;
  {Get TEMP-Dir...}
  Temp := FExpand(GetEnv('TEMP'));
  IF Temp='' THEN Temp := FExpand(GetEnv('TMP'));
  IF TEMP='' THEN Temp:='C:\' ELSE IF Temp[Length(Temp)]<>'\' THEN Temp:=Temp+'\';
  {Compile MOD4WIN.EXE}
  Defines := ' /Q /DCheck ';
  if MOD4WINShare then Defines := Defines + '/DDEMO ';
  CreateFile (ROOT+'mod4win' + GetExt (MOD4WINShare), ROOT+'compiler\'+bpc+'.exe', Defines + ROOT+'mod4win.pas', true);
  {Compile Player32.DLL}
  CreateFile (ROOT+PLAYER+'player32.dll', ROOT+'compiler\'+bpc+'.exe', '/Q '+ROOT+PLAYER+'player32.pas', false);
  {Compile Res_XXX.DLL}
  for l := FirstLang to LastLang do if l in Languages THEN
    CreateFile (ROOT+ResFile(l)+'.dll', ROOT+'compiler\'+bpc+'.exe', Defines+ROOT+ResFile(l)+'.pas', false);
  {Compile INSTALL.EXE}
  Defines := ' /Q ';
  if InstallShare then Defines := Defines + '/DShare ';
  CreateFile (ROOT+'install\install' + GetExt (InstallShare), ROOT+'compiler\'+bpc+'.exe', Defines+
              ROOT+'install\install.pas', true);
  {Compile Inst_XXX.DLL}
  for l := FirstLang to LastLang do if l in Languages then
    CreateFile (ROOT+'install\'+InstFile(l)+'.dll', ROOT+'compiler\'+bpc+'.exe',
                ROOT+'install\'+InstFile(l)+'.pas', false);
  {Remove *.dcu...}
  exec(Root+'compiler\xdel.exe',ROOT+'*.dcu /sn');
  {Remove *.tpw...}
  exec(Root+'compiler\xdel.exe',ROOT+'*.tpw /sn');
  {Copy mod4win.exe for modification}
  writeln;
  writeln ('Copying mod4win.exe to temp directory...');
  exec(command,'/C copy '+ROOT+'mod4win'+GetExt (Mod4WinShare)+' '+TEMP+'mod4win.exe');
  writeln;
  {Transfer user name and checksums}
  IF INSTALLShare THEN
  BEGIN
    TransferUserName (TEMP+'mod4win.exe');
    TransferCheckSum (TEMP+'mod4win.exe');
  END;
  { checksums for DLL's }
  for l := FirstLang to LastLang do if l in Languages then TransferCheckSum (ROOT+ResFile(l)+'.dll');
  {Compress mod4win.exe}
  exec(ROOT+'compiler\compress.exe',TEMP+'mod4win.exe '+TEMP+'mod4win.ex_');
  exec(command,'/C del '+TEMP+'mod4win.exe');
  { create/compress help files }
  for l := FirstLang to LastLang do if l in Languages then
  BEGIN
    IF NOT(FilePresent(ROOT+'help\'+HelpFile(l)+'.hlp'))
      THEN exec(command,'/C '+ROOT+'help\'+HelpFile(l)+'.bat '+ROOT+'help\'+LangCode[l]);
    IF NOT(FilePresent(ROOT+'help\'+HelpFile(l)+'.hl_'))
      THEN exec(ROOT+'compiler\compress.exe',ROOT+'help\'+HelpFile(l)+'.hlp '+ROOT+'help\'+HelpFile(l)+'.hl_');
  END;
  { compress player engine }
  IF NOT(FilePresent(ROOT+PLAYER+'player32.dl_'))
  THEN exec(ROOT+'compiler\compress.exe',ROOT+PLAYER+'player32.dll '+ROOT+PLAYER+'player32.dl_');
  { compress resource files }
  for l := FirstLang to LastLang do if l in Languages then
    IF NOT(FilePresent(ROOT+ResFile(l)+'.dl_'))
    THEN exec(ROOT+'compiler\compress.exe',ROOT+ResFile(l)+'.dll '+ROOT+ResFile(l)+'.dl_');
  { copy all the stuff }
  exec(command,'/C xcopy '+ROOT+'install\install'+GetExt (InstallShare)+' '+TARGET+'*.exe'); { install.exe  }
  for l := FirstLang to LastLang do if l in Languages then
    exec(command,'/C xcopy '+ROOT+'install\'+InstFile(l)+'.dll '+target);                    { inst_???.dll }
  writeinf;                                                                                  { install.inf  }
  exec(command,'/C xcopy ' +TEMP+'mod4win.ex_ '+target);                                      { mod4win.exe  }
  exec(command,'/C del ' +TEMP+'mod4win.ex_ ');                                      { mod4win.exe  }
  exec(command,'/C xcopy '+ROOT+PLAYER+'player32.dl_ '+target);                              { player32.dll }
  for l := FirstLang to LastLang do if l in Languages then
    exec(command,'/C xcopy '+ROOT+ResFile(l)+'.dl_ '+target);                                { res_???.dll  }
  for l := FirstLang to LastLang do if l in Languages then
    exec(command,'/C xcopy '+ROOT+'help\'+HelpFile(l)+'.hl_ '+target);                       { m4w_???.hlp  }
  for l := FirstLang to LastLang do if l in Languages then
    exec(command,'/C xcopy '+ROOT+'defaults\'+TextFile[l]+'.txt '+target);                   { text files   }
  exec(command,'/C xcopy '+ROOT+'defaults\*.??_ /S '+target);                                { modules      }
  exec(command,'/C xcopy '+ROOT+'defaults\*.386 /S '+target);                                { VxDs         }
  exec(command,'/C xcopy '+ROOT+'defaults\*.VXD /S '+target);                                { VxDs         }
  exec(command,'/C xcopy '+ROOT+'defaults\*.SYS /S '+target);                                { VxDs         }
  { set date and time }
  setdt(target+'install.exe');
  for l := FirstLang to LastLang do if l in Languages then
    setdt(target+InstFile(l)+'.dll');
  setdt(target+'install.inf');
  setdt(target+'mod4win.ex_');
  setdt(target+'player32.dl_');
  for l := FirstLang to LastLang do if l in Languages then
    setdt(target+ResFile(l)+'.dl_');
  for l := FirstLang to LastLang do if l in Languages then
    setdt(target+HelpFile(l)+'.hl_');
  for l := FirstLang to LastLang do if l in Languages then if FilePresent (target+TextFile[l]+'.txt') then
    setdt(target+TextFile[l]+'.txt');
  IF HyperDskInstalled THEN
  BEGIN
    WRITELN('Restoring HYPERDISK state....Please wait!');
    HyperSetState(0);
    HyperSetState(HyperState);
  END;
END.