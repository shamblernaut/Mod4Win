program ValidateMod4WinCopy;
{$DEFINE MSDOS}
{$M 8192,0,0}

uses strings,crt,dos,HyperApi;

CONST bpc='bpc';
const Full_Name='2.40 Beta 27';
      Demo_name=Full_Name+' Demo';
      UserName:STRING=Demo_Name;
      VerMaj=2;
      VerMin=40;
      MOD4WINShare:Boolean=True;
      nocheck:Boolean=False;
      PLAYER='player\';

VAR  x:array[0..32767] of char;
VAR  f:file;
     sr: SearchRec;
     test:word;
     i:word;
     j:longint;
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

function ResFile : PathStr;
begin ResFile := RES_Pref + '_USA'; end;

function HelpFile: PathStr;
begin HelpFile := HLP_Pref + '_USA'; end;

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
 writeln('  -n              = compile without self check');
 writeln('  -p<path>        = target directory');
 writeln('    not specified = current directory');
 writeln('  -ufull          = user name will be included by INSTALL.EXE');
 writeln('  -uwin           = user name is taken from the Windows 3.1 clipboard');
 writeln('  -u"<user name>" = user name is taken from the command line');
 writeln('  -d"<demo name>" = demo name is taken from the command line');
 writeln('    not specified = create demo version without user name');
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
    exec(cm_line, cm_arg);
    if rename then begin
      FSplit (cr_file, Dir, Name, Ext);
      exec(command,'/C ren '+Dir+Name+'.exe *'+Ext);
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
      if (upcase (s[2]) in ['P', 'U', 'T', 'L']) and (length (s) = 2)
        then ErrorMessage ('Missing parameter. (' + ParamStr (i) + ')');
      case upcase (s[2]) of
        'N': nocheck:=true;
        'P': begin delete (s, 1, 2); target := s; end;
        'U': begin
               if s[3] = '"' then
               begin
                 delete (s, 1, 3);
                 UserName := s;
                 while (s[length (s)] <> '"') and (i < ParamCount) do
                 begin
                   inc (i);
                   s := ParamStr (i);
                   UserName := UserName + ' ' + s;
                 end;
                 if UserName[Length(UserName)]='"' THEN DEC(Byte(UserName[0]));
                 if s[length (s)] = '"'
                 then dec (s[0])
                 else ErrorMessage ('End of string not found. (' + UserName + ')');
                 OEMToANSI (UserName);
               end else begin
                 delete (s, 1, 2);
                 s[succ (length (s))] := #0;
                 StrLower (@s[1]);
                 if s = 'full' then UserName := Full_Name else
                 if s = 'win'  then UserName := GetClipBoard
                 else ErrorMessage ('Invalid option. (' + ParamStr (i) + ')');
               end;
               MOD4WINShare := false;
             end;
        'D': begin
               if s[3] = '"' then
               begin
                 delete (s, 1, 3);
                 UserName := s;
                 while (s[length (s)] <> '"') and (i < ParamCount) do
                 begin
                   inc (i);
                   s := ParamStr (i);
                   UserName := UserName + ' ' + s;
                 end;
                 if UserName[Length(UserName)]='"' THEN DEC(Byte(UserName[0]));
                 if s[length (s)] = '"'
                 then dec (s[0])
                 else ErrorMessage ('End of string not found. (' + UserName + ')');
                 OEMToANSI (UserName);
               end else begin
                 delete (s, 1, 2);
                 s[succ (length (s))] := #0;
                 StrLower (@s[1]);
                 if s = 'win'  then UserName := GetClipBoard
                 else ErrorMessage ('Invalid option. (' + ParamStr (i) + ')');
               end;
             end;
        'H', '?': Usage;
        else ErrorMessage ('Unknown option. (' + ParamStr (i) + ')');
      end;
    end else ErrorMessage ('Not an option. (' + ParamStr (i) + ')');
  end;
  { fix languages }
  ProjectName := 'MOD4WIN';
  RES_Pref := 'res';
  HLP_Pref := 'm4w';
  INST_Pref := 'inst';
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
            Exec(Root+'..\compiler\format.com',target[1]+': /autotest /F:1.44');
            HyperSetState($c3);
            GOTO Search;
          END;
      ELSE exec(Root+'..\compiler\xdel.exe',target[1]+':\*.* /sndr');
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
  IF NoCheck
  THEN Defines := ' /Q '
  ELSE Defines := ' /Q /DCheck ';
  if MOD4WINShare then Defines := Defines + '/DDEMO ';
  CreateFile (ROOT+'mod4win' + GetExt (MOD4WINShare), ROOT+'..\compiler\'+bpc+'.exe', Defines + ROOT+'mod4win.pas', true);
  {Transfer user name and checksums}
  TransferUserName (ROOT+'mod4win' + GetExt (MOD4WINShare));
  {Compile Player32.DLL}
  CreateFile (ROOT+PLAYER+'player32.dll', ROOT+'..\compiler\'+bpc+'.exe', '/Q '+ROOT+PLAYER+'player32.pas', false);
  {Compile Res_XXX.DLL}
  CreateFile (ROOT+ResFile+'.dll', ROOT+'..\compiler\'+bpc+'.exe', Defines+ROOT+ResFile+'.pas', false);
  {Remove *.dcu...}
  exec(Root+'..\compiler\xdel.exe',ROOT+'*.dcu /sn');
  {Remove *.tpw...}
  exec(Root+'..\compiler\xdel.exe',ROOT+'*.tpw /sn');
  {Copy mod4win.exe for modification}
  writeln;
  writeln ('Copying mod4win.exe to temp directory...');
  exec(command,'/C copy '+ROOT+'mod4win'+GetExt (Mod4WinShare)+' '+TEMP+'mod4win.exe');
  writeln;
  {Transfer checksums}
  TransferCheckSum (TEMP+'mod4win.exe');
  { checksums for DLL's }
  TransferCheckSum (ROOT+ResFile+'.dll');
  { create/compress help files }
  IF NOT(FilePresent(ROOT+'help\'+HelpFile+'.hlp'))
  THEN exec(command,'/C '+ROOT+'help\'+HelpFile+'.bat '+ROOT+'help\USA');
  { copy all the stuff }
  exec(command,'/C xcopy '+TEMP+'mod4win.exe '+target);
  exec(command,'/C del '  +TEMP+'mod4win.exe ');
  exec(command,'/C xcopy '+ROOT+PLAYER+'player32.dll '+target);
  exec(command,'/C xcopy '+ROOT+ResFile+'.dll '+target);
  IF MOD4WINShare THEN
  BEGIN
    exec(command,'/C xcopy '+ROOT+'help\'+HelpFile+'.hlp '+target);
    exec(command,'/C xcopy '+ROOT+'defaults\readme.txt '+target);
    exec(command,'/C xcopy '+ROOT+'defaults\reg_eng.txt '+target);
  END;
  exec(command,'/C xcopy '+ROOT+'whatsnew.txt '+target);
  exec(command,'/C xcopy '+ROOT+'defaults\*.386 /S '+target);
  exec(command,'/C xcopy '+ROOT+'defaults\*.VXD /S '+target);
  exec(command,'/C xcopy '+ROOT+'defaults\*.SYS /S '+target);
  { set date and time }
  setdt(target+'mod4win.exe');
  setdt(target+'player32.dll');
  setdt(target+ResFile+'.dll');
  IF MOD4WINShare THEN
  BEGIN
    setdt(target+HelpFile+'.hlp');
    setdt(target+'readme.txt');
    setdt(target+'reg_eng.txt');
  END;
  setdt(target+'whatsnew.txt');
  setdt(target+'trlplay.vxd');
  setdt(target+'trlplay.sys');
  setdt(target+'trlplay.386');
  IF HyperDskInstalled THEN
  BEGIN
    WRITELN('Restoring HYPERDISK state....Please wait!');
    HyperSetState(0);
    HyperSetState(HyperState);
  END;
END.