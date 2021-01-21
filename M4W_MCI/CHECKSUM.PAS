program CheckSum;
{$DEFINE MSDOS}
{$M 8192,0,0}

uses strings,crt,dos;


const Username:string='2.40 Beta Demo';
      VerMaj=2;
      VerMin=40;

VAR  x:array[0..32767] of char;
VAR  f:file;
     test:word;
     i:word;
     okay:boolean;

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

LABEL Search;
BEGIN
  TransferUserName (ParamStr(1));
  TransferCheckSum (ParamStr(1));
  setdt(ParamStr(1));
END.