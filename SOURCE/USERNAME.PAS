unit UserName;
{ $C Fixed Preload Permanent}

interface

implementation

uses winprocs,wintypes,strings,win31,omemory,modc;

const
    User:array[0..30] of char=('D','O','O','O','V','I','E','O','F','F','L','I','N','E'
                              ,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);
    Company:array[0..30] of char=('D','O','O','O','V','I','E','O','F','F','L','I','N','E'
                                 ,'2',#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);
var s: string[34];
    i,j:longint;
begin
  if strpas(User)<>'DOOOVIEOFFLINE' then begin
    s:='';
    i:=0;
    repeat
      s:=s+chr(not(byte(user[i])));
      inc(i);
    until (user[i]=#0);
    modc.username:=s;
    s:=' ['+s+']'#0;
    StrCat (longver, @s[1]);
  end;
  if strpas(company)<>'DOOOVIEOFFLINE2' then begin
    s:='';
    i:=0;
    repeat
      s:=s+chr(not(byte(company[i])));
      inc(i);
    until (company[i]=#0);
    compname:=s;
  end;
end.
