UNIT Cursor;
INTERFACE
USES WinTypes,WinProcs,oMemory,Win31;
{*********************************Fileload-Mauscursor********************************************************}
TYPE pplane=^plane;
     plane=ARRAY[0..31] OF longint;

TYPE pwaitcursor=^waitcursor;
     waitcursor=OBJECT
       hwindow:hwnd;
       Cursor,Cursor1,old:hcursor;
       andplane,xorplane:pplane;
       percnt:longint;
       CONSTRUCTOR init(hpwindow:hwnd);
       PROCEDURE setpercnt(x:longint);
       DESTRUCTOR done;
     END;

IMPLEMENTATION

{*********************************Fileload-Mauscursor********************************************************}
CONSTRUCTOR waitcursor.init;
VAR i:integer;
BEGIN
  hwindow:=hpwindow;
  andplane:=memalloc(sizeof(plane));xorplane:=memalloc(sizeof(plane));
  FOR i:=0 TO 4 DO BEGIN andplane^[i]:=-1;xorplane^[i]:=0; END;
  FOR i:=7 TO 24 DO andplane^[i]:=0;
  FOR i:=27 TO 31 DO BEGIN andplane^[i]:=-1; xorplane^[i]:=0; END;
  xorplane^[07]:=$ffffffff;  xorplane^[24]:=$ffffffff;  xorplane^[05]:=$f8ffff1f;
  xorplane^[26]:=$f8ffff1f;  xorplane^[06]:=$feffff7f;  xorplane^[25]:=$feffff7f;
  andplane^[05]:=$070000e0;  andplane^[26]:=$070000e0;  andplane^[06]:=$01000080;
  andplane^[25]:=$01000080;  xorplane^[08]:=$b335e6bc;  xorplane^[09]:=$add45abb;
  xorplane^[10]:=$2fd55abb;  xorplane^[11]:=$a9d542bb;  xorplane^[12]:=$add55abb;
  xorplane^[13]:=$add55abb;  xorplane^[14]:=$b335da8c;  xorplane^[15]:=$FFFFFFFF;
  xorplane^[16]:=$FFFFFFFF;  xorplane^[17]:=$01000080;  xorplane^[23]:=$01000080;
  percnt:=1; old:=getcursor;
  cursor:=0;
  setpercnt(0);
END;

PROCEDURE waitcursor.setpercnt(x:longint);
VAR a:longint;
    point:tpoint;
BEGIN
  x:=trunc(0.28*x)+2;IF x>15 THEN x:=($ffff shr (x-16)) ELSE x:=($ffffffff shr x);
  x:=(x OR $80000001) AND $bffffffd;
  IF percnt<>x THEN
  BEGIN
    cursor1:=cursor;
    a:=lo(loword(x));
    a:=(a shl 8)+hi(loword(x));
    a:=(a shl 8)+lo(hiword(x));
    a:=(a shl 8)+hi(hiword(x));
    xorplane^[18]:=a;
    xorplane^[19]:=a;
    xorplane^[20]:=a;
    xorplane^[21]:=a;
    xorplane^[22]:=a;
    cursor:=createcursor(hinstance,15,15,32,32,andplane,xorplane);
    getcursorpos(point);
    setcursor(cursor);  percnt:=x;
    IF cursor1<>0 THEN destroycursor(cursor1);
  END;
END;

DESTRUCTOR waitcursor.done;
BEGIN
  setcursor(old); destroycursor(cursor);
  freemem(andplane,sizeof(plane));freemem(xorplane,sizeof(plane));
END;

END.