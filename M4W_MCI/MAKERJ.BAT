@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=
call bp checksum.pas
cd ..\player
ren player32.pas rjmod4wn.pas
call bpw rjmod4wn.pas /DMCI /R..\intl /DRJM4W
ren rjmod4wn.pas player32.pas
copy rjmod4wn.dll ..\m4w_mci\m4w_mci.dll
del rjmod4wn.dll
cd ..\m4w_mci
e:\td\rc -r inst_exe.rc
call bpw rjmod4wn.pas /DCHECK
CheckSum rjmod4wn.exe
pkzip W:\M4W_HTML\MOD4WIN.COM\files\rjmod4wn rjmod4wn.exe
call delbak
del *.exe
del *.res
del *.dll