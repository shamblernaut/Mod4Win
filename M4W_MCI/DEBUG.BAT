@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=
call bp checksum.pas
cd ..\player
ren player32.pas m4w_mci.pas
rem call bpw m4w_mci.pas /DMCI /DMCI_Debug /DDebugOutput /R..\intl
call bpw m4w_mci.pas /DMCI /DMCI_Debug /R..\intl
rem call bpw m4w_mci.pas /DMCI /R..\intl
ren m4w_mci.pas player32.pas
copy m4w_mci.dll ..\m4w_mci
del m4w_mci.dll
cd ..\m4w_mci
rc -r inst_exe.rc
call bpw inst_mci.pas /DCHECK
CheckSum inst_mci.exe
call delbak
del *.dll
del *.res
del checksum.exe