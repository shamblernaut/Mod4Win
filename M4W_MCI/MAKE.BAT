@echo off
del *.dll
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=
call bp checksum.pas
cd ..\player
ren player32.pas m4w_mci.pas
call bpw m4w_mci.pas /DMCI /R..\intl
ren m4w_mci.pas player32.pas
copy m4w_mci.dll ..\m4w_mci
del m4w_mci.dll
cd ..\m4w_mci
rc -r inst_exe.rc
call bpw inst_mci.pas /DCHECK
CheckSum inst_mci.exe
del ..\setup\MCI_FULL\inst_mci.exe
del ..\setup\MCI_full\m4w_mci.txt
copy inst_mci.exe ..\setup\MCI_full
copy ..\m4w_mci.txt ..\setup\MCI_full
checksum ..\setup\MCI_full\m4w_mci.txt
call delbak
del *.res
del inst_mci.exe
del checksum.exe