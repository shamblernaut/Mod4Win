@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=
call bp checksum.pas
cd ..\player
ren player32.pas m4w_mci.pas
call bpw m4w_mci.pas /DMCI /DDEMO /DCHECK /R..\intl
ren m4w_mci.pas player32.pas
copy m4w_mci.dll ..\m4w_mci
del m4w_mci.dll
cd ..\m4w_mci
CheckSum m4w_mci.dll
e:\td\rc -r inst_exe.rc
call bpw inst_mci.pas /DCHECK
CheckSum inst_mci.exe
del x:\sound\mod4win\mci_demo\inst_mci.exe
del x:\sound\mod4win\mci_demo\m4w_mci.txt
copy inst_mci.exe x:\sound\mod4win\mci_demo
copy ..\m4w_mci.txt x:\sound\mod4win\mci_demo
checksum x:\sound\mod4win\mci_demo\m4w_mci.txt
del *.tpw
del *.tpu
del *.dll
del *.res
del *.exe