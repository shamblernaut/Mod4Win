@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=

call cleanup
del player\*.dl?

xdel setup\*.* /sndr
md setup
md setup\m4w_full
md setup\mci_full
call bp setup
if not exist setup.exe goto exit
echo MOD4WIN files
setup -p.\setup\M4W_FULL\ -u"2.40 Last Beta" -lUSA -lGER
if errorlevel=255 goto exit
del setup.exe
if not exist MOD4WIN.fxe goto exit
copy MOD4WIN.fxe CUSTOM
echo CUSTOM files
cd CUSTOM
call make
cd ..

echo M4W_MCI files
copy whatsnew.txt .\setup\M4W_FULL
cd m4w_mci
call make
cd ..

echo ZIP
pkzip M4WDISK.ZIP .\setup\M4W_FULL\*.* -a -ex -p -r
start /w WZIPSE M4WDISK.ZIP -setup -3 -i intl\m4w.ico -t intl\M4W_WZSE.TXT -auto -st"MOD4WIN Install" -C INSTALL.EXE
copy m4wdisk.exe .\setup\m4w24ins.exe
del m4wdisk.exe

echo ZIP
compress CUSTOM\FULL\MOD4WIN.EXE CUSTOM\FULL\MOD4WIN.EX_
pkzip m4wdisk.zip CUSTOM\FULL\MOD4WIN.EX_

start /w WZIPSE M4WDISK.ZIP -setup -3 -i intl\m4w.ico -t intl\M4W_WZSE.TXT -auto -st"MOD4WIN Install" -C INSTALL.EXE
del m4wdisk.zip
copy m4wdisk.exe .\setup\m4w24cin.exe
del m4wdisk.exe

pkzip M4WDISK.ZIP .\setup\MCI_FULL\inst_mci.exe -a -ex
start /w WZIPSE M4WDISK.ZIP -setup -3 -i intl\m4w.ico -t intl\M4W_WZSE.TXT -auto -st"MCI Install" -C INST_MCI.EXE
del m4wdisk.zip
copy m4wdisk.exe .\setup\mci24ins.exe
del m4wdisk.exe

:exit
call cleanup
