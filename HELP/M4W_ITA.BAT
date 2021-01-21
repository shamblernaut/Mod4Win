@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd ita
:GO
..\helpcomp\hcp.exe modithlp.hpj
echo.
echo.
copy modithlp.hlp ..\m4w_ita.hlp
del modithlp.hlp
cd ..\..
