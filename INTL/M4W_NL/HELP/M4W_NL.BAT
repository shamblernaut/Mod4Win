@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd nl
:GO
..\helpcomp\hcp.exe modnlhlp.hpj
echo.
echo.
copy modnlhlp.hlp ..\m4w_nl.hlp
del modnlhlp.hlp
cd ..\..
