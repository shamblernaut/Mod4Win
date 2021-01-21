@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
rlink MOD4WIN.FXE
rlink -feFULL\MOD4WIN.EXE MOD4WIN.RES MOD4WIN.FXE
CheckSum FULL\MOD4WIN.EXE
