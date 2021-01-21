USES WinCrt,MakeCode;
CONST Result  : Array[0..40]OF Char=#0;
      Name    : Array[0..40]OF Char='Kay Bruns'#0;
      Compn   : Array[0..40]OF Char=''#0;
      PayMent : Array[0..40]OF Char=''#0;
BEGIN
  GenerateCode('2.3',Name,CompN,PayMent,Result);
  Writeln(Result);
  GenerateCode1('2.3',Name,CompN,PayMent,Result);
  Writeln(Result);
END.