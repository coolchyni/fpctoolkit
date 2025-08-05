program main;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

begin
  WriteLn('Hello from Simple Console App!');
  WriteLn('Created on: {{DATE}} at {{TIME}}');
  WriteLn('Author: {{USER}}');
  
  // Add your code here
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.