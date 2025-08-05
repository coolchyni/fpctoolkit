program {{PROJECT_NAME}};
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

begin
  WriteLn('Hello from {{PROJECT_NAME}}!');
  WriteLn('Created on: {{DATE}} at {{TIME}}');
  WriteLn('Author: {{USER}}');
  
  // Add your code here
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.