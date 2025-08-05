program {{PROJECT_NAME}};
{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  {{PROJECT_NAME}}unit;

begin
  WriteLn('{{PROJECT_NAME}} - Program with Unit Example');
  WriteLn('Created by {{USER}} on {{DATE}}');
  
  // Call function from our unit
  SayHello('{{USER}}');
  
  WriteLn('Current year: ', GetCurrentYear);
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.