program main;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  myunit;

begin
  WriteLn('Program with Unit Example');
  WriteLn('Created by {{USER}} on {{DATE}}');
  
  // Call function from our unit
  SayHello('{{USER}}');
  
  WriteLn('Current year: ', GetCurrentYear);
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.