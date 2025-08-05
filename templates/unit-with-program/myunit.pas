unit myunit;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Public procedures and functions
procedure SayHello(const Name: string);
function GetCurrentYear: Integer;

implementation

procedure SayHello(const Name: string);
begin
  WriteLn('Hello, ', Name, '! Welcome to Pascal programming.');
end;

function GetCurrentYear: Integer;
begin
  Result := CurrentYear;
end;

end.