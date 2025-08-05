unit {{PROJECT_NAME}}unit;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

// Public procedures and functions for {{PROJECT_NAME}}
procedure SayHello(const Name: string);
function GetCurrentYear: Integer;

implementation

procedure SayHello(const Name: string);
begin
  WriteLn('Hello, ', Name, '! Welcome to {{PROJECT_NAME}} Pascal programming.');
end;

function GetCurrentYear: Integer;
begin
  Result := YearOf(Now);
end;

end.