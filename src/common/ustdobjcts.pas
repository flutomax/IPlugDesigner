unit uStdObjcts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, Graphics, SysUtils, uCommon, uTree, uStorage, uIObject;

type

  { TIStdText }

  TIStdText = class(TIObject)
  protected
    //function GetAutoSize: Boolean; override;
    class function GetObjectName: String; override;
  end;

implementation

{ TIStdText }


class function TIStdText.GetObjectName: String;
begin
  Result:=inherited GetObjectName;
end;

end.

