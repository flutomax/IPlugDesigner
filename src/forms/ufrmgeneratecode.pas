{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

unit uFrmGenerateCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  uCommon, uIObject, uIDocument;

type

  { TFrmGenerateCode }

  TFrmGenerateCode = class(TForm)
    BpMain: TButtonPanel;
    BtnCopyCode: TButton;
    Editor: TMemo;
    procedure BtnCopyCodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Generate(doc: TIDocument);
  end;

var
  FrmGenerateCode: TFrmGenerateCode;

implementation

{$R *.lfm}

{ TFrmGenerateCode }

procedure TFrmGenerateCode.BtnCopyCodeClick(Sender: TObject);
var
  ss, sl: integer;
begin
  ss := Editor.SelStart;
  sl := Editor.SelLength;
  try
    Editor.SelectAll;
    Editor.CopyToClipboard;
  finally
    Editor.SelStart := ss;
    Editor.SelLength := sl;
  end;
end;

procedure TFrmGenerateCode.FormCreate(Sender: TObject);
begin
  BtnCopyCode.SetBounds(0, 8, 83, 26);
{$IFDEF MSWINDOWS}
  Editor.Font.Name := 'Consolas';
{$ENDIF}
end;

procedure TFrmGenerateCode.Generate(doc: TIDocument);

  procedure AddStyles;
  var
    i: integer;
  begin
    for i := 0 to doc.StyleCache.StylesCount - 1 do
      doc.StyleCache[i].GetCodeStr(Editor.Lines);
  end;

  procedure AddObject(obj: TIObject);
  var
    i: integer;
    s: string;
    g: ICodeGenerate;
  begin
    for i := 0 to obj.ChildsCount - 1 do
      AddObject(obj.Childs[i]);
    if Supports(obj, ICodeGenerate, g) then
    begin
      s := g.GetCodeStr;
      s := Format(#9#9'pGraphics->AttachControl(new %s);', [s]);
      Editor.Lines.Add(s);
    end;
  end;

begin
  Editor.Lines.BeginUpdate;
  try
    doc.ImageCache.GetCodeStr(Editor.Lines, false);
    doc.SVGCache.GetCodeStr(Editor.Lines, true);
    if doc.VectorControlsUsed then
      AddStyles;
    Editor.Lines.Add(#9'mLayoutFunc = [&](IGraphics* pGraphics)');
    Editor.Lines.Add(#9'{');
    AddObject(doc.RootObject);
    Editor.Lines.Add(#9'}');
  finally
    Editor.Lines.EndUpdate;
  end;
end;

end.


