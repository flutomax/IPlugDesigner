{
 This file is part of the IPlugDesigner. Copyright (C) the IPlugDesigner developer.
 See LICENSE.txt for more info.
}

program IPlugDesigner;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  clocale,
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, uStorage, uCommon, uTree, uIDesigner,
  uIDocument, uIObject, uGraphics, uStandartIControls,
  uMRUList, uIniFileEx, uIMiscControls, uLayersInspector,
  uObjectInspector, uListViewUtils, uColorControls, uClipboard,
  uCommands, uHistory, uIPlugPropEdits, uVectorControls,
  // Forms
  uFrmMain, uFrmDocProperties, uFrmInputText, uFrmImages, uFrmSelectImage,
  uFrmStyles, uFrmGenerateCode, uFrmColorDialog, uFrmFontDialog, uBmpReader,
  uFrmFauIcons, uFauIcons, uIVKeyboardControl, uSVGControls, uAGGCanvas,
  uFrmSVG, uFrmSelectSVG, uSVGObjects, uSVGTypes, uSVGPath, uSVGFills, uSVGProc,
  uFrmSelectSVGs, uBitmapControls, uFileAssoc, uFrmPreferences, uFrmAbout;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.



