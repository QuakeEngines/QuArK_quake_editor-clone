unit QkFormCfg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, QkObjects, QkFileObjects, TB97, QkFormVw, QkQuakeCtx;

type
 QInternal = class(QFormObject)
             public
               class function TypeInfo: String; override;
               function GetConfigStr1: String; override;
               procedure ObjectState(var E: TEtatObjet); override;
             end;
 QFormContext = class(QQuakeCtx)
             protected
               function GetConfigStr1: String; override;
             public
               class function TypeInfo: String; override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
               procedure ObjectState(var E: TEtatObjet); override;
             end;
 QFormCfg = class(QFormObject)
            public
              class function TypeInfo: String; override;
              function GetConfigStr1: String; override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure ObjectState(var E: TEtatObjet); override;
            end;

implementation

uses QkUnknown, Undo, TbPalette, Toolbar1, ToolBox1,
     Setup, QkInclude, QkMacro, QkImages, QkTextures,
     Python, Quarkx, PyMacros, PyToolbars, PyForms,
     QkPixelSet, QkObjectClassList;

class function QFormCfg.TypeInfo: String;
begin
 TypeInfo:=':form';
end;

function QFormCfg.GetConfigStr1: String;
begin
 Result:='FormCFG';
end;

procedure QFormCfg.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiForm;
end;

function QFormCfg.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
  Result:=ieResult[Q is QInternal];
end;

 {------------------------}

class function QInternal.TypeInfo;
begin
  Result:=':';
end;

function QInternal.GetConfigStr1: String;
begin
 Result:='SpecArgForm';
end;

procedure QInternal.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiFormElement;
end;

 {------------------------}

class function QFormContext.TypeInfo;
begin
 TypeInfo:='.fctx';
end;

function QFormContext.GetConfigStr1: String;
begin
 Result:='FormContext';
end;

function QFormContext.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
  Result:=ieResult[(Q is QFormCfg)]
end;

class procedure QFormContext.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5179);
{Info.FileExt:=779;
 Info.WndInfo:=[wiWindow];}
end;

procedure QFormContext.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiFormContext;
end;

initialization
  RegisterQObject(QFormCfg, 'a');
  RegisterQObject(QInternal, 'a');
  RegisterQObject(QFormContext, 'a');
end.
