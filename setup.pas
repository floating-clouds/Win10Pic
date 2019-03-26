unit setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, IniFiles;

type

  { TfrmSetup }

  TfrmSetup = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtSource: TLabeledEdit;
    edtTarget: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FIni: TIniFile;
    FSourceOldDir: string;
    FTargetOldDir: string;
  public
    constructor Create(TheOwner: TComponent; const CfgFile: string); overload;
  end;

var
  frmSetup: TfrmSetup;

implementation

{$R *.lfm}

{ TfrmSetup }

procedure TfrmSetup.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIni);
end;

procedure TfrmSetup.SpeedButton1Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir, True) then begin
    edtSource.Text := Dir;
  end;
end;

procedure TfrmSetup.SpeedButton2Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir, True) then begin
    edtTarget.Text := Dir;
  end;
end;

procedure TfrmSetup.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FIni.WriteString('PicDir', 'SourceDir', edtSource.Text);
  FIni.WriteString('PicDir', 'TargetDir', edtTarget.Text);
  FIni.UpdateFile;
end;

constructor TfrmSetup.Create(TheOwner: TComponent; const CfgFile: string);
begin
  inherited Create(TheOwner);
  FIni := TIniFile.Create(CfgFile);

  FSourceOldDir := FIni.ReadString('PicDir', 'SourceDir', '');
  edtSource.Text := FSourceOldDir;
  FTargetOldDir := FIni.ReadString('PicDir', 'TargetDir', '');
  edtTarget.Text := FTargetOldDir;
end;

end.

