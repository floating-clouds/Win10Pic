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
    procedure edtSourceChange(Sender: TObject);
    procedure edtTargetChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
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
    Button1.Enabled := not FSourceOldDir.Equals(Dir);
  end;
end;

procedure TfrmSetup.SpeedButton2Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir, True) then begin
    edtTarget.Text := Dir;
    Button1.Enabled := not FTargetOldDir.Equals(Dir);
  end;
end;

procedure TfrmSetup.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;

  if ModalResult = mrOK then begin
    if not DirectoryExists(edtSource.Text) then begin
      ShowMessage('Source folder not exists !');
      Exit;
    end;
    if not DirectoryExists(edtTarget.Text) then begin
      ShowMessage('Target folder not exists !');
      Exit;
    end;

    FIni.WriteString('PicDir', 'SourceDir', edtSource.Text);
    FIni.WriteString('PicDir', 'TargetDir', edtTarget.Text);
    FIni.UpdateFile;
  end;

  CanClose := True;
end;

procedure TfrmSetup.edtTargetChange(Sender: TObject);
begin
  Button1.Enabled := not FTargetOldDir.Equals(edtTarget.Text);
end;

procedure TfrmSetup.edtSourceChange(Sender: TObject);
begin
  Button1.Enabled := not FSourceOldDir.Equals(edtSource.Text);
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

