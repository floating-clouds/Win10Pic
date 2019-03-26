unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus;

type
  TOnFileEvent = procedure (const Dir: string; const SearchRec: TSearchRec) of object;

  { TformMain }

  TformMain = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    FPicDir: string;
    FSourceDir: string;
    FTargetDir: string;

    FCfgFile: string;

    procedure DoEnumFile(const Dir: string; const OnFile: TOnFileEvent);

    procedure InitListView;
    procedure ClearListView;
    procedure AddListViewItem(const Dir: string; const SearchRec: TSearchRec);

    procedure CopyFile(const Dir: string; const SearchRec: TSearchRec);

    procedure LoadConfig;
  end;

var
  formMain: TformMain;

implementation

{$R *.lfm}

{ TformMain }

uses windirs, IniFiles, setup, FPimage;

procedure TformMain.FormCreate(Sender: TObject);
begin
  // todo check the windows version
  LoadConfig;
  FPicDir := FSourceDir;
  InitListView;
end;

procedure TformMain.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
begin
  if Selected then
  begin
    try
      Image1.Picture.Graphic.LoadFromFile(IncludeTrailingPathDelimiter(FPicDir) + Item.Caption);
      Image1.Invalidate;
      StatusBar1.Panels.Items[1].Text := Image1.Picture.Graphic.MimeType;
    except
      on E: FPImageException do begin
         Image1.Picture.Graphic.Clear;
         ShowMessage(E.Message);
      end;
    end;
  end;
end;

procedure TformMain.MenuItem1Click(Sender: TObject);
begin
  if not TMenuItem(Sender).Checked then
  begin
    if Sender.Equals(MenuItem1) then
    begin
      MenuItem1.Checked := True;
      MenuItem2.Checked := False;
      FPicDir := FSourceDir;
    end else
    begin
      MenuItem1.Checked := False;
      MenuItem2.Checked := True;
      FPicDir := FTargetDir;
    end;
    InitListView;
  end;
end;

procedure TformMain.ToolButton1Click(Sender: TObject);
begin
  DoEnumFile(FSourceDir, @CopyFile);
end;

procedure TformMain.ToolButton2Click(Sender: TObject);
begin
  InitListView;
end;

procedure TformMain.ToolButton4Click(Sender: TObject);
var
  MR: Integer;
begin
  with TfrmSetup.Create(Self, FCfgFile) do begin
    MR := ShowModal;
    Free;
  end;

  if MR = mrOK then begin
    LoadConfig;
    if MenuItem1.Checked then
    begin
      FPicDir := FSourceDir;
    end else
    begin
      FPicDir := FTargetDir;
    end;
    InitListView;
  end;
end;

procedure TformMain.ClearListView;
var
  I: integer;
  Item: TListItem;
begin
  for I := ListView1.Items.Count - 1 downto 0 do
  begin
    Item := ListView1.Items[I];
    Item.Delete;
  end;
end;

procedure TformMain.AddListViewItem(const Dir: string; const SearchRec: TSearchRec);
var
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Caption := SearchRec.Name;
end;

procedure TformMain.InitListView;
begin
  ListView1.Items.BeginUpdate;
  try
    ClearListView;
    DoEnumFile(FPicDir, @AddListViewItem);
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TformMain.DoEnumFile(const Dir: string; const OnFile: TOnFileEvent);
var
  DirTmp: string;
  SearchRec: TSearchRec;
  DosError: longint;
begin
  DirTmp := IncludeTrailingPathDelimiter(Dir);
  DosError := FindFirst(DirTmp + '*.*', faArchive, SearchRec);
  try
    while (DosError = 0) do
    begin
      if (SearchRec.Size > 102400) and Assigned(OnFile) then
      begin
        OnFile(DirTmp, SearchRec);
      end;
      DosError := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TformMain.CopyFile(const Dir: string; const SearchRec: TSearchRec);
var
  Target, Source: string;
  SourceStream, TargetStream: TFileStream;
begin
  Target := IncludeTrailingPathDelimiter(FTargetDir) + SearchRec.Name + '.jpeg';
  if not FileExists(Target) then
  begin
    Source := Dir + SearchRec.Name;

    SourceStream := TFileStream.Create(Source, fmOpenRead + fmShareDenyNone);
    try
      TargetStream := TFileStream.Create(Target, fmCreate + fmShareExclusive);
      try
        TargetStream.CopyFrom(SourceStream, SourceStream.Size);
      finally
        FreeAndNil(TargetStream);
      end;
    finally
      FreeAndNil(SourceStream);
    end;
  end;
end;

procedure TformMain.LoadConfig;
var
  Ini: TIniFile;
  SourceDir, TargetDir: string;

  function ReadString(const Key: string; const DefaultValue: string): string;
  begin
    Result := Ini.ReadString('PicDir', key, '');
    if Result.IsEmpty then begin
      Result := DefaultValue;
      Ini.WriteString('PicDir', key, DefaultValue);
      Ini.UpdateFile;
    end;
  end;

begin
  SourceDir := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA, False)
      + 'Packages\Microsoft.Windows.ContentDeliveryManager_cw5n1h2txyewy\LocalState\Assets';
  TargetDir := GetWindowsSpecialDir(CSIDL_MYPICTURES, False);

  FCfgFile := ChangeFileExt(Application.ExeName, '.cfg');
  Ini := TIniFile.Create(FCfgFile);
  try
    FSourceDir := ReadString('SourceDir', SourceDir);
    FTargetDir := ReadString('TargetDir', TargetDir);
  finally
    FreeAndNil(Ini);
  end;
end;

end.
