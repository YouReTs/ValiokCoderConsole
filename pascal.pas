unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, FileUtil, LCLType, LCLIntf, BGRABitmap, BGRABitmapTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnDownload: TButton;
    btnOpenImage: TButton;
    btnReplace: TButton;
    btnInfo: TButton;
    cmbMainBackground: TComboBox;
    cmbMenuSettings: TComboBox;
    cmbOther: TComboBox;
    cmbYesNo: TComboBox;
    GroupBox1: TGroupBox;
    ImagePreview: TImage;
    ImageSource: TImage;
    lblFileSelected: TLabel;
    lblMainBackground: TLabel;
    lblMenuSettings: TLabel;
    lblOther: TLabel;
    lblStatus: TLabel;
    lblYesNo: TLabel;
    lblSelectImage: TLabel;
    lblInfo: TLabel;
    lblToggle: TLabel;
    MemoLog: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure btnDownloadClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnOpenImageClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure cmbMainBackgroundChange(Sender: TObject);
    procedure cmbMenuSettingsChange(Sender: TObject);
    procedure cmbOtherChange(Sender: TObject);
    procedure cmbYesNoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblToggleClick(Sender: TObject);
  private
    FSelectedSizeIndex: Integer;
    FIsExpanded: Boolean;
    FResourcesPath: String;
    procedure LoadImageToPreview(const FileName: String);
    procedure UpdateInterface;
    procedure ResizeAndConvertImage(SourceImage: TImage; Width, Height: Integer; 
      out OutputStream: TMemoryStream);
    function GetCurrentSizeIndex: Integer;
    procedure SaveProcessedImage;
    function CheckResourcesFolder: Boolean;
  public
    { public declarations }
  end;

const
  // Размеры изображений
  ImageSizes: array[0..3] of record
    Width: Integer;
    Height: Integer;
    Description: String;
  end = (
    (Width: 853; Height: 480; Description: 'Главный фон'),
    (Width: 256; Height: 480; Description: 'Меню настроек'),
    (Width: 597; Height: 80; Description: 'Прочее'),
    (Width: 217; Height: 37; Description: 'Кнопки Yes/No')
  );

  // Категории файлов (имитация оригинальных строк)
  Categories: array[0..3] of TStringArray = (
    ('Sport', 'Shooter', 'Fighting', '8 bit (roms)', 'Puzzle', 'Adventure',
     'View Sport', 'View Shooter', 'View Fighting', 'View 8 bit', 'View Puzzle',
     'View Adventure', 'Game Menu'),
    ('Return', 'Exit', 'Load', 'Save', 'Screen', 'System'),
    ('Preview 1', 'Preview 2', 'Preview 3', 'Preview 4', 'Preview 5',
     'NTSC', 'PAL', 'AUTO', 'All screen', 'Scale', 'Clear'),
    ('Yes', 'No')
  );

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'PBitmap image changer';
  Width := 566;
  Height := 325;
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  
  // Инициализация компонентов
  FIsExpanded := False;
  FSelectedSizeIndex := 0;
  
  // Настройка ComboBox
  cmbMainBackground.Items.Assign(Categories[0]);
  cmbMainBackground.ItemIndex := 0;
  
  cmbMenuSettings.Items.Assign(Categories[1]);
  cmbMenuSettings.ItemIndex := 0;
  
  cmbOther.Items.Assign(Categories[2]);
  cmbOther.ItemIndex := 0;
  
  cmbYesNo.Items.Assign(Categories[3]);
  cmbYesNo.ItemIndex := 0;
  
  // Проверка папки Resources
  FResourcesPath := ExtractFilePath(Application.ExeName) + 'Resources';
  if not CheckResourcesFolder then
  begin
    ShowMessage('Файл не найден!' + sLineBreak +
                'Переместите программу в папку "Resources" и запустите');
    lblInfo.Caption := 'Переместите программу в папку Resources и запустите*';
  end
  else
  begin
    lblInfo.Caption := 'Запущено из папки "Resources"';
  end;
  
  UpdateInterface;
end;

procedure TMainForm.btnOpenImageClick(Sender: TObject);
begin
  OpenDialog1.Filter := 
    'Image Files|*.jpg;*.jpeg;*.bmp;*.gif;*.png;*.bmp|' +
    'Все типы файлов|*.*|';
  OpenDialog1.Title := 'Выберите изображение для замены';
  
  if OpenDialog1.Execute then
  begin
    try
      ImageSource.Picture.LoadFromFile(OpenDialog1.FileName);
      lblFileSelected.Caption := ExtractFileName(OpenDialog1.FileName);
      MemoLog.Lines.Add('Загружено изображение: ' + OpenDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('Ошибка загрузки изображения: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.btnReplaceClick(Sender: TObject);
var
  StartTime: TDateTime;
  ProcessingTime: Double;
begin
  if ImageSource.Picture.Bitmap.Empty then
  begin
    ShowMessage('Сначала выберите изображение!');
    Exit;
  end;
  
  StartTime := Now;
  
  try
    SaveProcessedImage;
    
    ProcessingTime := (Now - StartTime) * 86400; // конвертация в секунды
    lblStatus.Caption := Format('Ok, %.1f сек', [ProcessingTime]);
    
    MemoLog.Lines.Add('Изображение успешно обработано за ' + 
                      FormatFloat('0.0', ProcessingTime) + ' сек');
  except
    on E: Exception do
    begin
      ShowMessage('Ошибка при обработке: ' + E.Message);
      lblStatus.Caption := 'Ошибка';
    end;
  end;
end;

procedure TMainForm.btnDownloadClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Картинка формата Jpeg|*.jpeg';
  SaveDialog1.FileName := 'image.jpeg';
  
  if SaveDialog1.Execute then
  begin
    try
      if not ImagePreview.Picture.Bitmap.Empty then
      begin
        ImagePreview.Picture.SaveToFile(SaveDialog1.FileName);
        MemoLog.Lines.Add('Изображение сохранено: ' + SaveDialog1.FileName);
      end;
    except
      on E: Exception do
        ShowMessage('Ошибка сохранения: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.btnInfoClick(Sender: TObject);
var
  InfoText: String;
begin
  InfoText := 
    '*Рабочая папка по умолчанию Resources' + sLineBreak +
    '(У вас может называться по другому,' + sLineBreak +
    'главное наличие в ней целевых файлов)' + sLineBreak + sLineBreak +
    'Размер изображений' + sLineBreak +
    ' - Главный фон (853х480)' + sLineBreak +
    ' - Меню настроек (256х480)' + sLineBreak +
    ' - Прочее (597х80)' + sLineBreak +
    ' - Кнопки Yes No (217х37)' + sLineBreak + sLineBreak +
    'Кнопка ">>>>" отображает окно выбора' + sLineBreak +
    'изображения для замены фона меню приставки.' + sLineBreak + sLineBreak +
    'Подходит для замены изображений фона игрового меню для' + sLineBreak +
    ' - Data Frog Y2 SG' + sLineBreak +
    ' - DEXP RetroGAME Flash 2' + sLineBreak +
    ' - DEXP RetroGAME 6' + sLineBreak +
    ' - SG800' + sLineBreak +
    ' - Прочие... (с аналогичной прошивкой)' + sLineBreak + sLineBreak +
    'Автор приложения Flapiotun and ValiokCoder and YouReTs';
    
  ShowMessage(InfoText);
end;

procedure TMainForm.cmbMainBackgroundChange(Sender: TObject);
begin
  FSelectedSizeIndex := 0;
  LoadImageToPreview(cmbMainBackground.Text);
end;

procedure TMainForm.cmbMenuSettingsChange(Sender: TObject);
begin
  FSelectedSizeIndex := 1;
  LoadImageToPreview(cmbMenuSettings.Text);
end;

procedure TMainForm.cmbOtherChange(Sender: TObject);
begin
  FSelectedSizeIndex := 2;
  LoadImageToPreview(cmbOther.Text);
end;

procedure TMainForm.cmbYesNoChange(Sender: TObject);
begin
  FSelectedSizeIndex := 3;
  LoadImageToPreview(cmbYesNo.Text);
end;

procedure TMainForm.lblToggleClick(Sender: TObject);
begin
  FIsExpanded := not FIsExpanded;
  UpdateInterface;
end;

procedure TMainForm.LoadImageToPreview(const FileName: String);
var
  FullPath: String;
  TempImage: TBGRABitmap;
begin
  if not CheckResourcesFolder then Exit;
  
  // Здесь должна быть логика загрузки конкретного файла
  // В примере просто создаем тестовое изображение нужного размера
  try
    TempImage := TBGRABitmap.Create(
      ImageSizes[FSelectedSizeIndex].Width,
      ImageSizes[FSelectedSizeIndex].Height,
      BGRA(80, 80, 80, 255)
    );
    
    // Добавляем текст на изображение для демонстрации
    TempImage.FontHeight := 20;
    TempImage.TextOut(
      TempImage.Width div 2 - 100,
      TempImage.Height div 2 - 10,
      FileName + ' ' + 
      IntToStr(ImageSizes[FSelectedSizeIndex].Width) + 'x' +
      IntToStr(ImageSizes[FSelectedSizeIndex].Height),
      BGRAWhite
    );
    
    ImagePreview.Picture.Bitmap.Assign(TempImage.Bitmap);
    TempImage.Free;
    
    MemoLog.Lines.Add('Просмотр: ' + FileName + ' (' + 
                      IntToStr(ImageSizes[FSelectedSizeIndex].Width) + 'x' +
                      IntToStr(ImageSizes[FSelectedSizeIndex].Height) + ')');
  except
    on E: Exception do
      ShowMessage('Ошибка создания preview: ' + E.Message);
  end;
end;

procedure TMainForm.UpdateInterface;
begin
  if FIsExpanded then
  begin
    Width := 1000;
    lblToggle.Caption := '<<<<';
    
    // Показываем дополнительные компоненты
    GroupBox1.Visible := True;
    MemoLog.Visible := True;
    ImagePreview.Visible := True;
    btnDownload.Visible := True;
  end
  else
  begin
    Width := 566;
    lblToggle.Caption := '>>>>';
    
    // Скрываем дополнительные компоненты
    GroupBox1.Visible := False;
    MemoLog.Visible := False;
    ImagePreview.Visible := False;
    btnDownload.Visible := False;
  end;
end;

procedure TMainForm.ResizeAndConvertImage(SourceImage: TImage; 
  Width, Height: Integer; out OutputStream: TMemoryStream);
var
  Bmp: TBGRABitmap;
  x, y: Integer;
  p: PBGRAPixel;
  ByteArray: array of Byte;
  ByteIndex: Integer;
begin
  OutputStream := TMemoryStream.Create;
  
  try
    // Создаем bitmap нужного размера
    Bmp := TBGRABitmap.Create(Width, Height);
    
    // Масштабируем исходное изображение
    Bmp.StretchPutImage(
      Rect(0, 0, Width, Height),
      SourceImage.Picture.Bitmap,
      dmSet
    );
    
    // Конвертируем в нужный формат (аналог оригинального формата)
    // Формат: RGB + Alpha канал (как в оригинальном коде)
    SetLength(ByteArray, Width * Height * 4);
    ByteIndex := 0;
    
    for y := 0 to Height - 1 do
    begin
      p := Bmp.Scanline[y];
      for x := 0 to Width - 1 do
      begin
        ByteArray[ByteIndex] := p^.red;
        ByteArray[ByteIndex + 1] := p^.green;
        ByteArray[ByteIndex + 2] := p^.blue;
        ByteArray[ByteIndex + 3] := 255; // Alpha канал
        Inc(ByteIndex, 4);
        Inc(p);
      end;
    end;
    
    // Сохраняем в поток
    OutputStream.WriteBuffer(ByteArray[0], Length(ByteArray));
    OutputStream.Position := 0;
    
    Bmp.Free;
    
  except
    OutputStream.Free;
    raise;
  end;
end;

function TMainForm.GetCurrentSizeIndex: Integer;
begin
  Result := FSelectedSizeIndex;
end;

procedure TMainForm.SaveProcessedImage;
var
  OutputStream: TMemoryStream;
  FileName: String;
  SizeIndex: Integer;
begin
  SizeIndex := GetCurrentSizeIndex;
  
  // Создаем обработанное изображение
  ResizeAndConvertImage(
    ImageSource,
    ImageSizes[SizeIndex].Width,
    ImageSizes[SizeIndex].Height,
    OutputStream
  );
  
  try
    // Сохраняем в файл (в оригинале - в папку Resources)
    FileName := FResourcesPath + '\' + 
                GetEnumName(TypeInfo(TComboBox), SizeIndex) + '_' +
                FormatDateTime('yyyymmdd_hhnnss', Now) + '.bin';
    
    OutputStream.SaveToFile(FileName);
    
    MemoLog.Lines.Add('Сохранено: ' + ExtractFileName(FileName) + 
                      ' (' + IntToStr(OutputStream.Size) + ' байт)');
    
    // Обновляем preview
    LoadImageToPreview('processed');
    
  finally
    OutputStream.Free;
  end;
end;

function TMainForm.CheckResourcesFolder: Boolean;
var
  TestFile: String;
begin
  Result := DirectoryExists(FResourcesPath);
  
  if Result then
  begin
    // Проверяем наличие хотя бы одного файла
    TestFile := FResourcesPath + '\test.txt'; // Здесь должна быть ваша логика проверки файлов
    // В реальном приложении проверяйте наличие конкретных файлов ресурсов
  end;
end;

end.
