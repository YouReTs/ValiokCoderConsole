unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, FileUtil, LCLType, LCLIntf, Math;

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
    ScrollBox1: TScrollBox;
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
    function CreateTestBitmap(Width, Height: Integer; Text: String): TBitmap;
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

  // Категории файлов
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
  if cmbMainBackground.Items.Count > 0 then
    cmbMainBackground.ItemIndex := 0;
  
  cmbMenuSettings.Items.Assign(Categories[1]);
  if cmbMenuSettings.Items.Count > 0 then
    cmbMenuSettings.ItemIndex := 0;
  
  cmbOther.Items.Assign(Categories[2]);
  if cmbOther.Items.Count > 0 then
    cmbOther.ItemIndex := 0;
  
  cmbYesNo.Items.Assign(Categories[3]);
  if cmbYesNo.Items.Count > 0 then
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
    'Image Files|*.jpg;*.jpeg;*.bmp;*.gif;*.png|' +
    'Все типы файлов|*.*';
  OpenDialog1.Title := 'Выберите изображение для замены';
  
  if OpenDialog1.Execute then
  begin
    try
      ImageSource.Picture.LoadFromFile(OpenDialog1.FileName);
      lblFileSelected.Caption := ExtractFileName(OpenDialog1.FileName);
      lblFileSelected.Font.Color := clGreen;
      
      if Assigned(MemoLog) then
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
  if not Assigned(ImageSource.Picture) or ImageSource.Picture.Bitmap.Empty then
  begin
    ShowMessage('Сначала выберите изображение!');
    Exit;
  end;
  
  StartTime := Now;
  
  try
    SaveProcessedImage;
    
    ProcessingTime := (Now - StartTime) * 86400; // конвертация в секунды
    if Assigned(lblStatus) then
      lblStatus.Caption := Format('Ok, %.1f сек', [ProcessingTime]);
    
    if Assigned(MemoLog) then
      MemoLog.Lines.Add('Изображение успешно обработано за ' + 
                       FormatFloat('0.0', ProcessingTime) + ' сек');
  except
    on E: Exception do
    begin
      ShowMessage('Ошибка при обработке: ' + E.Message);
      if Assigned(lblStatus) then
        lblStatus.Caption := 'Ошибка';
    end;
  end;
end;

procedure TMainForm.btnDownloadClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Картинка формата Jpeg|*.jpeg';
  SaveDialog1.FileName := 'image.jpeg';
  SaveDialog1.DefaultExt := 'jpeg';
  
  if SaveDialog1.Execute then
  begin
    try
      if Assigned(ImagePreview.Picture) and not ImagePreview.Picture.Bitmap.Empty then
      begin
        ImagePreview.Picture.SaveToFile(SaveDialog1.FileName);
        if Assigned(MemoLog) then
          MemoLog.Lines.Add('Изображение сохранено: ' + SaveDialog1.FileName);
        ShowMessage('Изображение сохранено успешно!');
      end
      else
      begin
        ShowMessage('Нет изображения для сохранения!');
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

function TMainForm.CreateTestBitmap(Width, Height: Integer; Text: String): TBitmap;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    Bmp.PixelFormat := pf24bit;
    
    // Заливаем фон
    Bmp.Canvas.Brush.Color := clGray;
    Bmp.Canvas.FillRect(0, 0, Width, Height);
    
    // Рисуем рамку
    Bmp.Canvas.Pen.Color := clWhite;
    Bmp.Canvas.Rectangle(0, 0, Width, Height);
    
    // Рисуем текст
    Bmp.Canvas.Font.Color := clWhite;
    Bmp.Canvas.Font.Size := 12;
    Bmp.Canvas.Font.Style := [fsBold];
    Bmp.Canvas.TextOut(
      Width div 2 - Bmp.Canvas.TextWidth(Text) div 2,
      Height div 2 - Bmp.Canvas.TextHeight(Text) div 2,
      Text
    );
    
    Result := Bmp;
  except
    Bmp.Free;
    raise;
  end;
end;

procedure TMainForm.LoadImageToPreview(const FileName: String);
var
  TempBitmap: TBitmap;
  SizeText: String;
begin
  if not CheckResourcesFolder then 
  begin
    // Если папки нет, просто создаем тестовое изображение
    SizeText := FileName + ' (' + 
                IntToStr(ImageSizes[FSelectedSizeIndex].Width) + 'x' +
                IntToStr(ImageSizes[FSelectedSizeIndex].Height) + ')';
    
    TempBitmap := CreateTestBitmap(
      ImageSizes[FSelectedSizeIndex].Width,
      ImageSizes[FSelectedSizeIndex].Height,
      SizeText
    );
    
    try
      ImagePreview.Picture.Assign(TempBitmap);
    finally
      TempBitmap.Free;
    end;
    
    Exit;
  end;
  
  // Здесь можно добавить реальную загрузку файлов из Resources
  // В данном примере создаем тестовое изображение
  SizeText := FileName + ' (' + 
              IntToStr(ImageSizes[FSelectedSizeIndex].Width) + 'x' +
              IntToStr(ImageSizes[FSelectedSizeIndex].Height) + ')';
  
  TempBitmap := CreateTestBitmap(
    ImageSizes[FSelectedSizeIndex].Width,
    ImageSizes[FSelectedSizeIndex].Height,
    SizeText
  );
  
  try
    ImagePreview.Picture.Assign(TempBitmap);
    
    if Assigned(MemoLog) then
      MemoLog.Lines.Add('Просмотр: ' + FileName + ' (' + 
                       IntToStr(ImageSizes[FSelectedSizeIndex].Width) + 'x' +
                       IntToStr(ImageSizes[FSelectedSizeIndex].Height) + ')');
  finally
    TempBitmap.Free;
  end;
end;

procedure TMainForm.UpdateInterface;
begin
  if FIsExpanded then
  begin
    Width := 1000;
    Height := 600;
    lblToggle.Caption := '<<<<';
    lblToggle.Font.Color := clRed;
    
    // Показываем дополнительные компоненты
    if Assigned(GroupBox1) then GroupBox1.Visible := True;
    if Assigned(MemoLog) then MemoLog.Visible := True;
    if Assigned(ImagePreview) then ImagePreview.Visible := True;
    if Assigned(btnDownload) then btnDownload.Visible := True;
  end
  else
  begin
    Width := 566;
    Height := 325;
    lblToggle.Caption := '>>>>';
    lblToggle.Font.Color := clBlue;
    
    // Скрываем дополнительные компоненты
    if Assigned(GroupBox1) then GroupBox1.Visible := False;
    if Assigned(MemoLog) then MemoLog.Visible := False;
    if Assigned(ImagePreview) then ImagePreview.Visible := False;
    if Assigned(btnDownload) then btnDownload.Visible := False;
  end;
end;

procedure TMainForm.ResizeAndConvertImage(SourceImage: TImage; 
  Width, Height: Integer; out OutputStream: TMemoryStream);
var
  TempBitmap: TBitmap;
  x, y: Integer;
  P: PByte;
  Pixel: TRGBTriple;
  ByteArray: array of Byte;
  ByteIndex: Integer;
begin
  OutputStream := TMemoryStream.Create;
  
  try
    // Создаем временный bitmap для ресайза
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.Width := Width;
      TempBitmap.Height := Height;
      TempBitmap.PixelFormat := pf24bit;
      
      // Масштабируем изображение
      TempBitmap.Canvas.StretchDraw(
        Rect(0, 0, Width, Height),
        SourceImage.Picture.Bitmap
      );
      
      // Подготавливаем массив байт
      SetLength(ByteArray, Width * Height * 4);
      ByteIndex := 0;
      
      // Конвертируем в формат RGB + Alpha (как в оригинале)
      for y := 0 to Height - 1 do
      begin
        P := TempBitmap.ScanLine[y];
        for x := 0 to Width - 1 do
        begin
          Pixel := PRGBTriple(P)^;
          
          // Сохраняем в формате RGB + Alpha
          ByteArray[ByteIndex] := Pixel.rgbtRed;
          ByteArray[ByteIndex + 1] := Pixel.rgbtGreen;
          ByteArray[ByteIndex + 2] := Pixel.rgbtBlue;
          ByteArray[ByteIndex + 3] := 255; // Alpha канал
          
          Inc(ByteIndex, 4);
          Inc(P, 3); // Переходим к следующему пикселю (24 бита = 3 байта)
        end;
      end;
      
      // Сохраняем в поток
      OutputStream.WriteBuffer(ByteArray[0], Length(ByteArray));
      OutputStream.Position := 0;
      
    finally
      TempBitmap.Free;
    end;
    
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
    // Сохраняем в файл
    FileName := FResourcesPath + PathDelim + 
                'processed_' + 
                IntToStr(SizeIndex) + '_' +
                FormatDateTime('yyyymmdd_hhnnss', Now) + '.bin';
    
    // Создаем папку если её нет
    ForceDirectories(ExtractFilePath(FileName));
    
    OutputStream.SaveToFile(FileName);
    
    if Assigned(MemoLog) then
      MemoLog.Lines.Add('Сохранено: ' + ExtractFileName(FileName) + 
                       ' (' + IntToStr(OutputStream.Size) + ' байт)');
    
    // Обновляем preview
    LoadImageToPreview('processed');
    
    ShowMessage('Изображение успешно сохранено в:' + sLineBreak + FileName);
    
  finally
    OutputStream.Free;
  end;
end;

function TMainForm.CheckResourcesFolder: Boolean;
begin
  Result := DirectoryExists(FResourcesPath);
  
  if not Result then
  begin
    // Пытаемся создать папку
    if ForceDirectories(FResourcesPath) then
    begin
      Result := True;
      if Assigned(MemoLog) then
        MemoLog.Lines.Add('Создана папка Resources');
    end;
  end
  else
  begin
    if Assigned(MemoLog) then
      MemoLog.Lines.Add('Папка Resources найдена');
  end;
end;

end.
