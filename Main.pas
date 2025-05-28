unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, scGPControls, Vcl.ExtCtrls,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.Imaging.pngimage, scStyledForm, scGPExtControls,
  scGPPagers, scGPImages, Vcl.StdCtrls, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, Vcl.ComCtrls, System.Generics.Collections,
  IdHTTP, IdTCPConnection, IdTCPClient, IdURI,
  IdSSLOpenSSL, IdSSL, System.JSON, System.IOUtils, System.UITypes, Math, IdContext, IdGlobal,
  Vcl.Menus, Winapi.CommCtrl, DzSocket, UClient, Winapi.MMSystem,
  scModernControls, Vcl.Mask, System.NetEncoding, Winapi.ShellAPI;

type
  TForm2 = class(TForm)
    scGPPanel23: TscGPPanel;
    lblTotalConnections: TLabel;
    lblActiveConnections: TLabel;
    lblPeakConnections: TLabel;
    lblUptime: TLabel;
    scGPChart1: TscGPChart;
    scGPPanel24: TscGPPanel;
    lvGeography: TListView;
    scGPPanel25: TscGPPanel;
    pbCPUUsage: TProgressBar;
    pbMemoryUsage: TProgressBar;
    lblCPUUsage: TLabel;
    lblMemoryUsage: TLabel;
    tmrStats: TTimer;
    
    procedure FormCreate(Sender: TObject);
    procedure tmrStatsTimer(Sender: TObject);
    
  private
    FPeakConnections: Integer;
    FStartTime: TDateTime;
    procedure UpdateStatistics;
    procedure UpdateGeographicalData;
    procedure UpdateSystemHealth;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  inherited;
  
  FPeakConnections := 0;
  FStartTime := Now;
  
  with lvGeography do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Country';
    Columns.Add.Caption := 'Connections';
    Columns.Add.Caption := 'Percentage';
  end;
  
  pbCPUUsage.Min := 0;
  pbCPUUsage.Max := 100;
  pbMemoryUsage.Min := 0;
  pbMemoryUsage.Max := 100;
  
  tmrStats.Interval := 1000;
  tmrStats.Enabled := True;
  
  scGPChart1.Title := 'Connection History';
  scGPChart1.XAxisTitle := 'Time';
  scGPChart1.YAxisTitle := 'Connections';
  
  scGPPanel23.FillColor := $2D2D30;
  scGPPanel24.FillColor := $2D2D30;
  scGPPanel25.FillColor := $2D2D30;
  
  scGPPageControl1.TabIndex := 1;
  SetupListView;
  StartMonitoring;
end;

procedure TForm2.tmrStatsTimer(Sender: TObject);
begin
  UpdateStatistics;
  UpdateGeographicalData;
  UpdateSystemHealth;
end;

procedure TForm2.UpdateStatistics;
var
  CurrentConnections: Integer;
begin
  CurrentConnections := L.Items.Count;
  
  if CurrentConnections > FPeakConnections then
    FPeakConnections := CurrentConnections;
    
  lblTotalConnections.Caption := Format('Total Connections: %d', [S.ConnectionCount]);
  lblActiveConnections.Caption := Format('Active Connections: %d', [CurrentConnections]);
  lblPeakConnections.Caption := Format('Peak Connections: %d', [FPeakConnections]);
  lblUptime.Caption := Format('Uptime: %s', 
    [FormatDateTime('dd days hh:nn:ss', Now - FStartTime)]);
    
  scGPChart1.AddPoint(CurrentConnections);
end;

procedure TForm2.UpdateGeographicalData;
var
  CountryStats: TDictionary<string, Integer>;
  Country: string;
  Count: Integer;
  Item: TListItem;
  TotalConnections: Integer;
begin
  CountryStats := TDictionary<string, Integer>.Create;
  try
    for var i := 0 to L.Items.Count - 1 do
    begin
      Country := ExtractCountryFromNickname(L.Items[i]);
      if CountryStats.ContainsKey(Country) then
        CountryStats[Country] := CountryStats[Country] + 1
      else
        CountryStats.Add(Country, 1);
    end;
    
    lvGeography.Items.BeginUpdate;
    try
      lvGeography.Items.Clear;
      TotalConnections := L.Items.Count;
      
      for Country in CountryStats.Keys do
      begin
        Count := CountryStats[Country];
        Item := lvGeography.Items.Add;
        Item.Caption := Country;
        Item.SubItems.Add(IntToStr(Count));
        if TotalConnections > 0 then
          Item.SubItems.Add(Format('%.1f%%', [Count / TotalConnections * 100]))
        else
          Item.SubItems.Add('0%');
      end;
    finally
      lvGeography.Items.EndUpdate;
    end;
  finally
    CountryStats.Free;
  end;
end;

procedure TForm2.UpdateSystemHealth;
var
  MemStatus: TMemoryStatusEx;
  SysInfo: TSystemInfo;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  pbMemoryUsage.Position := Round(MemStatus.dwMemoryLoad);
  lblMemoryUsage.Caption := Format('Memory Usage: %d%%', [pbMemoryUsage.Position]);
  
  pbCPUUsage.Position := GetCPUUsage;
  lblCPUUsage.Caption := Format('CPU Usage: %d%%', [pbCPUUsage.Position]);
end;

end.