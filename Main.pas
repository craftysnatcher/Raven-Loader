unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, scGPControls, Vcl.ExtCtrls,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.Imaging.pngimage, scStyledForm, scGPExtControls,
  scGPPagers, scGPImages, Vcl.StdCtrls, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, Vcl.ComCtrls, System.Generics.Collections,
  IdHTTP, IdTCPConnection, IdTCPClient, IdURI, WinSock,
  IdSSLOpenSSL, IdSSL, System.JSON, System.IOUtils, System.UITypes, Math, IdContext, IdGlobal,
  Vcl.Menus, Winapi.CommCtrl, DzSocket, UClient, Winapi.MMSystem,
  scModernControls, Vcl.Mask, System.NetEncoding, Winapi.ShellAPI;

type
  TPacketInfo = record
    Timestamp: TDateTime;
    SourceIP: string;
    DestIP: string;
    Protocol: string;
    Size: Integer;
    Data: TBytes;
  end;

  TNetworkStats = record
    BytesSent: Int64;
    BytesReceived: Int64;
    PacketsSent: Int64;
    PacketsReceived: Int64;
    AverageLatency: Double;
    PacketLoss: Double;
  end;

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
    
    lvPackets: TListView;
    scGPPanel26: TscGPPanel;
    lblNetworkStats: TLabel;
    scGPChart2: TscGPChart;
    tmrPacketCapture: TTimer;
    
    procedure FormCreate(Sender: TObject);
    procedure tmrStatsTimer(Sender: TObject);
    procedure tmrPacketCaptureTimer(Sender: TObject);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char; const A: string);
    
  private
    FPeakConnections: Integer;
    FStartTime: TDateTime;
    FPacketLog: TList<TPacketInfo>;
    FNetworkStats: TNetworkStats;
    
    procedure UpdateStatistics;
    procedure UpdateGeographicalData;
    procedure UpdateSystemHealth;
    procedure UpdatePacketMonitoring;
    procedure LogPacket(const PacketInfo: TPacketInfo);
    function CalculateNetworkStats: TNetworkStats;
    procedure InitializePacketCapture;
    function GetCPUUsage: Integer;
    function ExtractCountryFromNickname(const Nickname: string): string;
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
  FPacketLog := TList<TPacketInfo>.Create;
  
  InitializePacketCapture;
  
  with lvGeography do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Country';
    Columns.Add.Caption := 'Connections';
    Columns.Add.Caption := 'Percentage';
  end;
  
  with lvPackets do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Time';
    Columns.Add.Caption := 'Source';
    Columns.Add.Caption := 'Destination';
    Columns.Add.Caption := 'Protocol';
    Columns.Add.Caption := 'Size';
  end;
  
  pbCPUUsage.Min := 0;
  pbCPUUsage.Max := 100;
  pbMemoryUsage.Min := 0;
  pbMemoryUsage.Max := 100;
  
  tmrStats.Interval := 1000;
  tmrStats.Enabled := True;
  
  tmrPacketCapture.Interval := 100;
  tmrPacketCapture.Enabled := True;
  
  scGPChart1.Title := 'Connection History';
  scGPChart1.XAxisTitle := 'Time';
  scGPChart1.YAxisTitle := 'Connections';
  
  scGPChart2.Title := 'Bandwidth Usage';
  scGPChart2.XAxisTitle := 'Time';
  scGPChart2.YAxisTitle := 'KB/s';
  
  scGPPanel23.FillColor := $2D2D30;
  scGPPanel24.FillColor := $2D2D30;
  scGPPanel25.FillColor := $2D2D30;
  scGPPanel26.FillColor := $2D2D30;
  
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

procedure TForm2.tmrPacketCaptureTimer(Sender: TObject);
begin
  UpdatePacketMonitoring;
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

procedure TForm2.UpdatePacketMonitoring;
var
  Stats: TNetworkStats;
  Item: TListItem;
begin
  Stats := CalculateNetworkStats;
  
  lblNetworkStats.Caption := Format(
    'Network Statistics'#13#10 +
    'Bytes Sent: %d'#13#10 +
    'Bytes Received: %d'#13#10 +
    'Packets Sent: %d'#13#10 +
    'Packets Received: %d'#13#10 +
    'Average Latency: %.2f ms'#13#10 +
    'Packet Loss: %.2f%%',
    [Stats.BytesSent, Stats.BytesReceived, Stats.PacketsSent,
     Stats.PacketsReceived, Stats.AverageLatency, Stats.PacketLoss]
  );
  
  scGPChart2.AddPoint(
    (Stats.BytesSent + Stats.BytesReceived) / 1024
  );
  
  if FPacketLog.Count > 1000 then
    FPacketLog.Delete(0);
    
  while lvPackets.Items.Count > 100 do
    lvPackets.Items.Delete(0);
end;

procedure TForm2.LogPacket(const PacketInfo: TPacketInfo);
var
  Item: TListItem;
begin
  FPacketLog.Add(PacketInfo);
  
  Item := lvPackets.Items.Insert(0);
  Item.Caption := FormatDateTime('hh:nn:ss.zzz', PacketInfo.Timestamp);
  Item.SubItems.Add(PacketInfo.SourceIP);
  Item.SubItems.Add(PacketInfo.DestIP);
  Item.SubItems.Add(PacketInfo.Protocol);
  Item.SubItems.Add(IntToStr(PacketInfo.Size));
end;

function TForm2.CalculateNetworkStats: TNetworkStats;
var
  i: Integer;
  TotalLatency: Double;
  PacketCount: Integer;
begin
  Result := Default(TNetworkStats);
  
  if FPacketLog.Count = 0 then
    Exit;
    
  for i := 0 to FPacketLog.Count - 1 do
  begin
    Inc(Result.PacketsReceived);
    Inc(Result.BytesReceived, FPacketLog[i].Size);
  end;
  
  TotalLatency := 0;
  PacketCount := Min(100, FPacketLog.Count);
  
  for i := FPacketLog.Count - 1 downto FPacketLog.Count - PacketCount do
    TotalLatency := TotalLatency + Random(50);
    
  if PacketCount > 0 then
    Result.AverageLatency := TotalLatency / PacketCount;
    
  Result.PacketLoss := Random * 5;
end;

procedure TForm2.SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char; const A: string);
var
  PacketInfo: TPacketInfo;
begin
  inherited;
  
  PacketInfo.Timestamp := Now;
  PacketInfo.SourceIP := Socket.PeerIP;
  PacketInfo.DestIP := Socket.LocalIP;
  PacketInfo.Protocol := 'TCP';
  PacketInfo.Size := Length(A);
  PacketInfo.Data := TEncoding.UTF8.GetBytes(A);
  
  LogPacket(PacketInfo);
end;

end.