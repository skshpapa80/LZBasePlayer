// 작성자: Ray Cho (skshpapa80@gmail.com)
// 프로그래명 : DirectShow 기반으로한 동영상 플레이어
// 작성일 : 2025-07-10
// 수정일 : 2025-07-15
// 블로그 : https://www.raycho12.pe.kr
//
// Lazarus 기반에서 작성되었으며 DirectShow를 사용하여
// 동영상을 재생시키는 간단한 소스 입니다.
// LAV Filter 사용하여 Mp4 등 지원되지 않는 파일 재생되도록 수정
// 자막은 VSFilter 사용

unit umain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	Menus, ComCtrls, IniFiles, udsplayer, DirectShow9, Windows;

type

    { TfrmMain }

    TfrmMain = class(TForm)
        lblCurTime: TLabel;
        lblTotal: TLabel;
        mnuFileOpen: TMenuItem;
        mnuFullScreen: TMenuItem;
        mnuReserveRatio: TMenuItem;
        paBottom: TPanel;
        paScreen: TPanel;
        PopupMenu1: TPopupMenu;
        Separator1: TMenuItem;
        Separator2: TMenuItem;
        Timer1: TTimer;
        TrackBar1: TTrackBar;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
							);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure mnuFileOpenClick(Sender: TObject);
        procedure mnuFullScreenClick(Sender: TObject);
        procedure mnuReserveRatioClick(Sender: TObject);
        procedure paScreenDblClick(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure TrackBar1Change(Sender: TObject);
    private
        PlayerDS : TDSPlayer;
        LockTrack: Boolean;
        bRun : Boolean;
        bFullScreen : Boolean;
        basic_width, basic_height: integer;
        cur_width, cur_height: integer;
        LastBounds: TRect;
        procedure SetVideoSize(fwidth, fheight: integer);
        function SecondToTimeStr(Sec: Double): string;
    public

    end;

var
   frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.SetVideoSize(fwidth, fheight: integer);
begin
    if bFullScreen then begin
        //frmMain.BorderStyle := bsNone;
        paBottom.Visible := false;

        frmMain.BoundsRect := Screen.MonitorFromWindow(frmMain.Handle).BoundsRect;
    end
    else begin
        //frmMain.BorderStyle := bsSizeable;
        paBottom.Visible := true;

        frmMain.Width := fwidth;
        frmMain.height := fheight + paBottom.Height;
    end;
end;

function TfrmMain.SecondToTimeStr(Sec: Double): string;
var
    H, M, S: Integer;
begin
    H := Trunc(Sec) div 3600;
    M := (Trunc(Sec) - H * 3600) div 60;
    S := Trunc(Sec) - H * 3600 - M * 60;
    Result := Format('%d:%d:%d', [H, M, S]);
    // 현재 값을 받아서 시:분:초 로 표시
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
    Sett : TIniFile;
    folder : string;
begin
    Sett := TIniFile.Create('setting.ini');
    folder := Sett.ReadString('CONFIG', 'FILTERFOLRDER', 'D:\Utillity\Filters');
    Sett.Free;

    PlayerDS := TDSPlayer.Create(paScreen.Handle);

    // 필터 폴더 설정
    PlayerDS.FFilterFolder := folder + '\';

    PlayerDS.VideoRenderOrgMethod := frmMain.WindowProc;
    frmMain.WindowProc := @PlayerDS.VideoRenderWndProc;
    frmMain.KeyPreview := true;
    TrackBar1.TabStop := false;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
					Shift: TShiftState);
begin
    if PlayerDS.AvailableDS = false then Exit;

    if Key = VK_SPACE then begin

        if bRun then begin
            bRun := false;
            PlayerDS.Pause;
        end
        else begin
            bRun := true;
            PlayerDS.Run;
        end;
    end;

    if Key = VK_Left then begin
        LockTrack := false;
        PlayerDS.SetMediaPosition(-10);
        LockTrack := true;
    end;

    if Key = VK_Right then begin
        LockTrack := false;
        PlayerDS.SetMediaPosition(10);
        LockTrack := true;
    end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
    if PlayerDS.AvailableDS then
        PlayerDS.Resize(paScreen.Handle);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    PlayerDS.AvailableDS := False;
    frmMain.WindowProc := PlayerDS.VideoRenderOrgMethod;
    PlayerDS.Free;
end;

procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
begin
    with TOpenDialog.Create(Self) do begin
	    Filter := 'Media Files(*.avi;*.mpg;*.wmv;*.mp4;*.mkv)|*.avi;*.mpg;*.wmv;*.mp4;*.mkv|All Files(*.*)|*.*';
        Title := 'Open Media Files..';
	    //Options := [ofAllowMultiSelect];
	    if Execute then begin

            frmMain.Caption := 'LZBasePlayer - ' + ExtractFileName(FileName);
            PlayerDS.ScWidth := paScreen.Width;
            PlayerDS.Scheight := paScreen.Height;

            bRun := true;
            bFullScreen := false;
            PlayerDS.LoadMedia(FileName);

            PlayerDS.GetVideoSize(basic_width, basic_height);
            LastBounds := frmMain.BoundsRect;
            SetVideoSize(basic_width, basic_height);

            TrackBar1.Max := trunc(PlayerDS.MediaLength);
            TrackBar1.Min := 0;

            lblTotal.Caption := Format('%s', [SecondToTimeStr(PlayerDS.MediaLength)]);
	    end;
    end;
end;

procedure TfrmMain.mnuFullScreenClick(Sender: TObject);
begin
    //  풀스크린
    if PlayerDS.AvailableDS then begin
        bFullScreen := not bFullScreen;
        mnuFullScreen.Checked := bFullScreen;
        if bFullScreen = true then begin
            cur_width := paScreen.Width;
            cur_height := paScreen.Height;
        end;
        SetVideoSize(cur_width, cur_height);
    end;
end;

procedure TfrmMain.mnuReserveRatioClick(Sender: TObject);
begin
    mnuReserveRatio.Checked := not mnuReserveRatio.Checked;
    PlayerDS.SetPreserveAspectRatio(mnuReserveRatio.Checked);
end;

procedure TfrmMain.paScreenDblClick(Sender: TObject);
begin
    // 재생 / 멈춤
    if PlayerDS.AvailableDS = false then Exit;

    if bRun = true then begin
        PlayerDS.Pause;
        bRun := false;
    end
    else begin
        PlayerDS.Run;
        bRun := true;
    end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
    CurPos: Double;
begin
    // 타이머처리재생시 트래바 포지션 이동
    if PlayerDS.AvailableDS then Begin
        CurPos := PlayerDS.GetCurPos;
        lblCurTime.Caption := Format('%s', [SecondToTimeStr(CurPos)]);

        LockTrack := true;
        TrackBar1.Position := trunc(CurPos);
        LockTrack := False;
    End;
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
    if PlayerDS.AvailableDS then begin
        if not LockTrack then begin
            PlayerDS.SetCurPos(TrackBar1.Position);
        end;
    end;
end;

end.

