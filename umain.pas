// 작성자: Ray Cho (skshpapa80@gmail.com)
// 프로그래명 : DirectShow 기반으로한 동영상 플레이어
// 작성일 : 2025-07-10
// 수정일 : 2025-07-15
// 블로그 : https://www.raycho12.pe.kr
//
// Lazarus 기반에서 작성되었으며 DSPack을 사용하여
// 동영상을 재생시키는 간단한 소스 입니다.
// LAV Filter 사용하여 Mp4 등 지원되지 않는 파일 재생되도록 수정
// 자막은 VSFilter 사용

unit umain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	Menus, DSPack, IniFiles, ActiveX;

type

    { TfrmMain }

    TfrmMain = class(TForm)
        DSScreen: TDSVideoWindowEx2;
        DSTrackBar1: TDSTrackBar;
        FilterGraph1: TFilterGraph;
        lblCur: TLabel;
        lblTotal: TLabel;
		MenuOpen: TMenuItem;
		OpenDialog: TOpenDialog;
        paButton: TPanel;
		PopupMenu1: TPopupMenu;
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
							);
        procedure FormShow(Sender: TObject);
		procedure MenuOpenClick(Sender: TObject);
    private
        ffolder : string;
        PFileName: PWideChar;
        procedure LoadMedia(filename: String);
    public

    end;

var
   frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.LoadMedia(filename: String);
var
    WFileName: Array [0 .. 255] of WideChar;
begin
    CoInitialize(nil);
    try
        StringToWideChar(filename, WFileName, 255);
        PFileName := @WFileName[0];

        // --------------------------------------------------------------------------------------
        // This is a workaround the problem that we don't always get the EC_CLOCK_CHANGED.
        // and because we didn't get the EC_CLOCK_CHANGED the DSTrackbar and DSVideoWindowEx1
        // didn't got reassigned and that returned in misfuntions.
        FilterGraph1.Active := False;
        FilterGraph1.Active := True;
        // --------------------------------------------------------------------------------------

        FilterGraph1.Stop;

        DSScreen.FilterGraph := FilterGraph1;
        DSTrackBar1.FilterGraph := FilterGraph1;

        FilterGraph1.RenderFile(PFileName);
        FilterGraph1.Play;
	finally
        CoUninitialize;
	end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
    Sett : TIniFile;
begin
    Sett := TIniFile.Create('setting.ini');
    ffolder := Sett.ReadString('CONFIG', 'FILTERFOLRDER', 'D:\Utillity\Filters');
    Sett.Free;


end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
					Shift: TShiftState);
begin

end;

procedure TfrmMain.MenuOpenClick(Sender: TObject);
begin
    if OpenDialog.Execute() then begin
        LoadMedia(OpenDialog.FileName);
    end;
end;

end.

