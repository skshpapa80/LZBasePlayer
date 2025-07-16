unit udsplayer;

{$mode ObjFPC}{$H+}

interface

uses
    Windows, Messages, Classes, SysUtils, ActiveX, DirectShow9, Graphics;

const
    WM_GRAPHEVENT                   = WM_APP + 100;
    CLSID_LAVSplitterSource: TGUID  = '{B98D13E7-55DB-4385-A33D-09FD1BA26338}';
    CLSID_LAVAudioDecoder: TGUID    = '{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}';
    CLSID_LAVVideoDecoder: TGUID    = '{EE30215D-164F-4A92-A4EB-9D4C13390F9F}';
    CLSID_VSFIlter: TGUID           = '{9852A670-F845-491B-9BE6-EBD841B8A613}';
    IID_IPersistStream : TGUID      = '{00000109-0000-0000-C000-000000000046}';

type
    TStreamType                     = (stAudio, stVideo, stMiscellaneous);
    TPlayerState                    = (psNotReady, psPlaying, psPaused, psStopped);  // psStandby -> psNotReady
    TWorkingVR                      = (NoWorkingVR, EVR, VMR9, VMR7, madVR{$IFDEF INCLUDE_EVRCP}, EVRCP{$ENDIF});
    TVRenderer                      = (vrAuto, vrEVR, vrVMR9, vrVMR7, vrmadVR{$IFDEF INCLUDE_EVRCP}, vrEVRCP{$ENDIF});
    TRotationType                   = (ROTATE_NONE, ROTATE_90, ROTATE_180, ROTATE_270, FLIP_HORZ, FLIP_VERT);
    TPlayerEvent                    = (PlayEnded, ErrorEnded);
    CBEventNotice                   = procedure(Event: TPlayerEvent); stdcall;

    TStreamInfo = record
        streamType: TStreamType;
        ppmt: PAMMEDIATYPE;
        pdwFlags: Cardinal;
        plcid: Cardinal;
        pdwGroup: Cardinal;
        ppszName: PWideChar;
        ppObject: IInterface;
        ppUnk: IInterface;
    end;

	{ TDSPlayer }

    TDSPlayer = Class
        constructor Create(Handle: HWND);
        destructor Destroy; override;
    private
        DsHwnd: HWND;
        { DShow 변수}
        GraphBuilder        : IGraphBuilder;    // 필터그래프
        FilterGraph2        : IFilterGraph2;
        MediaControl        : IMediaControl;    // 동영상 재생, 정지, 제어
        MediaEvent          : IMediaEventEx;    // DSHOW 이벤트 제어
        MediaSeeking        : IMediaSeeking;    // 동영상 길이 정보 제어
        MediaPosition       : IMediaPosition;   // 동영상 재생 위치

        VMR9BaseFilter      : IBaseFilter;
        VMR9FilterConfig    : IVMRFilterConfig9;
        m_pWindowless       : IVMRWindowlessControl9;
        VMR9MixerBitmap     : IVMRMixerBitmap9;
        VMR9MonitorConfig   : IVMRMonitorConfig9;

        LAVAudioDecoder     : IBaseFilter;
        LAVVideoDecoder     : IBaseFilter;
        LAVSplitter         : IBaseFilter;
        VSFilter            : IBaseFilter;

        LibSplitterHandle   : THandle;
        LibAudioHandle      : THandle;
        LibVideoHandle      : THandle;
        LibVSHandle         : THandle;

        bKeepAspectRatio    : BOOL;
        FStreamInfo         : array of TStreamInfo;
        FNumStreams         : Cardinal;
        FNumVideoStream     : Cardinal;
        FNumAudioStream     : Cardinal;
        FNumMiscStream      : Cardinal;
        FVideoStreamIndex   : integer;
        FAudioStreamIndex   : integer;

        FPlayerState        : TPlayerState;

        StreamSelector      : IAMStreamSelect;

        function BuildLAVFilters: BOOL;
        function BuildSoundRenderer: BOOL;

        procedure GetMediaStreamInfo;
    public
        alphaBitmap         : VMR9AlphaBitmap;
        ScWidth, Scheight   : LongInt;
        srcFilter           : IBaseFilter;
        FFilterFolder       : string;
        AvailableDS         : Boolean;
        DirectSoundFilter   : IBaseFilter;
        MediaLength         : Double;
        // 서브 클래싱용
        VideoRenderOrgMethod: TWndMethod;
        procedure VideoRenderWndProc(var Msg: TMessage);

        Function SetupDs: Boolean;
        Function ShutDownDs: Boolean;

        Function LoadMedia(FileName: String): Boolean;
        Function GetCurPos: Double;
        Procedure SetCurPos(Pos : Double);

        Procedure Run;
        procedure Pause;
        procedure Resize(MediaWindow: HWND);
        procedure GetVideoSize(var Width, Height: integer);
        procedure SetPreserveAspectRatio(bPreserve: BOOL);
        function GetStreamInfo(StreamType: TStreamType; index: Cardinal;
            out StreamInfo: TStreamInfo): BOOL;
        function SelectVideoStream(index: Cardinal): BOOL;
        function SelectAudioStream(index: Cardinal): BOOL;
        property NumVideoStream: Cardinal read FNumVideoStream;
        property NumAudioStream: Cardinal read FNumAudioStream;

        property PlayerState: TPlayerState read FPlayerState;

        procedure SetMediaPosition(Pos: Int64);
    End;


implementation

{ TDSPlayer }

constructor TDSPlayer.Create(Handle: HWND);
begin
    // COM을 초기화한다.
    CoInitialize(nil);

    DsHwnd := Handle;
end;

destructor TDSPlayer.Destroy;
begin
    ShutDownDs;

    if Assigned(LAVSplitter) then LAVSplitter := nil;
    if Assigned(LAVAudioDecoder) then LAVAudioDecoder := nil;
    if Assigned(LAVVideoDecoder) then LAVVideoDecoder := nil;
    if Assigned(VSFilter) then VSFilter := nil;

    // COM을 셧다운시킨다.
    CoUninitialize;

    inherited Destroy;
end;

function TDSPlayer.BuildLAVFilters: BOOL;
var
    DllGetClassObject: function(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
    ClassF: IClassFactory;
    hr1, hr2, hr3, hr4: HRESULT;
begin
    try
        hr1 := E_FAIL;
        hr2 := E_FAIL;
        hr3 := E_FAIL;
        hr4 := E_FAIL;
        LAVSplitter := nil;
        LAVAudioDecoder := nil;
        LAVVideoDecoder := nil;
        VSFilter := nil;
        if FileExists(FFilterFolder + '\LAVSplitter.ax') then begin
            LibSplitterHandle := LoadLibrary(pchar(FFilterFolder + '\LAVSplitter.ax'));
            if LibSplitterHandle <> 0 then begin
                pointer(DllGetClassObject) := GetProcAddress(LibSplitterHandle, 'DllGetClassObject');
                DllGetClassObject(CLSID_LAVSplitterSource, IClassFactory, ClassF);
                hr1 := ClassF.CreateInstance(nil, IID_IBaseFilter, LAVSplitter);
            end;
        end;

        if FileExists(FFilterFolder + '\LAVAudio.ax') then begin
            LibAudioHandle := LoadLibrary(pchar(FFilterFolder + '\LAVAudio.ax'));
            if LibAudioHandle <> 0 then begin
                pointer(DllGetClassObject) := GetProcAddress(LibAudioHandle, 'DllGetClassObject');
                DllGetClassObject(CLSID_LAVAudioDecoder, IClassFactory, ClassF);
                hr2 := ClassF.CreateInstance(nil, IID_IBaseFilter, LAVAudioDecoder);
            end;
        end;

        if FileExists(FFilterFolder + '\LAVVideo.ax') then begin
            LibVideoHandle := LoadLibrary(pchar(FFilterFolder + '\LAVVideo.ax'));
            if LibVideoHandle <> 0 then  begin
                pointer(DllGetClassObject) := GetProcAddress(LibVideoHandle, 'DllGetClassObject');
                DllGetClassObject(CLSID_LAVVideoDecoder, IClassFactory, ClassF);
                hr3 := ClassF.CreateInstance(nil, IID_IBaseFilter, LAVVideoDecoder);
            end;
        end;

        if FileExists(FFilterFolder + '\VSFilterMod.dll') then begin
            LibVSHandle := LoadLibrary(pchar(FFilterFolder + '\VSFilterMod.dll'));
            if LibVSHandle <> 0 then begin
                pointer(DllGetClassObject) := GetProcAddress(LibVSHandle, 'DllGetClassObject');
                DllGetClassObject(CLSID_VSFIlter, IClassFactory, ClassF);
                hr4 := ClassF.CreateInstance(nil, IID_IBaseFilter, VSFilter);
            end;
        end;
	except
        Result := false;
        exit;
	end;
    Result := True;
end;

function TDSPlayer.BuildSoundRenderer: BOOL;
var
    hr: HRESULT;
begin
    result := FALSE;

    hr := CoCreateInstance(CLSID_DSoundRender, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, DirectSoundFilter);
    if (FAILED(hr)) then begin
        exit;
    end;

    hr := GraphBuilder.AddFilter(DirectSoundFilter, 'DirectSound');

	if (FAILED(hr)) then
        exit
    else
   	    result := TRUE;
end;

procedure TDSPlayer.GetMediaStreamInfo;
var
    hr: HRESULT;
    i: integer;
begin
    if not Assigned(srcFilter) then
        exit;

    hr := srcFilter.QueryInterface(IID_IAMStreamSelect, StreamSelector);
    if SUCCEEDED(hr) then begin
        hr := StreamSelector.Count(FNumStreams);
        if (hr <> S_OK) then
            FNumStreams := 0;
    end else
        FNumStreams := 0;

    SetLength(FStreamInfo, FNumStreams);
    FNumVideoStream := 0;
    FNumAudioStream := 0;
    FNumMiscStream := 0;

    for i := 0 to FNumStreams - 1 do begin
        StreamSelector.Info(i, FStreamInfo[i].ppmt, FStreamInfo[i].pdwFlags, FStreamInfo[i].plcid,
                                FStreamInfo[i].pdwGroup, FStreamInfo[i].ppszName,
                                FStreamInfo[i].ppObject, FStreamInfo[i].ppUnk);

        if IsEqualGUID(FStreamInfo[i].ppmt^.majortype, MEDIATYPE_Video) then begin
            inc(FNumVideoStream);
            FStreamInfo[i].streamType := stVideo;
        end
        else if IsEqualGUID(FStreamInfo[i].ppmt^.majortype, MEDIATYPE_Audio) then begin
            inc(FNumAudioStream);
            FStreamInfo[i].streamType := stAudio;
        end
        else begin
            inc(FNumMiscStream);
            FStreamInfo[i].streamType := stMiscellaneous;
        end;
    end;
end;

procedure TDSPlayer.VideoRenderWndProc(var Msg: TMessage);
var
    iEventCode: LongInt;
    iParam1, iParam2: LongInt;
begin
    case Msg.Msg of
        WM_GRAPHEVENT:
            begin
                MediaEvent.GetEvent(iEventCode, iParam1, iParam2, 100);
                if (iEventCode = EC_COMPLETE) or (iEventCode = EC_USERABORT)
                then
                begin
                    MediaControl.Stop;
                end;
            end;
        WM_LBUTTONDOWN:
            begin

            end;
        WM_LBUTTONDBLCLK:
            begin

            end;
        WM_RBUTTONDOWN:
            begin

            end;
        WM_KEYDOWN:
            begin
                if Msg.WParam = VK_SPACE then begin
                   MediaControl.Stop;
                end;
            end;
    else
        VideoRenderOrgMethod(Msg);
    end;
end;

function TDSPlayer.SetupDs: Boolean;
var
    hr: HRESULT;
begin
    Result := False;

    if Failed(CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph, GraphBuilder)) then
        Exit; // 필터그래프 생성

    GraphBuilder.QueryInterface(IID_IMediaControl, MediaControl);

    if Failed(GraphBuilder.QueryInterface(IID_IMediaEventEx, MediaEvent)) then
        Exit;
    if Failed(GraphBuilder.QueryInterface(IID_IMediaSeeking, MediaSeeking)) then
        Exit;
    if Failed(GraphBuilder.QueryInterface(IID_IMediaPosition, MediaPosition)) then
        Exit;

    if Failed(CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, VMR9BaseFilter)) then
        Exit;

    if Failed(GraphBuilder.QueryInterface(IID_IFilterGraph2, FilterGraph2)) then
        Exit;

    // QUERY the VMR9 interfaces
	hr := VMR9BaseFilter.QueryInterface(IID_IVMRFilterConfig9, VMR9FilterConfig);
	if (SUCCEEDED(hr)) then begin
	    // CONFIGURE the VMR9
		VMR9FilterConfig.SetRenderingMode(VMR9Mode_Windowless);
	 	VMR9FilterConfig.SetNumberOfStreams(1);
	end;

    hr := VMR9BaseFilter.QueryInterface(IID_IVMRWindowlessControl9, m_pWindowless);
	if (SUCCEEDED(hr)) then begin
	    // CONFIGURE the VMR9
		m_pWindowless.SetVideoClippingWindow(DsHwnd);
        m_pWindowless.SetAspectRatioMode(VMR9ARMode_LetterBox);
	end;

    VMR9BaseFilter.QueryInterface(IID_IVMRMixerBitmap9, VMR9MixerBitmap);
 	VMR9BaseFilter.QueryInterface(IID_IVMRMonitorConfig9, VMR9MonitorConfig);

    AvailableDS := true;
    Result := true;
end;

function TDSPlayer.ShutDownDs: Boolean;
begin
    if Assigned(VMR9FilterConfig) then VMR9FilterConfig := nil;
    if Assigned(m_pWindowless) then m_pWindowless := nil;

    if Assigned(MediaControl) then
        MediaControl.Stop;

    VMR9BaseFilter := nil;
    MediaControl := nil;
    MediaSeeking := nil;
    MediaPosition := nil;
    FilterGraph2 := nil;
    GraphBuilder := nil;

    SetLength(FStreamInfo, 0);
    FNumStreams := 0;
    FNumVideoStream := 0;
    FNumAudioStream := 0;
    FNumMiscStream := 0;
    FVideoStreamIndex := -1;
    FAudioStreamIndex := -1;

    Result := true;
end;

function TDSPlayer.LoadMedia(FileName: String): Boolean;
var
    hr: HRESULT;
    pEnum: IEnumPins;
    pPin: IPin;
    bRenderedAnyPin: BOOL;
    rc: TRECT;
    WFileName: Array [0 .. 255] of WideChar;
    PFileName: PWideChar;
begin
    result := false;

    StringToWideChar(filename, WFileName, 255);
    PFileName := @WFileName[0];


    // DShow 해제
    ShutDownDs;

    // DShow 초기화
    if SetupDs = False then
        Exit;

    BuildLAVFilters;

    BuildSoundRenderer;

    if Assigned(LAVAudioDecoder) then
        hr := GraphBuilder.AddFilter(LAVAudioDecoder, 'LAV Audio Decoder');
    if Assigned(LAVVideoDecoder) then
        hr := GraphBuilder.AddFilter(LAVVideoDecoder, 'LAV Video Decoder');

    GraphBuilder.AddFilter(VMR9BaseFilter, 'Video Mixing Renderer 9');

    // 자말 필터 추가
    if Assigned(VSFilter) then
        hr := GraphBuilder.AddFilter(VSFilter, 'VS Filter');

    // DirectSound 필터 추가
    if SUCCEEDED(hr) then
        hr := GraphBuilder.AddFilter(DirectSoundFilter, 'DirectSound');

    FilterGraph2.RenderFile(PFileName, nil);

    pEnum := nil;
    bRenderedAnyPin := true;

    if bRenderedAnyPin then begin

        GetMediaStreamInfo;
        if NumVideoStream >= 1 then
            SelectVideoStream(0);
        if NumAudioStream >= 1 then
            SelectAudioStream(0);

        // 디스플레이 Screen = Panel
        GetClientRect(DsHwnd, rc);
        m_pWindowless.SetVideoPosition(nil, @rc);

        // 이벤트 설정
        MediaEvent.SetNotifyWindow(OAHWND(DsHwnd), WM_GRAPHEVENT, 0);
        MediaEvent.SetNotifyFlags(0);

        MediaPosition.get_Duration(MediaLength);

        // 재생
        MediaControl.Run;

        result := true;
    end;
end;

function TDSPlayer.GetCurPos: Double;
var
    CurPos: Double;
begin
    MediaPosition.get_CurrentPosition(CurPos);
    Result := CurPos;
end;

procedure TDSPlayer.SetCurPos(Pos: Double);
begin
    if Assigned(MediaPosition) then
        MediaPosition.put_CurrentPosition(Pos);
end;

procedure TDSPlayer.Run;
begin
    // 재생
    if Assigned(MediaControl) then
        MediaControl.Run;
end;

procedure TDSPlayer.Pause;
begin
    // 일시정지
    if Assigned(MediaControl) then
        MediaControl.Pause;
end;

procedure TDSPlayer.Resize(MediaWindow: HWND);
var
    rc: TRECT;
begin
    GetClientRect(MediaWindow, rc);
    m_pWindowless.SetVideoPosition(nil, @rc);
end;

procedure TDSPlayer.GetVideoSize(var Width, Height: integer);
var
    hr: HRESULT;
    ARWidth: Longint;
	ARHeight: Longint;
begin
    if (m_pWindowless <> nil) then begin
        hr := m_pWindowless.GetNativeVideoSize(Width, Height, ARWidth, ARHeight);
    end;
end;

procedure TDSPlayer.SetPreserveAspectRatio(bPreserve: BOOL);
begin
    bKeepAspectRatio := bPreserve;
    if Assigned(m_pWindowless) then begin
        if bKeepAspectRatio then
            m_pWindowless.SetAspectRatioMode(VMR9ARMode_LetterBox)
        else
            m_pWindowless.SetAspectRatioMode(VMR9ARMode_None);
    end;
end;

function TDSPlayer.GetStreamInfo(StreamType: TStreamType; index: Cardinal; out
    StreamInfo: TStreamInfo): BOOL;
var
    i, k: integer;
begin
    result := false;

    k := -1;
    case StreamType of
        stAudio: begin;
        for i := 0 to FNumStreams - 1 do
            if FStreamInfo[i].streamType = stAudio then begin
                inc(k);
                if index = k then begin
                    StreamInfo := FStreamInfo[i];
                    result := true;
                    break;
                end;
            end;
        end;
        stVideo: begin
        for i := 0 to FNumStreams - 1 do
            if FStreamInfo[i].streamType = stVideo then begin
                inc(k);
                if index = k then begin
                    StreamInfo := FStreamInfo[i];
                    result := true;
                    break;
                end;
            end;
        end;
        stMiscellaneous: begin
        for i := 0 to FNumStreams - 1 do
            if FStreamInfo[i].streamType = stMiscellaneous then begin
                inc(k);
                if index = k then begin
                    StreamInfo := FStreamInfo[i];
                    result := true;
                    break;
                end;
            end;
        end;
    end;
end;

function TDSPlayer.SelectVideoStream(index: Cardinal): BOOL;
var
    i, k: integer;
    hr: HRESULT;
begin
    result := false;

    if not Assigned(StreamSelector) then
        exit;
    if index >= FNumVideoStream then
        exit;
    if FNumVideoStream < 1 then
        exit;

    k := -1;
    for i := 0 to FNumStreams - 1 do
        if FStreamInfo[i].streamType = stVideo then begin
            inc(k);
            if index = k then begin
                hr := StreamSelector.Enable(i, AMSTREAMSELECTENABLE_ENABLE);  // 비디오 스트림 선택
                result := (hr = S_OK);
                if result then
                    FVideoStreamIndex := index;

                break;
            end;
        end;
end;

function TDSPlayer.SelectAudioStream(index: Cardinal): BOOL;
var
    i, k: integer;
    hr: HRESULT;
begin
    result := false;

    if not Assigned(StreamSelector) then
        exit;
    if index >= FNumAudioStream then
        exit;
    if FNumAudioStream < 1 then
        exit;

    k := -1;
    for i := 0 to FNumStreams - 1 do
        if FStreamInfo[i].streamType = stAudio then begin
            inc(k);
            if index = k then begin
                hr := StreamSelector.Enable(i, AMSTREAMSELECTENABLE_ENABLE);  // 오디오 스트림 선택
                result := (hr = S_OK);
                if result then
                    FAudioStreamIndex := index;
                break;
            end;
        end;
end;

procedure TDSPlayer.SetMediaPosition(Pos: Int64);
var
   cur_pos, stop : Int64;
begin
    MediaSeeking.GetPositions(cur_pos, stop);
    cur_pos := cur_pos + (pos * 10000000);
    MediaSeeking.setPositions(cur_pos, AM_SEEKING_AbsolutePositioning, stop, AM_SEEKING_NoPositioning);
end;

end.

