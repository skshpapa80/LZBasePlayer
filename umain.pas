unit umain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    DSPack;

type

    { TfrmMain }

    TfrmMain = class(TForm)
        DSScreen: TDSVideoWindowEx2;
        DSTrackBar1: TDSTrackBar;
        FilterGraph1: TFilterGraph;
        lblCur: TLabel;
        lblTotal: TLabel;
        paButton: TPanel;
    private

    public

    end;

var
   frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

