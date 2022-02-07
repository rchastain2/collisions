
unit main;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes,
  Ball;

type
  { TForm1 }
  TForm1 = class(TForm)
    Prop: TApplicationProperties;
    procedure PropIdle(Sender: TObject; var Done: boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  LBalls: array of TBall;
  LInitialized: boolean = False;
  LBitmap: TBGRABitmap;
  LWaitingForDisplay: boolean = False;

implementation

{$R *.lfm}

procedure Render;
var
  i, j: integer;
begin
  if not LInitialized then
    Exit;

  LBitmap.Fill(BGRABlack);

  for i := 0 to High(LBalls) do
    LBalls[i].Render(LBitmap);

  Form1.Invalidate;
  LWaitingForDisplay := True;

  for i := 0 to High(LBalls) do
    LBalls[i].Move;

  for i := 0 to High(LBalls) do
    for j := i + 1 to High(LBalls) do
      LBalls[i].Collide(LBalls[j]);

  for i := 0 to High(LBalls) do
    LBalls[i].BorderCollision(Form1.ClientRect);
end;

{ TForm1 }

procedure TForm1.PropIdle(Sender: TObject; var Done: boolean);
begin
  if not LWaitingForDisplay then
    Render;
  Done := False;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  LInitialized := False;
  for i := 0 to High(LBalls) do
    LBalls[i].Free;
  LBitmap.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, w, h: integer;
begin
  BorderStyle := bsNone;

  Top := 0;
  Left := 0;
  Width := Screen.Width;
  Height := Screen.Height;

  LBitmap := TBGRABitmap.Create(ClientWidth, ClientHeight);

  SetLength(LBalls, 10);
  for i := 0 to High(LBalls) do
  begin
    LBalls[i] := TBall.Create(PointF(0, 0), PointF(0, 0), Random(20) + 30, 0);
    LBalls[i].CalculateMass;
  end;

  w := Width div 5;
  h := Height div 4;

  LBalls[0].position := PointF(w * 1, h * 2);
  LBalls[1].Position := PointF(w * 2, h * 1);
  LBalls[2].Position := PointF(w * 3, h * 1);
  LBalls[3].Position := PointF(w * 4, h * 1);
  LBalls[4].Position := PointF(w * 2, h * 2);
  LBalls[5].Position := PointF(w * 3, h * 2);
  LBalls[6].Position := PointF(w * 4, h * 2);
  LBalls[7].Position := PointF(w * 2, h * 3);
  LBalls[8].Position := PointF(w * 3, h * 3);
  LBalls[9].Position := PointF(w * 4, h * 3);

  LBalls[0].Speed := PointF(Cos((-45) * PI / 180) * 4, Sin(-45 * PI / 180) * 4);

  LInitialized := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Close;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  LBitmap.Draw(Canvas, 0, 0, True);
  LWaitingForDisplay := False;
end;

end.
