
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
    procedure PropIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  LBalls: array of TBall;
  LInitialized: Boolean = FALSE;
  LBitmap: TBGRABitmap;

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
  
  LBitmap.Draw(Form1.Canvas, 0, 0, True);
  
  for i := 0 to High(LBalls) do
    LBalls[i].Move;
  
  for i := 0 to High(LBalls) do
    for j := i + 1 to High(LBalls) do
        LBalls[i].Collide(LBalls[j]);
  
  for i := 0 to High(LBalls) do
    LBalls[i].BorderCollision(Form1.ClientRect);
end;

{ TForm1 }

procedure TForm1.PropIdle(Sender: TObject; var Done: Boolean);
begin
  Render;
  Done := FALSE;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  LInitialized := FALSE;
  for i := 0 to High(LBalls) do
    LBalls[i].Free;
  LBitmap.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, w, h: Integer;
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
    LBalls[i] := TBall.Create(PointF(0, 0), PointF(0, 0), Random(30) + 20, 0);
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

  LInitialized := TRUE;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Close;
end;

end.
