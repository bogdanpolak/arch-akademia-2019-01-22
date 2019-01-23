unit Helper.TWinControl;

interface

uses
  Vcl.Controls;

type
  TWinControlHelper = class helper for TWinControl
  public
    procedure HideAllChildFrames;
    function SumHeightForChildrens(
      ControlsToExclude: TArray<TControl>): Integer;
  end;

implementation

uses
  Vcl.Forms;

{ TWinControlHelper }

procedure TWinControlHelper.HideAllChildFrames;
var
  i: Integer;
begin
  for i := Self.ControlCount - 1 downto 0 do
    if Self.Controls[i] is TFrame then
      (Self.Controls[i] as TFrame).Visible := False;
end;

function TWinControlHelper.SumHeightForChildrens(
  ControlsToExclude: TArray<TControl>): Integer;
var
  i: Integer;
  ctrl: Vcl.Controls.TControl;
  isExcluded: Boolean;
  j: Integer;
  sumHeight: Integer;
  ctrlHeight: Integer;
begin
  sumHeight := 0;
  for i := 0 to Self.ControlCount - 1 do
  begin
    ctrl := Self.Controls[i];
    isExcluded := False;
    for j := 0 to Length(ControlsToExclude) - 1 do
      if ControlsToExclude[j] = ctrl then
        isExcluded := True;
    if not isExcluded then
    begin
      if ctrl.AlignWithMargins then
        ctrlHeight := ctrl.Height + ctrl.Margins.Top + ctrl.Margins.Bottom
      else
        ctrlHeight := ctrl.Height;
      sumHeight := sumHeight + ctrlHeight;
    end;
  end;
  Result := sumHeight;
end;

end.
