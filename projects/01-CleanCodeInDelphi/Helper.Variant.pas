unit Helper.Variant;

interface

uses
  System.Variants;

type
  TVariantHelper = record helper for Variant
    function IsNull: Boolean;
  end;

implementation

{ TVariantHelper }

function TVariantHelper.IsNull: Boolean;
begin
  Result := VarIsNull(Self);
end;

end.
