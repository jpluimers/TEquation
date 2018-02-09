unit Fraction;

interface

uses
 System.SysUtils, System.Math;

type
 TFraction = record
  strict private
   aNumerator: integer;
   aDenominator: integer;
   function GCD(a, b: integer): integer;
  public
   //methods
   constructor Create(aNumerator: integer; aDenominator: integer); overload;
   constructor Create(aNumber: double); overload;
   constructor Create(aFraction: string); overload;
   function ToString: string;
   procedure Reduce;
   //class operators
   class operator Add(fraction1, fraction2: TFraction): TFraction;
   class operator Subtract(fraction1, fraction2: TFraction): TFraction;
   class operator Multiply(fraction1, fraction2: TFraction): TFraction;
   class operator Divide(fraction1, fraction2: TFraction): TFraction;
   class operator Negative(const Value: TFraction): TFraction;
   class operator Implicit(const Value: TFraction): double;
   //properties
   property Numerator: integer read aNumerator;
   property Denominator: integer read aDenominator;
 end;

implementation

{ TFraction }

constructor TFraction.Create(aNumerator, aDenominator: integer);
begin

  if (aDenominator = 0) then
   begin
    raise Exception.Create('Denominator cannot be zero in rationals!');
   end;

  if ( (aNumerator < 0) or (aDenominator < 0) ) then
   begin
    Self.aNumerator := -aNumerator;
    Self.aDenominator := -aDenominator;
   end
  else
   begin
    Self.aNumerator := aNumerator;
    Self.aDenominator := aDenominator;
   end;

end;

constructor TFraction.Create(aNumber: double);
var h1, h2, k1, k2, y, a, aux: double;
begin

  //Setup the values
  h1 := 1;
  h2 := 0;
  k1 := 0;
  k2 := 1;
  y := abs(aNumber);

  //Generates the fraction
  repeat
    begin
      a := floor(y);
      aux := h1;
      h1 := a * h1 + h2;
      h2 := aux;
      aux := k1;
      k1 := a * k1 + k2;
      k2 := aux;
      if (y - a = 0) or (k1 = 0) then break;
      y := 1 / (y - a) ;
    end;
  until (Abs(abs(aNumber) - h1 / k1) <= abs(aNumber) * 0.000001);

  //If the number was negative, add a - in front of the numerator
  if (aNumber < 0) then
    h1 := h1*-1;

  //Output
  if not(h1 = 0) then
   begin
    Self.aNumerator := Trunc(h1);
    Self.aDenominator := Trunc(k1);
   end
  else
   begin
    Self.aNumerator := 0;
    Self.aDenominator := 1;
   end;

end;

constructor TFraction.Create(aFraction: string);
var
  BarPos: integer;
  numStr, denomStr: string;
begin
  BarPos := Pos('/', aFraction);

  if BarPos = 0 then
   raise Exception.Create('Invalid fraction as input!');

  numStr := Trim(Copy(aFraction, 1, BarPos - 1));
  denomStr := Trim(Copy(aFraction, BarPos + 1, Length(aFraction)));
  aNumerator := StrToInt(numStr);
  aDenominator := StrToInt(denomStr);
end;

function TFraction.GCD(a, b: integer): integer;
var remd: integer;
begin
  remd := a mod b;

  if remd = 0 then
   Result := b
  else
   Result := gcd(b, remd);
end;

procedure TFraction.Reduce;
var LGCD: integer;
begin
  LGCD := GCD(aNumerator, aDenominator);
  aNumerator := aNumerator div LGCD;
  aDenominator := aDenominator div LGCD;
end;

//operators overloads

class operator TFraction.Add(fraction1, fraction2: TFraction): TFraction;
begin
  Result := TFraction.Create(fraction1.Numerator*fraction2.Denominator+fraction1.Denominator*fraction2.Numerator,fraction1.Denominator*fraction2.Denominator);
end;

class operator TFraction.Subtract(fraction1, fraction2: TFraction): TFraction;
begin
  Result := fraction1 + (-fraction2);
end;

function TFraction.ToString: string;
begin
  Result := Numerator.ToString + '/' + Denominator.ToString;
end;

class operator TFraction.Multiply(fraction1, fraction2: TFraction): TFraction;
begin
  Result := TFraction.Create(fraction1.Numerator*fraction2.Numerator,fraction1.Denominator*fraction2.Denominator);
end;

class operator TFraction.Divide(fraction1, fraction2: TFraction): TFraction;
begin
  Result := TFraction.Create(fraction1.Numerator*fraction2.Denominator,fraction1.Denominator*fraction2.Numerator);
end;

class operator TFraction.Implicit(const Value: TFraction): double;
begin
  Result := Value.Numerator / Value.Denominator;
end;

class operator TFraction.Negative(const Value: TFraction): TFraction;
begin
  Result := TFraction.Create(-Value.aNumerator, Value.aDenominator);
end;

end.

