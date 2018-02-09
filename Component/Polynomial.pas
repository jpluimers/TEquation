unit Polynomial;

interface

uses
 System.SysUtils, System.Math;

type
 TPolyException = class(Exception)
 end;

type
 ArrOfDouble = array of double;

type
 TPolynomial = record
   private
     FPoly: array of double;
     FDegree: integer;
     function GetCoeff(Index: integer): double;
   public
     constructor Create(values: array of double);
     //useful methods for a poly
     procedure Negate();
     function EvaluateOn(x: double): double;
     function GetDerivative: TPolynomial;
     function ToString: string;
     function ToArray: ArrOfDouble;
     //operator overloads
     class operator Add(const a, b: TPolynomial): TPolynomial;
     class operator Subtract(const a, b: TPolynomial): TPolynomial;
     //properties
     property Degree: integer read Fdegree;
     property Coeff[Index: integer]: double read GetCoeff;
 end;

implementation

{ TPolynomial }

constructor TPolynomial.Create(values: array of double);
var i, len: integer;
begin

  len := Length(values);

  //The minimal allowed equation must have at least a number (like f(x) = 3)
  if (len = 0) then
    raise TPolyException.Create('There must be at least one coefficient!');

  FDegree := len - 1;
  SetLength(FPoly, len);

  for i := Low(values) to High(values) do
    FPoly[i] := values[i];

end;

function TPolynomial.EvaluateOn(x: double): double;
begin
  Result := Poly(x, FPoly);
end;

function TPolynomial.GetCoeff(Index: integer): double;
begin

  if ((Index < 0) or (Index > Length(FPoly)-1)) then
    raise TPolyException.Create('Out of bounds.');

  Result := FPoly[Index];
end;

function TPolynomial.GetDerivative: TPolynomial;
var x: array of double;
    i: integer;
begin

  if (FDegree = 0) then
   begin
     Result := TPolynomial.Create([0])
   end
  else
   begin
     SetLength(x, Length(FPoly)-1);

     for i := 0 to Length(x)-1 do
       x[i] := FPoly[i+1]*(i+1);

     Result := TPolynomial.Create(x);
   end;

end;

procedure TPolynomial.Negate;
var i: integer;
begin
  for i := Low(FPoly) to High(FPoly) do
    FPoly[i] := FPoly[i]*-1;
end;

function TPolynomial.ToArray: ArrOfDouble;
var i: integer;
begin

  SetLength(Result, Length(FPoly));
  for i := Low(FPoly) to High(FPoly) do
    Result[i] := FPoly[i];

end;

function TPolynomial.ToString: string;
var i: integer;
begin
  Result := '';

  for i := Low(FPoly) to High(FPoly) do
   begin

     if (i <> 0) then
       Result := Result + Abs(FPoly[i]).ToString + 'x^' + i.ToString
     else
       Result := Result + FPoly[i].ToString;

     if (i <> High(FPoly)) then
      begin
         if FPoly[i+1] >= 0 then
          Result := Result + ' + '
        else
          Result := Result + ' - ';
      end;

   end;

end;

class operator TPolynomial.Add(const a, b: TPolynomial): TPolynomial;
var size, i: integer;
begin

  if (a.Degree >= b.Degree) then
    size := a.Degree
  else
    size := b.Degree;

  SetLength(Result.FPoly, size + 1);
  for i := a.Degree downto 0 do
    Result.FPoly[i] := a.Coeff[i];
  for i := b.Degree downto 0 do
    Result.FPoly[i] := Result.FPoly[i] + b.Coeff[i];

  //check how many 0 we have at the end of the array and remove them
  i := 0;
  while (Result.FPoly[Length(Result.FPoly)-1] = 0) do
    begin
     Inc(i);
    end;

  if (i > 0) then
    SetLength(Result.FPoly, size + 1 - i);

end;

class operator TPolynomial.Subtract(const a, b: TPolynomial): TPolynomial;
begin
  b.Negate;
  Result := a + b;
end;

end.

