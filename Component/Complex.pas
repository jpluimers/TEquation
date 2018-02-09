unit Complex;

interface

uses
 System.Math, System.SysUtils;

type
 TComplex = record
  private
   r: Double;
   i: Double;
  public
   constructor Create(const real, imaginary: double);
   class operator Implicit(const D: Double): TComplex;
   class operator Negative(const C: TComplex): TComplex;
   class operator Equal(const C1, C2: TComplex): Boolean;
   class operator NotEqual(const C1, C2: TComplex): Boolean;
   class operator Add(const C1, C2: TComplex): TComplex;
   class operator Add(const C: TComplex; const D: Double): TComplex;
   class operator Add(const D: Double; const C: TComplex): TComplex;
   class operator Subtract(const C1, C2: TComplex): TComplex;
   class operator Subtract(const C: TComplex; const D: Double): TComplex;
   class operator Subtract(const D: Double; const C: TComplex): TComplex;
   class operator Multiply(const C1, C2: TComplex): TComplex;
   class operator Multiply(const C: TComplex; const D: Double): TComplex;
   class operator Multiply(const D: Double; const C: TComplex): TComplex;
   class operator Divide(const C1, C2: TComplex): TComplex;
   class operator Divide(const C: TComplex; const D: Double): TComplex;
   class operator Divide(const D: Double; const C: TComplex): TComplex;
   class function power(const D: TComplex; exp: double): TComplex; static;
   function IsZero: Boolean;
   function IsNonZero: Boolean;
   function Conj: TComplex;
   function Sqr: TComplex;
   function Sqrt: TComplex;
   function Mag: Double;
   function SqrMag: Double;
   function ToString: string;
   property RealPart: Double read r;
   property ImagPart: Double read i;
end;

const
  ZeroComplex: TComplex = ();

implementation

constructor TComplex.Create(const real, imaginary: double);
begin
  r := real;
  i := imaginary;
end;

class operator TComplex.Implicit(const D: Double): TComplex;
begin
  Result.r := D;
  Result.i := 0.0;
end;

class operator TComplex.Negative(const C: TComplex): TComplex;
begin
  Result.r := -C.r;
  Result.i := -C.i;
end;

class operator TComplex.Equal(const C1, C2: TComplex): Boolean;
begin
  Result := (C1.r = C2.r) and (C1.i = C2.i);
end;

class operator TComplex.NotEqual(const C1, C2: TComplex): Boolean;
begin
  Result := not (C1 = C2);
end;

class operator TComplex.Add(const C1, C2: TComplex): TComplex;
begin
  Result.r := C1.r + C2.r;
  Result.i := C1.i + C2.i;
end;

class operator TComplex.Add(const C: TComplex; const D: Double): TComplex;
begin
  Result.r := C.r + D;
  Result.i := C.i;
end;

class operator TComplex.Add(const D: Double; const C: TComplex): TComplex;
begin
  Result.r := D + C.r;
  Result.i := C.i;
end;

class operator TComplex.Subtract(const C1, C2: TComplex): TComplex;
begin
  Result.r := C1.r - C2.r;
  Result.i := C1.i - C2.i;
end;

class operator TComplex.Subtract(const C: TComplex; const D: Double): TComplex;
begin
  Result.r := C.r - D;
  Result.i := C.i;
end;

class operator TComplex.Subtract(const D: Double; const C: TComplex): TComplex;
begin
  Result.r := D - C.r;
  Result.i := -C.i;
end;

function TComplex.ToString: string;
begin
  if (Self.i >= 0) then
    Result := FloatToStr(Self.r) + ' + ' + FloatToStr(Self.i) + 'i'
  else
    Result := FloatToStr(Self.r) + ' - ' + FloatToStr(Self.i*-1) + 'i'
end;

class operator TComplex.Multiply(const C1, C2: TComplex): TComplex;
begin
  Result.r := C1.r*C2.r - C1.i*C2.i;
  Result.i := C1.r*C2.i + C1.i*C2.r;
end;

class operator TComplex.Multiply(const C: TComplex; const D: Double): TComplex;
begin
  Result.r := C.r*D;
  Result.i := C.i*D;
end;

class operator TComplex.Multiply(const D: Double; const C: TComplex): TComplex;
begin
  Result.r := D*C.r;
  Result.i := D*C.i;
end;

class operator TComplex.Divide(const C1, C2: TComplex): TComplex;
var
  R, Denominator: Double;
begin
 if abs(C2.r)>=abs(C2.i) then
  begin
   R := C2.i/C2.r;
   Denominator := C2.r+R*C2.i;
   Result.r := (C1.r+R*C1.i)/Denominator;
   Result.i := (C1.i-R*C1.r)/Denominator;
  end
 else
  begin
   R := C2.r/C2.i;
   Denominator := C2.i+R*C2.r;
   Result.r := (C1.r*R+C1.i)/Denominator;
   Result.i := (C1.i*R-C1.r)/Denominator;
  end;
end;

class operator TComplex.Divide(const C: TComplex; const D: Double): TComplex;
begin
  Result := C*(1.0/D);
end;

class operator TComplex.Divide(const D: Double; const C: TComplex): TComplex;
var
  R, Denominator: Double;
begin
  if abs(C.r)>=abs(C.i) then
   begin
    R := C.i/C.r;
    Denominator := C.r+R*C.i;
    Result.r := D/Denominator;
    Result.i := -R*Result.r;
   end
  else
   begin
    R := C.r/C.i;
    Denominator := C.i+R*C.r;
    Result.i := -D/Denominator;
    Result.r := -R*Result.i;
   end;
end;

class function TComplex.power(const D: TComplex; exp: double): TComplex;
begin
  Result.r := System.Math.power((D.r*D.r + D.i*D.i),exp/2)*cos(exp*arctan(D.i/D.r));
  Result.i := System.Math.power((D.r*D.r + D.i*D.i),exp/2)*sin(exp*arctan(D.i/D.r));
end;

function TComplex.IsZero: Boolean;
begin
  Result := Self=ZeroComplex;
end;

function TComplex.IsNonZero: Boolean;
begin
  Result := Self<>ZeroComplex;
end;

function TComplex.Conj: TComplex;
begin
  Result.r := r;
  Result.i := -i;
end;

function TComplex.Sqr: TComplex;
begin
  Result := Self*Self;
end;

function TComplex.Sqrt: TComplex;
var
  x, y, v, w: Double;
begin
  if IsZero then
   begin
    Result := ZeroComplex;
   end
  else
   begin
    x := abs(r);
    y := abs(i);

    if (x >= y) then
     begin
      v := y/x;
      w := System.Sqrt(x)*System.Sqrt(0.5*(1.0+System.Sqrt(1.0+v*v)));
     end
    else
     begin
      v := x/y;
      w := System.Sqrt(y)*System.Sqrt(0.5*(v+System.Sqrt(1.0+v*v)));
     end;

    if (r >= 0.0) then
     begin
      Result.r := w;
      Result.i := i/(2.0*w);
     end
    else
     begin

      if (i >= 0.0) then
        Result.i := w
      else
        Result.i := -w;

      Result.r := i/(2.0*Result.i);
     end;

   end;
end;

function TComplex.Mag: Double;
var
  x, y, Temp: Double;
begin
  x := abs(r);
  y := abs(i);

  if (x = 0.0) then
   begin
    Result := y;
   end
  else
   if (y = 0.0) then
    begin
     Result := x;
    end
   else
    if (x > y) then
     begin
      Temp := y/x;
      Result := x*System.Sqrt(1.0+Temp*Temp);
     end
    else
     begin
      Temp := x/y;
      Result := y*System.Sqrt(1.0+Temp*Temp);
     end;

end;

function TComplex.SqrMag: Double;
begin
  Result := System.Sqr(r) + System.Sqr(i);
end;

end.


