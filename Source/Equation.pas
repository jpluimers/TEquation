unit Equation;

interface

uses
 Complex, System.Math, Generics.Collections, System.SysUtils, Polynomial;

//Type of the equation that has to be solved
type
 TEquationType = (FirstDegree = 0, SecondDegree, ThirdDegree, FourthDegree, NthDegree);

//With Nth degree equations you have to use a root finding algorithm. The
//default one is NewtonRaphson but you can add others like Secant, Bisection...
type
 TSolverType = (TNewtonRaphson = 0, TSecant);

type
 TEquationSol = TList<TComplex>;

type
 TEquationError = class(Exception)
 end;

type
 TRootFindAlgorithm = reference to function(const points: array of double): double;

type
 TEquation = class abstract
  private
   FEquation: TPolynomial;
  public
   constructor Create(values: array of double);
   //abstract methods
   function GetType: TEquationType; virtual; abstract;
   procedure SolveEquation(const container: TEquationSol); virtual; abstract;
   //class methods
   function ToString: string; override;
   function GetPolynomial: TPolynomial;
   function GetDerivative: TPolynomial;
 end;

type
 TFirstDegree = class(TEquation)
  strict private
   Fa, Fb: double;
  public
   //methods
   constructor Create(const a, b: double);
   function GetType: TEquationType; override;
   procedure SolveEquation(const container: TEquationSol); override;
   //properties
   property a: double read Fa;
   property b: double read Fb;
 end;

type
 TSecondDegree = class(TEquation)
  strict private
   Fa, Fb, Fc: double;
  public
   //methods
   constructor Create(const a, b, c: double);
   function GetType: TEquationType; override;
   procedure SolveEquation(const container: TEquationSol); override;
   function getDelta: double;
   //properties
   property a: double read Fa;
   property b: double read Fb;
   property c: double read Fc;
 end;

type
 TThirdDegree = class(TEquation)
  strict private
   Fa, Fb, Fc, Fd: double;
  public
   //methods
   constructor Create(const a, b, c, d: double);
   function GetType: TEquationType; override;
   procedure SolveEquation(const container: TEquationSol); override;
   function getDelta: double;
   //properties
   property a: double read Fa;
   property b: double read Fb;
   property c: double read Fc;
   property d: double read Fd;
 end;

type
 TFourthDegree = class(TEquation)
  strict private
   Fa, Fb, Fc, Fd, Fe: double;
  public
   //methods
   constructor Create(const a, b, c, d, e: double);
   function GetType: TEquationType; override;
   procedure SolveEquation(const container: TEquationSol); override;
   function getDelta: double;
   //properties
   property a: double read Fa;
   property b: double read Fb;
   property c: double read Fc;
   property d: double read Fd;
   property e: double read Fe;
 end;

type
 TNthDegree = class(TEquation)
   strict private
    FSolver: TSolverType;
    FAreCPSetted: boolean;
    FCriticalPoints: array of double;
    TRFList: TDictionary<TSolverType, TRootFindAlgorithm>;
    procedure Init;
    function GetCoeff(Index: integer): double;
   public
    //methods
    constructor Create(const params: array of double);
    destructor Destroy; override;
    function GetType: TEquationType; override;
    procedure SolveEquation(const container: TEquationSol); override;
    procedure SetSolverType(const x: TSolverType);
    procedure SetPoints(const k: array of double);
    //properties
    property Coeff[Index: integer]: double read GetCoeff;
 end;

implementation

{ TEquation }

constructor TEquation.Create(values: array of double);
begin
  FEquation := TPolynomial.Create(values);
end;

function TEquation.GetDerivative: TPolynomial;
begin
  Result := FEquation.GetDerivative;
end;

function TEquation.GetPolynomial: TPolynomial;
begin
  Result := FEquation;
end;

function TEquation.ToString: string;
begin
  Result := FEquation.ToString;
end;

{ TFirstDegree }

constructor TFirstDegree.Create(const a, b: double);
begin
  inherited Create([a, b]);

  Fa := b;
  Fb := a;
end;

function TFirstDegree.GetType: TEquationType;
begin
  Result := TEquationType.FirstDegree;
end;

procedure TFirstDegree.SolveEquation(const container: TEquationSol);
begin

  if Assigned(container) then
   begin
     container.Clear;
     container.Add(TComplex.Create(-b/a, 0));
   end;

end;

{ TSecondDegree }

constructor TSecondDegree.Create(const a, b, c: double);
begin
  inherited Create([a, b, c]);

  if (a = 0) then
    raise TEquationError.Create('The first term cannot be zero.');

  Fa := c;
  Fb := b;
  Fc := a;
end;

function TSecondDegree.getDelta: double;
begin
  Result := Fb*Fb- 4*Fa*Fc;
end;

function TSecondDegree.GetType: TEquationType;
begin
  Result := TEquationType.SecondDegree;
end;

procedure TSecondDegree.SolveEquation(const container: TEquationSol);
var delta: double;
begin

  if Assigned(container) then
   begin

    delta := getDelta;
    container.Clear;

    if (delta >= 0) then
     begin
       container.Add(TComplex.Create( ((-Fb + sqrt(delta))/(2*Fa)), 0 ));
       container.Add(TComplex.Create( ((-Fb - sqrt(delta))/(2*Fa)), 0 ));
     end
    else
     begin
       container.Add(TComplex.Create( -Fb/(2*Fa) , (sqrt(abs(delta)))/(2*Fa) ));
       container.Add(TComplex.Create( -Fb/(2*Fa) , -(sqrt(abs(delta)))/(2*Fa) ));
     end;

   end;

end;

{ TThirdDegree }

constructor TThirdDegree.Create(const a, b, c, d: double);
begin
  inherited Create([a, b, c, d]);

  if (a = 0) then
    raise TEquationError.Create('The first term cannot be zero.');

  Fa := d;
  Fb := c;
  Fc := b;
  Fd := a;
end;

function TThirdDegree.getDelta: double;
begin
  Result := Fc*Fc*Fb*Fb - 4*Fd*Fb*Fb*Fb - 4*Fc*Fc*Fc*Fa + 18*Fa*Fb*Fc*Fd - 27*Fd*Fd*Fa*Fa;
end;

function TThirdDegree.GetType: TEquationType;
begin
  Result := TEquationType.ThirdDegree;
end;

procedure TThirdDegree.SolveEquation(const container: TEquationSol);
var a_over_3, q, q_cube, r, theta, sqrt_q: double;
    s, t, sqrt_d, term1, delta: double;
    a, b, c: double;

 function cbrt(const val: double): double;
  begin
    if (val >= 0) then
      Result := power(val, 1/3)
    else
      Result := -power(val*-1, 1/3)
  end;

  const
   TWO_PI = 2*Pi;
   FOUR_PI = 4*Pi;

begin

  if Assigned(container) then
   begin

     a := Fb/Fa;
     b := Fc/Fa;
     c := Fd/Fa;
     q := (3*b - a*a) / 9.0;
     r := (9*a*b - 27*c - 2*a*a*a) / 54.0;

     a_over_3 := a/3;
     q_cube := q*q*q;
     delta := q_cube + (r*r);

     container.Clear;

     if (delta < 0) then
      begin

       theta := arccos(r/sqrt(-q_cube));
       sqrt_q := sqrt(-q);

       container.Add(TComplex.Create( (sqrt_q*2*cos(theta/3) - a_over_3) , 0 ));
       container.Add(TComplex.Create( (sqrt_q*2*cos((theta+TWO_PI)/3) - a_over_3) , 0 ));
       container.Add(TComplex.Create( (sqrt_q*2*cos((theta+FOUR_PI)/3) - a_over_3) , 0 ));

      end
     else
      begin

       if (delta > 0) then
        begin

         sqrt_d := sqrt(delta);
         s := cbrt(r+sqrt_d);
         t := cbrt(r-sqrt_d);
         term1 := a_over_3 + ((s+t)/2);

         container.Add(TComplex.Create( ((s+t) - a_over_3) , 0 ));
         container.Add(TComplex.Create( (-term1) , (sqrt(3)*(-t+s)/2) ));
         container.Add(TComplex.Create( (-term1) , -container[1].imagPart ));

        end
       else
        begin

         container.Add(TComplex.Create( (2*cbrt(r) - a_over_3) , 0 ));
         container.Add(TComplex.Create( (cbrt(r) - a_over_3) , 0 ));
         container.Add(TComplex.Create( (cbrt(r) - a_over_3) , 0 ));

        end;
      end;

   end;

end;

{ TFourthDegree }

constructor TFourthDegree.Create(const a, b, c, d, e: double);
begin
  inherited Create([a, b, c, d, e]);

  if (a = 0) then
    raise TEquationError.Create('The first term cannot be zero.');

  Fa := e;
  Fb := d;
  Fc := c;
  Fd := b;
  Fe := a;
end;

function TFourthDegree.getDelta: double;
var k, phi, rho: double;
begin
  k := Fb*Fb*Fc*Fc*Fd*Fd - 4.0*Fd*Fd*Fd*Fb*Fb*Fb - 4.0*Fd*Fd*Fc*Fc*Fc*Fa +
       18.0*Fd*Fd*Fd*Fc*Fb*Fa - 27.0*Fd*Fd*Fd*Fd*Fa*Fa + 256.0*Fe*Fe*Fe*Fa*Fa*Fa;
  phi := Fe*(-4.0*Fc*Fc*Fc*Fb*Fb + 18.0*Fd*Fc*Fb*Fb*Fb + 16.0*Fc*Fc*Fc*Fc*Fa -
         80.0*Fd*Fc*Fc*Fb*Fa - 6.0*Fd*Fd*Fb*Fb*Fa + 144.0*Fd*Fd*Fa*Fa*Fc);
  rho := Fe*Fe*(-27*Fb*Fb*Fb*Fb + 144*Fc*Fb*Fb*Fa - 128*Fc*Fc*Fa*Fa - 192*Fd*Fb*Fa*Fa);

  Result := k + phi + rho;
end;

function TFourthDegree.GetType: TEquationType;
begin
  Result := TEquationType.FourthDegree;
end;

procedure TFourthDegree.SolveEquation(const container: TEquationSol);
var a, b, c, d, e: TComplex;
    Q1, Q2, Q3, Q4, Q5, Q6, Q7, temp: TComplex;
begin

  if Assigned(container) then
   begin

     a := TComplex.Create( Fa, 0 );
     b := TComplex.Create( Fb/Fa, 0 );
     c := TComplex.Create( Fc/Fa, 0 );
     d := TComplex.Create( Fd/Fa, 0 );
     e := TComplex.Create( Fe/Fa, 0 );

     Q1 := c * c - 3.0 * b * d + 12.0 * e;
     Q2 := 2.0 * c * c * c - 9.0 * b * c * d + 27.0 * d * d + 27.0 * b * b * e - 72.0 * c * e;
     Q3 := 8.0 * b * c - 16.0 * d - 2.0 * b * b * b;
     Q4 := 3.0 * b * b - 8.0 * c;

     temp := Q2 * Q2 / 4.0 - Q1 * Q1 * Q1;
     Q5 := TComplex.power( temp.Sqrt + Q2 / 2.0 , 1.0/3.0);
     Q6 := (Q1 / Q5 + Q5) / 3.0;
     temp := Q4 / 12.0 + Q6;
     Q7 := temp.Sqrt * 2.0;

     container.Clear;
     temp := (4.0 * Q4 / 6.0 - 4.0 * Q6 - Q3 / Q7);
     container.Add( ((-b - Q7 - temp.Sqrt) / 4.0) );
     container.Add( ((-b - Q7 + temp.Sqrt) / 4.0) );
     temp := (4.0 * Q4 / 6.0 - 4.0 * Q6 + Q3 / Q7);
     container.Add( ((-b + Q7 - temp.Sqrt) / 4.0) );
     container.Add( ((-b + Q7 + temp.Sqrt) / 4.0) );

   end;

end;

{ TNthDegree }

constructor TNthDegree.Create(const params: array of double);
begin
  inherited Create(params);

  FAreCPSetted := false;
  FSolver := TSolverType.TNewtonRaphson;
  TRFList := TDictionary<TSolverType, TRootFindAlgorithm>.Create;

  Init;
end;

destructor TNthDegree.Destroy;
begin
  TRFList.Free;
  inherited Destroy;
end;

function TNthDegree.GetCoeff(Index: integer): double;
begin
  Result := GetPolynomial.Coeff[Index];
end;

function TNthDegree.GetType: TEquationType;
begin
  Result := TEquationType.NthDegree;
end;

procedure TNthDegree.Init;
begin

  //points contains 1 value (x0, the starting point of the algorithm)
  TRFList.Add(TSolverType.TNewtonRaphson,
              function(const points: array of double): double
              var x, eps: double;
              begin

                if (Length(points) <> 2) then
                  TEquationError.Create('The input array must have 2 values: x0 (starting point) and eps (the accuracy).');

                if (points[1] = 0) then
                  eps := 1.0E-8
                else
                  eps := points[1];

                Result := points[0];
                repeat
                  x := Result;
                  Result := x - GetPolynomial.EvaluateOn(x) / GetPolynomial.GetDerivative.EvaluateOn(x);
                until (abs(Result - x) >= eps);

              end);

  //points contains 2 values (x0 (lower bound), x1 (upper bould) and the precision)
  TRFList.Add(TSolverType.TSecant,
              function(const points: array of double): double
              var a, b, eps: double;
              begin

                if (Length(points) <> 3) then
                  TEquationError.Create('The input array must have 3 values: x0 (lower bound), x1 (upper bound) and eps (the precision).');

                a := points[0];
                b := points[1];
                eps := points[2];

                repeat
                  Result := b - (b-a)/( GetPolynomial.EvaluateOn(b) - GetPolynomial.EvaluateOn(a) ) * GetPolynomial.EvaluateOn(b);

                  if (Poly(Result, points) = 0) then
                   begin
                     Exit;
                   end;

                until (abs(Result-b) >= eps);

              end);

end;

procedure TNthDegree.SetPoints(const k: array of double);
var i: integer;
begin

  FAreCPSetted := true;
  SetLength(FCriticalPoints, Length(k));

  for i := Low(k) to High(k) do
    FCriticalPoints[i] := k[i];

end;

procedure TNthDegree.SetSolverType(const x: TSolverType);
begin
  FSolver := x;
end;

procedure TNthDegree.SolveEquation(const container: TEquationSol);
begin

  if not FAreCPSetted then
    raise TEquationError.Create('You have to call SetPoints() to setup the critical points to start the algorithm!');

  if Assigned(container) then
   begin
     container.Clear;
     container.Add(TComplex.Create(TRFList.Items[FSolver](FCriticalPoints), 0));
   end;

end;

end.
