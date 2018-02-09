unit EquationSolver;

interface

uses
 System.Classes, Equation, Fraction, Complex, Generics.Collections;

type
 TEquationSol = TList<TComplex>;

type
 [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
 TEquationSolver = class(TComponent)
   private
     FRFAlgorithm: boolean;
     FEquation: TEquation;
     FSolver: TSolverType;
   public
     constructor Create(AOwner: TComponent); override;
     procedure ForceRootFindAlgorithm(const aValue: boolean);
     procedure SolveEquation(const FParams: array of double; Solutions: TEquationSol; FCriticalPoints: array of double); overload;
     procedure SolveEquation(const FParams: array of string; Solutions: TEquationSol; FCriticalPoints: array of double); overload;
   published
     property RootFindAlgorithm: TSolverType read FSolver write FSolver;
 end;

 procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('AlbertoComp.', [TEquationSolver]);
end;

{ TEquationSolver }

constructor TEquationSolver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRFAlgorithm := false;
end;

procedure TEquationSolver.ForceRootFindAlgorithm(const aValue: boolean);
begin
  FRFAlgorithm := aValue;
end;

procedure TEquationSolver.SolveEquation(const FParams: array of string;
  Solutions: TEquationSol; FCriticalPoints: array of double);
var x: array of double;
    i: integer;
begin

  SetLength(x, Length(FParams));
  for I := Low(FParams) to High(FParams) do
    x[i] := TFraction.Create(FParams[i]);

  SolveEquation(x, Solutions, FCriticalPoints);

end;

procedure TEquationSolver.SolveEquation(const FParams: array of double;
  Solutions: TEquationSol; FCriticalPoints: array of double);
var len: integer;
    NthDegree: TNthDegree;
begin

  len := Length(FParams);

  if ((len = 0) or (len = 1)) then
    raise TEquationError.Create('The length of the array must be at least 2!');

  //Creation of the proper equation object
  case len of
    2: begin
         FEquation := TFirstDegree.Create(FParams[0], FParams[1]);
       end;
    3: begin
         FEquation := TSecondDegree.Create(FParams[0], FParams[1], FParams[2]);
       end;
    4: begin
         FEquation := TThirdDegree.Create(FParams[0], FParams[1], FParams[2], FParams[3]);
       end;
    5: begin
         FEquation := TFourthDegree.Create(FParams[0], FParams[1], FParams[2], FParams[3], FParams[4]);
       end
    else
       begin
         FEquation := TNthDegree.Create(FParams);
       end;
  end;

  //solve the equation
  try

    //The degree of the equation is >= 5
    if ((len > 5) or FRFAlgorithm) then
      begin
        NthDegree := (FEquation as TNthDegree);
        NthDegree.SetSolverType(FSolver);
        NthDegree.SetPoints(FCriticalPoints);
        NthDegree.SolveEquation(Solutions);
      end
    else
      begin
        FEquation.SolveEquation(Solutions);
      end;

  finally
    FEquation.Free;
  end;

end;

end.
