# TEquation

This is a library written in Delphi that works with VCL and FMX and can be used to solve equations `f(x) = 0` of any degree (from 1st degree to the Nth degree). There are 2 main folders:

 1. <b>Sources</b>. It contains only the source files of the library.
 2. <b>Component</b>. It is a simple component that you can drag and drop in the form; it offers an interface to the library.

# Usage

It's really simple. Just drop a TButton and a TEquationSolver component in the form. Double click the button and add the following code to solve a trivial equation like `x^2 + 3x + 5 = 0` (no particular libraries under the uses list are needed).

``` Delphi
procedure TForm1.Button1Click(Sender: TObject);
var a: integer;
    sol: TEquationSol;
begin

 Memo1.Clear;

 sol := TEquationSol.Create;
 try
   
   //Input of x^2 + 3x + 5 = 0
   EquationSolver1.SolveEquation([5, 3, 2], sol, []);

   for a := 0 to sol.Count-1 do
     Memo1.Lines.Add(sol[a].ToString);
   
   //now Memo1 contains the 2 solutions of the equation
   
 finally
   sol.Free;
 end;

end;
```

The coefficients of an equation in input must be listed in ascending order according with their degree; in fact as you can see above the correct input for `x^2 + 3x + 5 = 0` is `[5, 3, 2]` and <b>not</b> `[2, 3, 5]`. The component is easy to use, once you've put the component on the form you just need a few lines of code. 

When the degree of the equation is 1, 2, 3, or 4 the last parameter of the `SolveEquation` method can be just an empty array, otherwise you must input some points. I've said "some" because according with the root finding algorithm you've chosen there is a specified order/amount of points that you have to enter.

# Notes

 1. To solve an equation use the `SolveEquation()` method; the first parameter is an open array that accepts the coefficients in input, the second parameter is the container of the solutions and the third parameter is an open array that contains the critical points for the root finding algorithm you've chosen.
 
 2. When the degree of the equation is >= 5 the component uses a root finding algorithm. You can setup the algorithm by setting the property `RootFindAlgorithm`; the default value is `TNewtonRaphson` which is in general a good algorithm (but it doesn't guarantee a 100% convergence!).
 
 3. When the degree of the equation is < 5 the component uses some standard algorithms to find the roots. By the way if don't agree and you want to use a root finding algorithm just call the method `ForceRootFindAlgorithm(true)`. In this way (for example) a 3rd degree equation will be solved using the specified root finding algorithm.

# Boring notes

If you aren't a math expert maybe you might be wondering why I am using root finding algorithms to solve 5th degree equations (an higher). To make a long story short: because I am forced! Very easily: there are formulas to solve 1st, 2nd, 3rd and 4th degree equations (the latter is very <b>COMPLEX</b> :p). There cannot be algebraic formulas to solve 5th and higher degree equations (ask google about Ruffini and Galois)

# Code

I have provided only 2 famous root finding algorithms but if you want to create your own version or add/remove methods just do the following. Open the `Equation.pas` file and go to  the `Init` procedure; it looks like this:

``` delphi
  //points contains 1 value (x0, the starting point of the algorithm)
  TRFList.Add(TSolverType.TNewtonRaphson,
              function(const points: array of double): double
              begin
                //code...
              end);

  //points contains 2 values (x0 (lower bound), x1 (upper bould) and the precision)
  TRFList.Add(TSolverType.TSecant,
              function(const points: array of double): double
              begin
               //code...
              end);
```

The `TRFList` is a `TDictionary` that contains the algorithm type (an enum) and its implementation (an anonymous function). If you want to add a new algorithm just add the new name in the `type TSolverType` (top of the file) and call the Add method on `TRFList`. Use the open array input of the anonymous function to get the points that your algorithm needs.
