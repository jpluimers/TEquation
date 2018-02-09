# Usage (Component)

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
