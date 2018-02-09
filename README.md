# TEquation

This is a library written in Delphi that works with VCL and FMX and can be used to solve equations `f(x) = 0` of any degree (from 1st degree to the Nth degree). There are 2 main folders:

 1. <b>Sources</b>. It contains only the source files of the library.
 2. <b>Component</b>. It is a simple component that you can drag and drop in the form; it offers an interface to the library. 

The chapter below describes how to use the library only with the <b>Sources</b>, if you want to know how to install and use the component please check the readme in the Component folder.

# Usage

Just as a test, create a new VCL or FMX project. Now in the IDE click Project > Add To Project > Select the 4 source files (`.pas`) > Click ok. I suggest you to create a copy of the sources and put them in the <b>same</b> folder of your project. Now look at this very simple example that shows how to solve `x^2 + 3x + 5 = 0`.

``` Delphi
procedure TForm1.Button1Click(Sender: TObject);
var eq: TEquation;      //Equation solver engine
    sol: TEquationSol;  //Equation solutions container
    i: integer;
begin

  sol := TEquationSol.Create;
  try
    eq := TSecondDegree.Create(5, 3, 2);
    try
      
      //Solve the equation
      eq.SolveEquation(sol);
      
      //Output the results on a memo
      for i := 0 to sol.Count-1 do
        Memo1.Lines.Add(sol[i].ToString);

    finally
      eq.Free;
    end;
  finally
    sol.Free;
  end;

end;
``` 

In the example above the memo contains the two solutions that are `-0,75 + 1,39194109070751i` and `-0,75 - 1,39194109070751i`. The coefficients of an equation in input must be listed in ascending order according with their degree; in fact as you can see above the correct input for `x^2 + 3x + 5 = 0` is `[5, 3, 2]` and <b>not</b> `[2, 3, 5]`. The test example was a quadratic equation (2nd degree equation). Here there is a list of the available classes.

<table width="100%">
 <tr>
  <td style="width: 40%">
   <b>Class</b>
  </td>
  <td style="width: 40%">
   <b>Usage</b>
  </td>
 </tr>
 <tr>
  <td>TFirstDegree</td>
  <td>First degree equation solver</td>
 </tr>
 <tr>
  <td>TSecondDegree</td>
  <td>Second degree equation solver</td>
 </tr>
 <tr>
  <td>TThirdDegree</td>
  <td>Third degree equation solver</td>
 </tr>
 <tr>
  <td>TFourthDegree</td>
  <td>Fourth degree equation solver</td>
 </tr>
 <tr>
  <td>TNthDegree</td>
  <td>Nth degree equation solver</td>
 </tr>
</table>

The usage of an `NthDegree` is a bit different from the usual, here there's a simple example:

``` Delphi

 sol := TEquationSol.Create;
  try
   eq := TNthDegree.Create([5, 3, -2]);
   try
   
     eq.SetPoints([2.2, 3, 1.0E-8]);         //required method call
     eq.SetSolverType(TSolverType.TSecant);  //optional method call
     eq.SolveEquation(sol);
     
     for i := 0 to sol.Count-1 do
       Memo1.Lines.Add(sol[i].ToString);

   finally
     eq.Free;
   end;
 finally
   sol.Free;
 end;

```

You must call the `SetPoints()` method before calling the `SolveEquation()` because each implementation needs some points to work. I've said "some" because according with the root finding algorithm you've chosen there is a specified order/amount of points that you have to enter. 

The default root finding algorithm is `TNewtonRaphson` but here I've changed it calling `SetSolverType(TSolverType.TSecant);` and, since the secant alg. needs 3 starting points (start, end, precision), in the required method I've put them.

# Notes

If you aren't a math expert maybe you might be wondering why I am using root finding algorithms to solve 5th degree equations (an higher). To make a long story short: because I am forced! Very easily: there are formulas to solve 1st, 2nd, 3rd and 4th degree equations (the latter is very <b>COMPLEX</b> :p). There cannot be algebraic formulas to solve 5th and higher degree equations (ask google about Ruffini and Galois). So you have 2 choices:

 1. If you have an equation whose degree is >= 5 you must use the `NthDegree` class with a root finding algorithm.
 2. If you have an equation whose degree is <= 4 you can use the `NthDegree` (because root finding algorithms work with any kind of equation!) or the apposite class listed in the table above.
 
In my opinion you should use the apposite class when you can because it's more precise and has more functionalities (such as the discriminant option). Also keep in mind that root finding algorithms doesn't have a perfect convergence ration ( = they can fail) so, again, in case of deg. <= 4 use the proper class.

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
