DEFINE VARIABLE cStart      AS CLASS Calculator. 
DEFINE VARIABLE cCalcInput  AS CLASS CalculatorInput. 
DEFINE VARIABLE cCalcOutput AS CLASS CalculatorOutput. 

cCalcInput = NEW CalculatorInput().
/*cCalcOutput = new CalculatorOutput().*/
cStart = NEW Calculator().
cStart:CalcExecute(INPUT cCalcInput) .
