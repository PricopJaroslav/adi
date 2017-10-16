 
/*------------------------------------------------------------------------
   File        : Calculator
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Pricop
   Created     : Sun Oct 15 10:19:25 EEST 2017
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Calculator: 
        CONSTRUCTOR Calculator():

        END CONSTRUCTOR.
    
    METHOD PUBLIC VOID CalcExecute (
        INPUT  cInputInstance  AS CLASS CalculatorInput
        ):
        DEFINE VARIABLE cExpresion AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cResult    AS CHARACTER NO-UNDO.
        

        cExpresion     = cInputInstance:returnEquation().
        RUN calc_help.p (input cExpresion). 
        
/*        cOutputInstance:displayResult(cResult).*/
        
    END METHOD.




END CLASS.