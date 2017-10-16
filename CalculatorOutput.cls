 
 /*------------------------------------------------------------------------
    File        : CalculatorOutput
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Pricop
    Created     : Sun Oct 15 10:19:51 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS CalculatorOutput: 
    
    DEFINE VARIABLE cEquation AS CHARACTER NO-UNDO.

    CONSTRUCTOR CalculatorOutput ():

    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID displayResult (INPUT cResult AS character):
        RUN C-Win.w (INPUT cResult).

    END METHOD.
END CLASS.