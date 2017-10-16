 
 /*------------------------------------------------------------------------
    File        : CalculatorInput
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Pricop
    Created     : Sun Oct 15 10:19:37 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS CalculatorInput :
    DEFINE VARIABLE cEquation AS CHARACTER NO-UNDO.

    CONSTRUCTOR CalculatorInput ():
          DEFINE VARIABLE FILL-IN-1 AS CHARACTER NO-UNDO. 

    
          RUN C-Win.w (OUTPUT FILL-IN-1).

          cEquation = FILL-IN-1. 

    END CONSTRUCTOR.
    
   METHOD PUBLIC CHARACTER returnEquation ():

        RETURN cEquation.
    END METHOD.
    

END CLASS.