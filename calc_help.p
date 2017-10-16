DEFINE INPUT PARAMETER Expresion AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE cExpression      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dResult          AS DECIMAL    NO-UNDO.


DEFINE TEMP-TABLE tt NO-UNDO /*dummy TT*/
FIELD f1 AS INTEGER.
DEF QUERY q FOR tt. /* dummy query, but required */

FUNCTION GetDecimal RETURNS LOGICAL
(INPUT dValue AS DECIMAL).
dResult = dValue.
RETURN TRUE.
END FUNCTION.
cExpression = Expresion. 


/*Evaluate a 4GL decimal expression */

QUERY q:QUERY-PREPARE("FOR EACH tt WHERE ~
   DYNAMIC-FUNCTION( 'GetDecimal', " + cExpression + ") = TRUE").
QUERY q:QUERY-OPEN().
QUERY q:QUERY-CLOSE.
 
MESSAGE dResult VIEW-AS ALERT-BOX INFO BUTTONS OK.