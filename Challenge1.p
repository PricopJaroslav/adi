
/*------------------------------------------------------------------------
    File        : Challenge1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 28 14:26:00 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEF VAR c AS CHARACTER NO-UNDO.
c = "Cluj-Napoca - Berlin".
DEF VAR cField1 AS CHAR NO-UNDO.
DEF VAR cField2 AS CHAR NO-UNDO.
DEF VAR c2 AS CHARACTER NO-UNDO.

c2 = REPLACE (c, " - ", "*").
cField1 = ENTRY (1, c2 , "*").
cField2 = ENTRY(2 , c2, "*").
DISPLAY cField1 FORMAT "X(20)"
        cField2 FORMAT "X(20)".