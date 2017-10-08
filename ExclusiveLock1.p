
/*------------------------------------------------------------------------
    File        : ExclusiveLock.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 28 14:21:01 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FIND Customer 1 EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY Customer.Name.
WAIT-FOR CLOSE OF THIS-PROCEDURE.    
    

    
    
    
    
    
    
    
