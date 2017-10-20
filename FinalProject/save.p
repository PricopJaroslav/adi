
/*------------------------------------------------------------------------
    File        : save.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 18 23:45:59 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{ttUsers.i}
DEFINE INPUT PARAMETER TABLE FOR ttUsers.
FOR EACH ttUsers:

      CREATE Users.
      BUFFER-COPY ttUsers TO Users.  /*EXCEPT ttUsers.UserNum */
        
    
END.    