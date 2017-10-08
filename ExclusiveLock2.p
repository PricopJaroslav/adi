
/*------------------------------------------------------------------------
    File        : ExclusiveLock2.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 28 14:41:13 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND Customer 1 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF LOCKED Customer THEN
        MESSAGE "That Customer is Locked." VIEW-AS ALERT-BOX.
    ELSE IF NOT AVAILABLE Customer THEN
        MESSAGE "That Customer was not found." VIEW-AS ALERT-BOX.
    ELSE DO.
        DISPLAY Customer.Name.




/*    IF NOT AVAILABLE Customer THEN                             */
/*    MESSAGE "That customer  isn't available" VIEW-AS ALERT-BOX.*/
/*    ELSE DO.                                                   */
/*    DISPLAY Customer.Name.                                     */
    
