
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Sep 29 11:22:31 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*
1. Suma de plata pentru fiecare ultima comanda a Clientilor din Florida.
*/
DEFINE BUFFER BufCustomer FOR Customer .
DEFINE BUFFER BufOrder    FOR Order .  
DEFINE BUFFER BufOrderLine FOR OrderLine.

DEFINE VARIABLE ITotal AS INTEGER NO-UNDO.

OUTPUT TO LastOrderOfCust.
FOR EACH BufCustomer WHERE BufCustomer.State = "FL" NO-LOCK:
    DISPLAY BufCustomer.Name.
    FIND LAST BufOrder WHERE BufOrder.CustNum = BufCustomer.CustNum NO-LOCK NO-ERROR.
    IF AVAILABLE BufOrder THEN
        ITotal = 0.
        FOR EACH BufOrderLine WHERE BufOrderLine.Ordernum = BufOrder.Ordernum NO-LOCK.
            ITotal = ITotal + BufOrderLine.Qty *( BufOrderLine.Price - BufOrderLine.Discount * BufOrderLine.Price / 100).
        END.
         DISPLAY ITotal.
END.

OUTPUT CLOSE.
DISPLAY "Finished".