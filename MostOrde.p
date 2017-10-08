/*
4. Orasul din care s-au dat cele mai  multe comenzi.
*/
DEFINE BUFFER BufCustomer     FOR Customer .  
DEFINE BUFFER BufFunCustomer  FOR Customer .
DEFINE BUFFER BufOrder        FOR Order .  


DEFINE VARIABLE iMostNumOrd  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCityMostOrd AS CHARACTER NO-UNDO.

FUNCTION NrOrders RETURNS INTEGER (INPUT City AS CHARACTER):
    DEFINE VARIABLE iNumOrd AS INTEGER   NO-UNDO.
    FOR EACH BufFunCustomer WHERE BufFunCustomer.City = City NO-LOCK,
        EACH BufOrder       WHERE    BufOrder.CustNum = BufFunCustomer.CustNum NO-LOCK:
            iNumOrd = iNumOrd + 1.
    END.
    RETURN iNumOrd.
END.


FOR EACH BufCustomer NO-LOCK BREAK BY BufCustomer.City :
    IF FIRST-OF(BufCustomer.City) THEN 
        DO:
            IF ( iMostNumOrd < NrOrders(BufCustomer.City)) THEN
                DO:
                    iMostNumOrd  = NrOrders(BufCustomer.City).
                    cCityMostOrd = BufCustomer.City.
                END.
        END.
END.
DISPLAY cCityMostOrd FORMAT "X(20)" LABEL "City with most orders" .
                    
            
