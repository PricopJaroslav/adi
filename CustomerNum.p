/*
2. Strada si numarul  fiecarui client  din statele care incep cu N 
(folosind 2 functii pentru citirea strazii si citirea numarului)
*/

DEFINE VAR cAddressNum AS CHARACTER NO-UNDO.
DEFINE VAR cAddress AS CHARACTER NO-UNDO.

DEFINE BUFFER BufCustomer  FOR Customer.

FUNCTION StreetNumber RETURNS CHARACTER (INPUT Address AS CHARACTER):
    cAddressNum = STRING(INTEGER(ENTRY(1, Address, " ")))NO-ERROR.  
    IF ERROR-STATUS:ERROR THEN 
        RETURN "NO NUMBER".
    ELSE 
        RETURN cAddressNum. 
END.

  

FUNCTION StreetAddress RETURNS CHARACTER (INPUT Address AS CHARACTER):
    cAddress = StreetNumber(Address).
    IF (cAddress <> "NO NUMBER") THEN
        RETURN SUBSTRING(Address, LENGTH(cAddress) + 1 ).
    ELSE 
        RETURN Address.
END.        
OUTPUT TO ADDRESS.

FOR EACH BufCustomer WHERE BufCustomer.State BEGINS "N" NO-LOCK.

    DISPLAY     StreetAddress (BufCustomer.Address) FORMAT "X(20)"
                StreetNumber(BufCustomer.Address) FORMAT "X(9)".
END.  

OUTPUT CLOSE.
DISPLAY "Finished".  