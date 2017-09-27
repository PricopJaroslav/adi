/*
3. Calculati totalul Comenzilor date de clientii din afara statului Florida.
*/

DEFINE VARIABLE ITotal AS INTEGER NO-UNDO.   

FOR EACH Customer WHERE Customer.State <> "FL" NO-LOCK,
    EACH Order OF Customer NO-LOCK,
    EACH Orderline OF Order NO-LOCK:
    ITotal = ITotal + OrderLine.Qty * ( OrderLine.Price - OrderLine.Price * ( OrderLine.Discount / 100 )).

END.
DISPLAY ITotal.