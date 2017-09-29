/*
1. Mutati primul client din Boston in Florida.
*/

FIND FIRST Customer WHERE Customer.City = "Boston" EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Customer THEN 
        ASSIGN 
            Customer.State = "Florida".
    ELSE
        DISPLAY "No customer in Boston".
 