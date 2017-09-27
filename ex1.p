/*
1.Afisati numele tuturor clientilor (Customer) cu comenzi(Order) date in ultima zi a lunii, precum si data comenzii si ziua din saptamana in care a fost data comanda.
*/

DEFINE VARIABLE DayList AS CHARACTER NO-UNDO INITIAL "SUN,MON,TUE,WED,THU,FRI,SAT".
FOR EACH Customer NO-LOCK,
     EACH Order OF Customer NO-LOCK:
        IF (MONTH(Order.OrderDate + 1) <> MONTH(Order.OrderDate)) THEN
            DISPLAY 
			Customer.Name 
    		Order.OrderDate  
			ENTRY(WEEKDAY(Order.OrderDate),DayList) LABEL "Day".
END.   