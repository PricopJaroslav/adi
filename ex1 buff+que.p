/*
1.Afisati numele tuturor clientilor (Customer) cu comenzi(Order) date in ultima zi a lunii, precum si data comenzii si ziua din saptamana in care a fost data comanda.
*/

DEFINE VARIABLE DayList AS CHARACTER NO-UNDO INITIAL "SUN,MON,TUE,WED,THU,FRI,SAT".
DEFINE BUFFER BufCustomer FOR Customer.
DEFINE BUFFER BufOrder	  FOR Order.

DEFINE QUERY QueCustOrder FOR BufCustomer, BufOrder.


OPEN QUERY QueCustOrder 
  FOR EACH BufCustomer NO-LOCK,
      EACH BufOrder OF BufCustomer NO-LOCK.
GET FIRST QueCustOrder.

DO WHILE NOT QUERY-OFF-END('QueCustOrder'):
	IF (MONTH(BufOrder.OrderDate + 1) <> MONTH(BufOrder.OrderDate)) THEN
    	DISPLAY 
		BufCustomer.Name 
    	        BufOrder.OrderDate  
		ENTRY(WEEKDAY(BufOrder.OrderDate),DayList) LABEL "Day"
		WITH FRAME CustFrame 15 DOWN.
	GET NEXT QueCustOrder.
	DOWN WITH FRAME CustFrame.
END.
