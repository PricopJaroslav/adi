/*
3. Calculati totalul Comenzilor date de clientii din afara statului Florida.
*/

DEFINE VARIABLE ITotal AS INTEGER NO-UNDO.

DEFINE BUFFER BufCustomer 	  FOR Customer.
DEFINE BUFFER BufOrder	  	  FOR Order.
DEFINE BUFFER BufOrderLine	  FOR OrderLine.

DEFINE QUERY QueCustOrder FOR BufCustomer, BufOrder, BufOrderLine.

OPEN QUERY QueCustOrder 
  FOR EACH BufCustomer WHERE BufCustomer.State <> "FL" NO-LOCK,
      EACH BufOrder OF BufCustomer NO-LOCK,
      EACH BufOrderLine OF BufOrder NO-LOCK.
GET FIRST QueCustOrder.

DO WHILE NOT QUERY-OFF-END('QueCustOrder'):
    ITotal = ITotal + BufOrderLine.Qty * ( BufOrderLine.Price - BufOrderLine.Price * ( BufOrderLine.Discount / 100 )).
GET NEXT QueCustOrder.
END.
DISPLAY ITotal.
