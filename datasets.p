/*
    • Update/delete data on Customer, Order & Order lines using data-sets
    • Creati o functie care primeste ca si input un temp-table Customer apoi
      parcurce recordurile care au fost sterse din temp-table , pentru fiecare
      customer sters sa se verifice daca exista recorduri in tabela Order 
      pentru acel customer , daca exista sa se puna un mesaj de eroare pe 
      ecran:  Customer “X” has active orders and cannot be deleted
*/


/* Include Temp-table Definitions */
{ttDefs.i}

/* Include ProDataSet Definitions */
{dsDefs.i}

/* Include Data-Source Definitions */
{srcDefs.i}


BUFFER  ttCustomer:ATTACH-DATA-SOURCE(DATA-SOURCE srcCustomer:HANDLE,?,?).
BUFFER     ttOrder:ATTACH-DATA-SOURCE(DATA-SOURCE    srcOrder:HANDLE,?,?).
BUFFER ttOrderLine:ATTACH-DATA-SOURCE(DATA-SOURCE srcOrdlLine:HANDLE,?,?).

QUERY qCustomer:QUERY-PREPARE("FOR EACH CUSTOMER").
DATASET dsCustOrdOrdline:FILL().


/*
The function verifies all the deleted customers from the temp-table,
if they have active orders it will display "Customer X has active orders and 
cannot be deleted" if the customer doesn't have orders it saves the changes
 */
FUNCTION VerifOrder RETURN LOGICAL (INPUT TABLE ttCustomer):
    FOR EACH ttCustomerBefore WHERE ROW-STATE(ttCustomerBefore) = 1:
        FIND FIRST Order WHERE Order.CustNum = ttCustomerBefore.CustNum NO-LOCK NO-ERROR.
            IF AVAILABLE Order THEN
                DISPLAY "Customer" ttCustomerBefore.Name "has active orders and cannot be deleted".
            ELSE 
            DO:
                DISPLAY "Al orders and info of customer " ttCustomerBefore.Name "deleted".
                BUFFER ttCustomerBefore:SAVE-ROW-CHANGES() NO-ERROR.
            END.
                
    END.
END.


TEMP-TABLE ttCustomer:TRACKING-CHANGES = TRUE.

/*
Delets customers that have no address
*/
FOR EACH ttCustomer  WHERE 
    ((ttCustomer.Address = ?) 	OR
     (ttCustomer.Address = "")) AND
    ((ttCustomer.Address2 = ?)  OR
     (ttCustomer.Address2 = "")) EXCLUSIVE-LOCK:

    FOR EACH ttOrder WHERE ttOrder.CustNum = ttCustomer.CustNum EXCLUSIVE-LOCK:
        FOR EACH ttOrderLine WHERE ttOrderLine.Ordernum = ttOrder.Ordernum EXCLUSIVE-LOCK:
            DELETE ttOrderLine.
        END.
        DELETE ttOrder.
   END.
   DISPLAY ttCustomer.Name.
   DELETE ttCustomer.
END.


TEMP-TABLE ttCustomer:TRACKING-CHANGES = FALSE.

VerifOrder(TABLE ttCustomer BY-REFERENCE).

TEMP-TABLE ttCustomer:TRACKING-CHANGES = TRUE.

/*
Updates addresses of customers from the state CJ 
*/
FOR EACH ttCustomer  WHERE ttCustomer.State = "CJ" EXCLUSIVE-LOCK:
         DISPLAY ttCustomer.Name.
         UPDATE ttCustomer.Address.
END.

TEMP-TABLE ttCustomer:TRACKING-CHANGES = FALSE.

/*
Saves all the updated recors
*/
FOR EACH ttCustomerBefore WHERE ROW-STATE(ttCustomerBefore) = 2:
    BUFFER ttCustomerBefore:SAVE-ROW-CHANGES () NO-ERROR.
END.

BUFFER  ttCustomer:DETACH-DATA-SOURCE ().
BUFFER     ttOrder:DETACH-DATA-SOURCE ().
BUFFER ttOrderLine:DETACH-DATA-SOURCE ().


