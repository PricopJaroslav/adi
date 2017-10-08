/*
    • Fill dataset with data from Customer, Order & Order lines for 
    a specific customer using datasets
*/

/* Include Temp-table Definitions */
{ttDefs.i}

/* Include ProDataSet Definitions */
{dsDefs.i}

/* Include Data-Source Definitions */
{srcDefs.i}

DEFINE VARIABLE cState        AS CHARACTER NO-UNDO INITIAL "Suite 415".
DEFINE VARIABLE cJsonFileName AS CHARACTER NO-UNDO INITIAL "D:\workspace\exercitii\dsCustOrdOrdline.json".
DEFINE VARIABLE cType         AS CHARACTER NO-UNDO INITIAL "file".


BUFFER  ttCustomer:ATTACH-DATA-SOURCE(DATA-SOURCE srcCustomer:HANDLE,?,?).
BUFFER     ttOrder:ATTACH-DATA-SOURCE(DATA-SOURCE    srcOrder:HANDLE,?,?).
BUFFER ttOrderLine:ATTACH-DATA-SOURCE(DATA-SOURCE srcOrdlLine:HANDLE,?,?).

QUERY qCustomer:QUERY-PREPARE("FOR EACH CUSTOMER WHERE Customer.Address = " + QUOTER(cState)).
DATASET dsCustOrdOrdline:FILL().

PROCEDURE Writejson:
    
    DATASET dsCustOrdOrdline:WRITE-JSON (cType, cJsonFileName, TRUE).
    
END PROCEDURE.
        
RUN Writejson.
