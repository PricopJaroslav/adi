/*
Transmiteti informatiile dintrun dataset intre 2 proceduri prin 
scrierea si citirea de pe disk a unui .xml si a unui .json.
*/
    

/* Include Temp-table Definitions */
{ttCustomer.i}

/* Include ProDataSet Definitions */
{dsCustomer.i}

/* Include Data-Source Definitions */
{srcCustomer.i}

DEFINE VARIABLE cXmlFileName  AS CHARACTER NO-UNDO INITIAL "D:\workspace\exercitii\dsCustomer.xml".
DEFINE VARIABLE cJsonFileName AS CHARACTER NO-UNDO INITIAL "D:\workspace\exercitii\dsCustomer.json".
DEFINE VARIABLE cType         AS CHARACTER NO-UNDO INITIAL "file".
DEFINE VARIABLE cReadMode     AS CHARACTER NO-UNDO INITIAL "empty".
DEFINE VARIABLE lRetOK        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hCustomer     AS HANDLE    NO-UNDO.

BUFFER ttCustomer:ATTACH-DATA-SOURCE(DATA-SOURCE srcCustomer:HANDLE,?,?).

QUERY qCustomer:QUERY-PREPARE("FOR EACH CUSTOMER").
DATASET dsCustomer:FILL().


/*
Procedura scrie un dataset ca fisier .xml dupa care sterge recordurile din dataset
*/
PROCEDURE Writexml:
    
    DATASET dsCustomer:WRITE-XML (cType, cXmlFileName, TRUE, ?, ?, FALSE, FALSE).
    
    DATASET dsCustomer:EMPTY-DATASET.
    
END PROCEDURE.

/*
Procedura citeste un dataset dintr-un fisier xml dupa care
il scrie intr-un fiseir .json si sterge recordurile din dataset
*/
PROCEDURE ReadxmlWriteJson:
    
    DATASET dsCustomer:READ-XML (cType, cXmlFileName, cReadMode, ?, ?, ?, ?).
    
    DATASET dsCustomer:WRITE-JSON (cType, cJsonFileName, TRUE).
    
    DATASET dsCustomer:EMPTY-DATASET.
    
END PROCEDURE.

/*
Procedura citeste un dataset dintr-un fisier .json
*/
PROCEDURE Readjson:
    
    DATASET dsCustomer:READ-JSON (cType, cJsonFileName, cReadMode).
    
END PROCEDURE.


RUN Writexml.
RUN ReadxmlWriteJson.
RUN Readjson.