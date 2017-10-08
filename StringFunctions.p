/*
•   Creati o functie care sa extraga primele 3 caractere dintrun 
    string primit ca input parameter si sa intoarca rezultatul ca si ouput
*/

FUNCTION Firs3Characters RETURNS CHARACTER (INPUT cPrimStr AS CHARACTER):
    RETURN SUBSTRING(cPrimStr , 1, 3).
END.

/*
•   Creati o functie care sa primeasca un input string ca si 
    input parameter si sa intoarca numarul vocalelor din acel string
*/

FUNCTION NumOfVocals RETURNS INTEGER (INPUT cVerif AS CHARACTER):

    DEFINE VARIABLE vocals        AS CHARACTER NO-UNDO INITIAL "a,e,i,o,u".
    DEFINE VARIABLE iStringLength AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumEntries   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumOfVocals  AS INTEGER   NO-UNDO.


    REPEAT iStringLength = 1 TO LENGTH(cVerif):
        REPEAT iNumEntries = 1 TO NUM-ENTRIES(vocals):
            IF (ENTRY(iNumEntries , vocals) = SUBSTRING (cVerif, iStringLength, 1)) THEN 
            DO:
                iNumOfVocals = iNumOfVocals + 1.
            END.
        END.
    END.
    RETURN iNumOfVocals.
END.

/*
•   Creati o functie care sa primeasca ca si input parameter 2 stringuri,
    primul sa fie o lista de cuvinte separate prin separatorul “,” al doilea
    parametru este un singur cunvat , functia trebuie sa afle sis a returneze
    numarul apartiilor cuvantului din parametru 2 in lista de cuvinte parametrul 1
*/

FUNCTION NumAparitii RETURNS INTEGER (INPUT cVerif AS CHARACTER,INPUT cCuvant AS CHARACTER):
    
    DEFINE VARIABLE iNumEntries      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumOfApparances AS INTEGER NO-UNDO.
    
    REPEAT iNumEntries = 1 TO NUM-ENTRIES(cVerif):
        IF (ENTRY(iNumEntries , cVerif) = cCuvant) THEN
        DO:
            iNumOfApparances = iNumOfApparances + 1.
        END.
    END.
    RETURN iNumOfApparances.
END.

/*
    • Creati o functie care sa primeasca ca si input parameter 2 stringuri,
     primul sa fie o lista de cuvinte separate prin separatorul “,” al doilea
     parametru este un singur cunvat , functia trebuie sa gaseasca toate aparitiile
     cuvantului in lista si sa le stearga din lista , apoi sa returneze rezultatul stergerii
*/

FUNCTION DelAparitii RETURNS CHARACTER (INPUT cVerif AS CHARACTER,INPUT cCuvant AS CHARACTER):
    IF ENTRY( NUM-ENTRIES(cVerif),cVerif, ",") = cCuvant THEN
    DO:
        RETURN RIGHT-TRIM(REPLACE (cVerif , cCuvant + "," , ""),"," + cCuvant).
    END.
    ELSE
        RETURN REPLACE (cVerif , cCuvant + "," , "").
END.    
    


    
    


    

DEFINE VAR cVerifEll AS CHARACTER NO-UNDO INITIAL "lenovo".
DEFINE VARIABLE cSir AS CHARACTER NO-UNDO INITIAL "acer,lenovo,dell,hp,lenovo,toshiba,lenovo,dell,acer,acer,lenovo,dell".

DISPLAY Firs3Characters(cVerifEll) LABEL "Primele 3 caractere din sting".

DISPLAY DelAparitii(cSir ,cVerifEll) FORMAT "X(50)" LABEL "Sirul fara".

DISPLAY NumAparitii(cSir ,cVerifEll) LABEL "Numarul de aparitii ale cuvantului ".

DISPLAY NumOfVocals(cVerifEll) LABEL "Numarul de vocale din string "


