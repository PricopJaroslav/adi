 
 /*------------------------------------------------------------------------
    File        : beChalange
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Thu Oct 19 19:32:17 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beChalange
    INHERITS DataLayer: 
    {dsChalange.i}
    METHOD OVERRIDE PUBLIC VOID FetchData(OUTPUT DATASET-HANDLE dsDataset):

        getChalange (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE).

    END METHOD.

    METHOD PUBLIC VOID getChalange (INPUT-OUTPUT DATASET dsChalange):

        FOR EACH Chalage NO-LOCK:
            CREATE ttChalange.
            BUFFER-COPY Chalage TO ttChalange.
        END.
        
    END METHOD.
    
    METHOD PUBLIC VOID SaveRecord (INPUT-OUTPUT DATASET dsChalange):

    DEFINE DATA-SOURCE srcChalange FOR Chalage.

        BUFFER ttChalange:ATTACH-DATA-SOURCE(DATA-SOURCE srcChalange:HANDLE).
  
        
        FOR EACH ttChalangeBefore:

            BUFFER ttChalangeBefore:SAVE-ROW-CHANGES().
        END.
    END METHOD.
END CLASS.