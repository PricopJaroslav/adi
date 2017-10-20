 
 /*------------------------------------------------------------------------
    File        : beChalangLine
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : demo
    Created     : Thu Oct 19 19:44:56 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beChalangLine 
    INHERITS DataLayer: 
    {dsChalangeLine.i}
    METHOD OVERRIDE PUBLIC VOID FetchData(OUTPUT DATASET-HANDLE dsDataset):

        getChalangeLine (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE).

    END METHOD.

    METHOD PUBLIC VOID getChalangeLine (INPUT-OUTPUT DATASET dsChalangeLine):

        FOR EACH ChalangeLine NO-LOCK:
            CREATE ttChalangeLine.
            BUFFER-COPY ChalangeLine TO ttChalangeLine.
        END.
        
    END METHOD.
    
    METHOD PUBLIC VOID SaveRecord (INPUT-OUTPUT DATASET dsChalangeLine):

    DEFINE DATA-SOURCE srcChalangeLine FOR ChalangeLine.

        BUFFER ttChalangeLine:ATTACH-DATA-SOURCE(DATA-SOURCE srcChalangeLine:HANDLE).
  
        
        FOR EACH ttChalangeLineBefore:

            BUFFER ttChalangeLineBefore:SAVE-ROW-CHANGES().
        END.
    END METHOD.
END CLASS.