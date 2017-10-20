 
/*------------------------------------------------------------------------
   File        : daUsers
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : demo
   Created     : Wed Oct 18 13:07:38 EEST 2017
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS daUsers
    INHERITS beUsers:
    {dsUsers.i}
    DEFINE VARIABLE dsDataAcc AS HANDLE NO-UNDO.
    METHOD PUBLIC LOGICAL VerifE-mail (INPUT cVerifmail AS CHARACTER, INPUT cVerifpass AS CHARACTER):
        FetchData (OUTPUT DATASET-HANDLE dsDataAcc BY-REFERENCE).


        FOR EACH ttUsers .
        MESSAGE ttUsers.UserE-mail
        VIEW-AS ALERT-BOX.
        END.
        FIND ttUsers WHERE ttUsers.UserE-mail = cVerifmail NO-LOCK NO-ERROR.
        IF AVAILABLE ttUsers THEN
            IF ttUsers.UserPassword = cVerifpass THEN
                RETURN YES.
            ELSE
                MESSAGE "Wrong Password"
                VIEW-AS ALERT-BOX.
        ELSE
            MESSAGE "Wrong E-mail"
            VIEW-AS ALERT-BOX.
    END METHOD.
    
    METHOD PUBLIC LOGICAL VerifPassword (INPUT cVerif AS CHARACTER):
        FetchData (OUTPUT DATASET-HANDLE dsDataAcc BY-REFERENCE).
        FIND ttUsers WHERE ttUsers.UserPassword = cVerif.
        IF AVAILABLE ttUsers THEN
            RETURN YES.
        ELSE 
            MESSAGE "Wrong Password"
            VIEW-AS ALERT-BOX.
            RETURN NO.
    END METHOD.
    
    METHOD PUBLIC VOID AddUser (INPUT cUserAdd AS CHARACTER):

        TEMP-TABLE ttUsers:TRACKING-CHANGES = TRUE.
        CREATE ttUsers.
        ASSIGN
            ttUsers.ID           = ENTRY(1,cUserAdd,",")
            ttUsers.UserE-mail   = ENTRY(2,cUserAdd,",")
            ttUsers.UserPassword = ENTRY(3,cUserAdd,",")
            ttUsers.UserName     = ENTRY(4,cUserAdd,",")
            ttUsers.UserPosition = ENTRY(5,cUserAdd,",")
            ttUsers.UserNum      = NEXT-VALUE(Next_UserNum).
        TEMP-TABLE ttUsers:TRACKING-CHANGES = FALSE.
          temp-table ttUsers:WRITE-XML ("file","d:\workspace\ttusers.xml",true).

        
/*        RUN save.p(INPUT TABLE ttUsers).*/
/*        BeforeStoreData (INPUT-OUTPUT DATASET-HANDLE ttUsers BY-REFERENCE).*/
        SaveRecord (INPUT-OUTPUT DATASET dsUsers BY-REFERENCE).
    END METHOD.
    END CLASS.