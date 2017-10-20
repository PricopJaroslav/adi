
/*------------------------------------------------------------------------
   File        : beUser
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : demo
   Created     : Tue Oct 17 09:15:37 EEST 2017
   Notes       :
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS beUsers
    INHERITS DataLayer:
    {dsUsers.i}
    METHOD OVERRIDE PUBLIC VOID FetchData(OUTPUT DATASET-HANDLE dsDataset):

        getUser (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE).

    END METHOD.

    METHOD PUBLIC VOID getUser (INPUT-OUTPUT DATASET dsUsers):

        FOR EACH Users NO-LOCK:
            CREATE ttUsers.
            BUFFER-COPY Users TO ttUsers.
        END.
        
    END METHOD.

    METHOD OVERRIDE PUBLIC VOID FetchData (OUTPUT DATASET-HANDLE dsDataSet, INPUT iFilterValue AS CHARACTER):

        getUser(INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE, INPUT iFilterValue).

    END METHOD.

    METHOD PUBLIC VOID getUser (INPUT-OUTPUT DATASET dsUsers, INPUT iFilterValue AS CHARACTER):

    END METHOD.
    METHOD PUBLIC VOID SaveRecord (INPUT-OUTPUT DATASET dsUsers):
    DEFINE DATA-SOURCE srcUser FOR Users.

        BUFFER ttUsers:ATTACH-DATA-SOURCE(DATA-SOURCE srcUser:HANDLE).
  
        
        FOR EACH ttUsersBefore:

            BUFFER ttUsersBefore:SAVE-ROW-CHANGES().
        END.
    END METHOD.
        
/*    METHOD PUBLIC VOID BeforeStoreData (input-output dataset dsUsers):                                 */
/*                                                                                                       */
/*        for each ttUsersBefore no-lock on error undo, throw:                                           */
/*            if (row-state(ttUsersBefore) = row-modified or row-state(ttUsersBefore) = row-created) then*/
/*            do:                                                                                        */
/*                find first ttUsers no-lock                                                             */
/*                                                                                                       */
/*                    where rowid(ttUsers) = buffer ttUsersBefore:after-rowid no-error.                  */
/*                                                                                                       */
/*                if not avail ttUsers then                                                              */
/*                    return.                                                                            */
/*                case buffer ttUsersBefore:row-state:                                                   */
/*                                                                                                       */
/*                    when 3 /* create**/                                                                */
/*                    then                                                                               */
/*                        do:                                                                            */
/*                            ttUsers.UserNum = next-value (Next_UserNum).                               */
/*                            MESSAGE ttUsers.UserNum                                                    */
/*                                VIEW-AS ALERT-BOX.                                                     */
/*                        END.                                                                           */
/*                /*FOR UPDATE: id remains the same */                                                   */
/*                                                                                                       */
/*                end.                                                                                   */
/*            end.                                                                                       */
/*        end.                                                                                           */
/*    END METHOD.                                                                                        */
END CLASS.