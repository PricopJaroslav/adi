
/*------------------------------------------------------------------------
   File        : DataLayer
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : demo
   Created     : Tue Oct 17 09:09:07 EEST 2017
   Notes       :
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS DataLayer:

    DEFINE VARIABLE dsDataSet AS HANDLE NO-UNDO.

    METHOD PUBLIC VOID DeleteData (INPUT-OUTPUT DATASET-HANDLE dsDataSet):

    END METHOD.

    METHOD PUBLIC VOID FetchData (OUTPUT DATASET-HANDLE dsDataSet):

    END METHOD.

    METHOD PUBLIC VOID FetchData (OUTPUT DATASET-HANDLE dsDataSet, INPUT iFilterValue AS CHARACTER):

    END METHOD.

    METHOD PUBLIC VOID StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataSet):

    END METHOD.

END CLASS.