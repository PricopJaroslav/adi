DEFINE INPUT PARAMETER ChalangeName        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ChalangeDescription AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER UserNum             AS INTEGER   NO-UNDO.
DEFINE VARIABLE        iNum                AS INTEGER   NO-UNDO.
DEFINE VARIABLE        iNum2               AS INTEGER   NO-UNDO.
IF (ChalangeName = ""        OR ChalangeName = ?        AND
    ChalangeDescription = "" OR ChalangeDescription = ? AND
    UserNum = 0              OR UserNum = ?) THEN 
DO:
    MESSAGE "All fields must be filled"
        VIEW-AS ALERT-BOX.
END.
ELSE
DO:

    CREATE Chalage.
    ASSIGN 
        iNum                 = NEXT-VALUE(Next_ChalangeNum)
        Chalage.ChalangeName = ChalangeName
        Chalage.Description  = ChalangeDescription
        Chalage.ChalangeNum  = iNum.
    FIND FIRST Chalage WHERE Chalage.ChalangeNum  = iNum NO-LOCK NO-ERROR.
    IF AVAILABLE Chalage THEN
    DO:

        CREATE ChalangeLine.
        ASSIGN
            iNum2 = NEXT-VALUE(Next_ChalangeLineNum)
            ChalangeLine.ChalangeLineNum = iNum2
            ChalangeLine.UserNum         = UserNum
            ChalangeLine.ChalangeNum     = Chalage.ChalangeNum.

    END.
END.