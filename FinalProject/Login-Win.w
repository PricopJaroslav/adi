&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ADI   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE iNum  AS INTEGER       NO-UNDO.
DEFINE VARIABLE verif AS LOGICAL       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS E-mail Password Log-In Create-Acount 
&Scoped-Define DISPLAYED-OBJECTS E-mail Password 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Create-Acount 
     LABEL "Create Acount" 
     SIZE 30 BY 1.19
     FONT 8.

DEFINE BUTTON Log-In 
     LABEL "Log In" 
     SIZE 30 BY 1.19
     FONT 8.

DEFINE VARIABLE E-mail AS CHARACTER FORMAT "X(50)":U 
     LABEL "E-mail" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Password AS CHARACTER FORMAT "X(50)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     E-mail AT ROW 4 COL 18 COLON-ALIGNED WIDGET-ID 2
     Password AT ROW 6 COL 18 COLON-ALIGNED WIDGET-ID 4 PASSWORD-FIELD 
     Log-In AT ROW 8 COL 20 WIDGET-ID 6
     Create-Acount AT ROW 10 COL 20 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.4 BY 13.19
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Log-in"
         HEIGHT             = 13.14
         WIDTH              = 70.4
         MAX-HEIGHT         = 24.29
         MAX-WIDTH          = 195.2
         VIRTUAL-HEIGHT     = 24.29
         VIRTUAL-WIDTH      = 195.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Log-in */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Log-in */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Create-Acount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Create-Acount C-Win
ON CHOOSE OF Create-Acount IN FRAME DEFAULT-FRAME /* Create Acount */
DO:
        RUN CreateAcount-Win.w(OUTPUT ADI).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME E-mail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL E-mail C-Win
ON LEAVE OF E-mail IN FRAME DEFAULT-FRAME /* E-mail */
DO:
        E-mail = E-mail:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Log-In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Log-In C-Win
ON CHOOSE OF Log-In IN FRAME DEFAULT-FRAME /* Log In */
DO:
        /*    DEFINE VARIABLE daUsersInstance AS CLASS daUsers.         */
        /*    daUsersInstance = NEW daUsers().                          */
        /*    daUsersInstance:VerifE-mail(INPUT E-mail ,INPUT Password).*/
        RUN verifEmail.
        IF verif = YES THEN
            APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Password C-Win
ON LEAVE OF Password IN FRAME DEFAULT-FRAME /* Password */
DO:
        Password = Password:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY E-mail Password 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE E-mail Password Log-In Create-Acount 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifEmail C-Win 
PROCEDURE verifEmail :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    FIND FIRST Users WHERE Users.UserE-mail = E-mail AND Users.UserPassword = Password NO-LOCK NO-ERROR.
    IF AVAILABLE Users THEN
    do:
        iNum = Users.UserNum.
        IF Users.UserPosition = "Mentor" THEN
        do:

            RUN Mentor.w.
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY.

        end.
        ELSE
        do:
            
            RUN Intern.w(INPUT iNum).
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY.
        end.
    end.
    ELSE 
        MESSAGE "Incorect E-mail or password"
            VIEW-AS ALERT-BOX.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifpass C-Win 
PROCEDURE verifpass :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*DEFINE INPUT PARAMETER iNum AS INTEGER NO-UNDO. */
    /*DEFINE OUTPUT PARAMETER bOut AS LOGICAL NO-UNDO.*/
    FIND FIRST Users WHERE Users.UserNum = iNum NO-LOCK NO-ERROR.
    IF AVAILABLE Users THEN
    DO:
        IF Users.UserPassword <> Password THEN
        DO:
            verif = NO.
            MESSAGE "Incorect Password"
                VIEW-AS ALERT-BOX.
        END.

       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

