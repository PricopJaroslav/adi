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
DEFINE OUTPUT PARAMETER cOut AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE AddUser AS CHARACTER NO-UNDO INITIAL "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Intern-chek Mentor-chek First-Name User-Name ~
E-mail Password Repet-Password Last-Name Create-Buton 
&Scoped-Define DISPLAYED-OBJECTS Intern-chek Mentor-chek First-Name ~
User-Name E-mail Password Repet-Password Last-Name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Create-Buton 
     LABEL "Create" 
     SIZE 30 BY 1.1
     FONT 8.

DEFINE VARIABLE E-mail AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-mail *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE First-Name AS CHARACTER FORMAT "X(50)":U 
     LABEL "First Name *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Last-Name AS CHARACTER FORMAT "X(50)":U 
     LABEL "Last Name *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Repet-Password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Repet Password *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE User-Name AS CHARACTER FORMAT "X(50)":U 
     LABEL "User Name *" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Intern-chek AS LOGICAL INITIAL no 
     LABEL "Intern" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Mentor-chek AS LOGICAL INITIAL no 
     LABEL "Mentor" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Intern-chek AT ROW 4.52 COL 63 WIDGET-ID 24
     Mentor-chek AT ROW 3 COL 63 WIDGET-ID 22
     First-Name AT ROW 3 COL 20 COLON-ALIGNED WIDGET-ID 2
     User-Name AT ROW 6 COL 20 COLON-ALIGNED WIDGET-ID 4
     E-mail AT ROW 7.52 COL 20 COLON-ALIGNED WIDGET-ID 12
     Password AT ROW 9 COL 20 COLON-ALIGNED WIDGET-ID 8 PASSWORD-FIELD 
     Repet-Password AT ROW 10.52 COL 20 COLON-ALIGNED WIDGET-ID 10 PASSWORD-FIELD 
     Last-Name AT ROW 4.52 COL 20 COLON-ALIGNED WIDGET-ID 14
     Create-Buton AT ROW 12 COL 22 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.2 BY 14.71
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
         TITLE              = "Create Acount"
         HEIGHT             = 14.71
         WIDTH              = 82.2
         MAX-HEIGHT         = 18
         MAX-WIDTH          = 126.4
         VIRTUAL-HEIGHT     = 18
         VIRTUAL-WIDTH      = 126.4
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
ON END-ERROR OF C-Win /* Create Acount */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create Acount */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Create-Buton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Create-Buton C-Win
ON CHOOSE OF Create-Buton IN FRAME DEFAULT-FRAME /* Create */
DO: 
    IF (E-mail = "" OR First-Name = "" OR Last-Name = "" OR Password = "" OR User-name = "" OR Repet-Password = "") THEN
        DO:
          MESSAGE "All fields must be completed"
          VIEW-AS ALERT-BOX.
        END.
    ELSE IF  (Mentor-chek = YES AND ENTRY(2,E-mail,"@") <> "tss-yonder.com") THEN
        DO:
            MESSAGE "You need to have a tss-yonder E-mail for a mentor acount"
            VIEW-AS ALERT-BOX.
            Repet-Password:SCREEN-VALUE = "".
        END.
    ELSE IF (Password:SCREEN-VALUE <> Repet-Password:SCREEN-VALUE) THEN
        DO:
            Repet-Password:SCREEN-VALUE = "".
            MESSAGE "Incorect password"
            VIEW-AS ALERT-BOX.
        END.
    ELSE IF (Intern-chek = NO AND Mentor-chek = NO)THEN
        DO:
            MESSAGE "You must select a chek-box"
            VIEW-AS ALERT-BOX.
        END.
    ELSE
        DO:
      
            RUN AddUser.

            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY.
        END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME E-mail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL E-mail C-Win
ON LEAVE OF E-mail IN FRAME DEFAULT-FRAME /* E-mail * */
DO:
    E-mail = E-mail:SCREEN-VALUE.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME First-Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL First-Name C-Win
ON LEAVE OF First-Name IN FRAME DEFAULT-FRAME /* First Name * */
DO: 
    First-Name = First-Name:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Intern-chek
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Intern-chek C-Win
ON VALUE-CHANGED OF Intern-chek IN FRAME DEFAULT-FRAME /* Intern */
DO:    
    Intern-chek = LOGICAL(Intern-chek:SCREEN-VALUE).  
    Mentor-chek = not Intern-chek.
    display Mentor-chek with frame  DEFAULT-FRAME.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Last-Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Last-Name C-Win
ON LEAVE OF Last-Name IN FRAME DEFAULT-FRAME /* Last Name * */
DO:
    Last-Name = Last-Name:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Mentor-chek
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mentor-chek C-Win
ON VALUE-CHANGED OF Mentor-chek IN FRAME DEFAULT-FRAME /* Mentor */
DO:
    Mentor-chek = LOGICAL(Mentor-chek:SCREEN-VALUE).
    Intern-chek = not Mentor-chek.
    display Intern-chek with frame  DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Password C-Win
ON LEAVE OF Password IN FRAME DEFAULT-FRAME /* Password * */
DO:
    IF (LENGTH(Password:SCREEN-VALUE) < 8) THEN
    DO:
    MESSAGE "Password must be at least 8 characters long"
    VIEW-AS ALERT-BOX.
    Password:SCREEN-VALUE = "".
    END.
    ELSE 
    Password = Password:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Repet-Password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Repet-Password C-Win
ON LEAVE OF Repet-Password IN FRAME DEFAULT-FRAME /* Repet Password * */
DO:
    Repet-Password = Repet-Password:SCREEN-VALUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME User-Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL User-Name C-Win
ON LEAVE OF User-Name IN FRAME DEFAULT-FRAME /* User Name * */
DO:
    User-Name = User-Name:SCREEN-VALUE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddUser C-Win 
PROCEDURE AddUser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    AddUser = User-Name + "," + E-mail + "," + Password + "," + First-Name + " " + Last-Name + ",".
    IF (Mentor-Chek = YES) THEN
        AddUser = AddUser + "Mentor".
    ELSE 
        AddUser = AddUser + "Intern".
    DEFINE VARIABLE daUsersInstance AS CLASS daUsers.
    daUsersInstance = NEW daUsers().
    daUsersInstance:AddUser(INPUT AddUser).
    
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY Intern-chek Mentor-chek First-Name User-Name E-mail Password 
          Repet-Password Last-Name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Intern-chek Mentor-chek First-Name User-Name E-mail Password 
         Repet-Password Last-Name Create-Buton 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verif C-Win 
PROCEDURE Verif :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

