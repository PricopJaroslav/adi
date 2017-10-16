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
define output parameter icnb1 as character no-undo.  
  
 
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 BUTTON-1 BUTTON-2 BUTTON-3 ~
BUTTON-divide BUTTON-4 BUTTON-5 BUTTON-6 BUTTON-multiply BUTTON-7 BUTTON-8 ~
BUTTON-9 BUTTON-minus BUTTON-dot BUTTON-0 BUTTON-equals BUTTON-plus 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculate C-Win 
FUNCTION calculate RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD displayCharacter C-Win 
FUNCTION displayCharacter RETURNS CHARACTER
  ( ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD displaynumbers C-Win 
FUNCTION displaynumbers RETURNS CHARACTER
  ( ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-0 
     LABEL "0" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-1 
     LABEL "1" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-2 
     LABEL "2" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-3 
     LABEL "3" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-4 
     LABEL "4" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-5 
     LABEL "5" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-6 
     LABEL "6" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-7 
     LABEL "7" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-8 
     LABEL "8" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-9 
     LABEL "9" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-divide 
     LABEL "/" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-dot 
     LABEL "." 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-equals 
     LABEL "=" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-minus 
     LABEL "-" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-multiply 
     LABEL "*" 
     SIZE 10 BY 2.

DEFINE BUTTON BUTTON-plus 
     LABEL "+" 
     SIZE 10 BY 2.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 1 COL 9 COLON-ALIGNED WIDGET-ID 20 AUTO-RETURN 
     BUTTON-1 AT ROW 5 COL 10 WIDGET-ID 2
     BUTTON-2 AT ROW 5 COL 22 WIDGET-ID 16
     BUTTON-3 AT ROW 5 COL 34 WIDGET-ID 10
     BUTTON-divide AT ROW 5 COL 46 WIDGET-ID 28
     BUTTON-4 AT ROW 7.52 COL 10 WIDGET-ID 4
     BUTTON-5 AT ROW 7.52 COL 22 WIDGET-ID 14
     BUTTON-6 AT ROW 7.52 COL 34 WIDGET-ID 8
     BUTTON-multiply AT ROW 7.52 COL 46 WIDGET-ID 30
     BUTTON-7 AT ROW 10 COL 10 WIDGET-ID 18
     BUTTON-8 AT ROW 10 COL 22 WIDGET-ID 12
     BUTTON-9 AT ROW 10 COL 34 WIDGET-ID 6
     BUTTON-minus AT ROW 10 COL 46 WIDGET-ID 32
     BUTTON-dot AT ROW 12.52 COL 10 WIDGET-ID 36
     BUTTON-0 AT ROW 12.52 COL 22 WIDGET-ID 24
     BUTTON-equals AT ROW 12.52 COL 34 WIDGET-ID 26
     BUTTON-plus AT ROW 12.52 COL 46 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.8 BY 20.86 WIDGET-ID 100.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10 ROW 2.5
         SIZE 50 BY 2 WIDGET-ID 200.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 20.86
         WIDTH              = 101.8
         MAX-HEIGHT         = 20.86
         MAX-WIDTH          = 101.8
         VIRTUAL-HEIGHT     = 20.86
         VIRTUAL-WIDTH      = 101.8
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-A:MOVE-AFTER-TAB-ITEM (FILL-IN-1:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-A:MOVE-BEFORE-TAB-ITEM (BUTTON-1:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

ASSIGN 
       FILL-IN-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-0 C-Win
/*ON 0 OF BUTTON-0 IN FRAME DEFAULT-FRAME /* 0 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "0".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-0 C-Win
ON CHOOSE OF BUTTON-0 IN FRAME DEFAULT-FRAME /* 0 */
DO:
FILL-IN-1 = FILL-IN-1 + "0".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
/*ON 1 OF BUTTON-1 IN FRAME DEFAULT-FRAME /* 1 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "1".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* 1 */
DO:
FILL-IN-1 = FILL-IN-1 + "1".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
/*ON 2 OF BUTTON-2 IN FRAME DEFAULT-FRAME /* 2 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "2".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*return.                                        */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* 2 */
DO:
FILL-IN-1 = FILL-IN-1 + "2".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
/*ON 3 OF BUTTON-3 IN FRAME DEFAULT-FRAME /* 3 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "3".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* 3 */
DO:
FILL-IN-1 = FILL-IN-1 + "3".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
/*ON 4 OF BUTTON-4 IN FRAME DEFAULT-FRAME /* 4 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "4".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* 4 */
DO:
FILL-IN-1 = FILL-IN-1 + "4".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
/*ON 5 OF BUTTON-5 IN FRAME DEFAULT-FRAME /* 5 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "5".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* 5 */
DO:
FILL-IN-1 = FILL-IN-1 + "5".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
/*ON 6 OF BUTTON-6 IN FRAME DEFAULT-FRAME /* 6 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "6".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* 6 */
DO:
FILL-IN-1 = FILL-IN-1 + "6".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
/*ON 7 OF BUTTON-7 IN FRAME DEFAULT-FRAME /* 7 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "7".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
ON CHOOSE OF BUTTON-7 IN FRAME DEFAULT-FRAME /* 7 */
DO:
FILL-IN-1 = FILL-IN-1 + "7".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 C-Win
/*ON 8 OF BUTTON-8 IN FRAME DEFAULT-FRAME /* 8 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "8".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 C-Win
ON CHOOSE OF BUTTON-8 IN FRAME DEFAULT-FRAME /* 8 */
DO:
FILL-IN-1 = FILL-IN-1 + "8".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
/*ON 9 OF BUTTON-9 IN FRAME DEFAULT-FRAME /* 9 */*/
/*DO:                                            */
/*FILL-IN-1 = FILL-IN-1 + "9".                   */
/*display FILL-IN-1 with frame FRAME-A.          */
/*display FILL-IN-1 with frame DEFAULT-FRAME.    */
/*END.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* 9 */
DO:
FILL-IN-1 = FILL-IN-1 + "9".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-divide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-divide C-Win
/*ON / OF BUTTON-divide IN FRAME DEFAULT-FRAME /* / */*/
/*DO:                                                 */
/*FILL-IN-1 = FILL-IN-1 + " " + "/" + " ".            */
/*display FILL-IN-1 with frame FRAME-A.               */
/*display FILL-IN-1 with frame DEFAULT-FRAME.         */
/*END.                                                */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-divide C-Win
ON CHOOSE OF BUTTON-divide IN FRAME DEFAULT-FRAME /* / */
DO:
FILL-IN-1 = FILL-IN-1 + " " + "/" + " ".    
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-dot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-dot C-Win
/*ON , OF BUTTON-dot IN FRAME DEFAULT-FRAME /* . */*/
/*DO:                                              */
/*FILL-IN-1 = FILL-IN-1 + ".".                     */
/*display FILL-IN-1 with frame FRAME-A.            */
/*display FILL-IN-1 with frame DEFAULT-FRAME.      */
/*END.                                             */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-dot C-Win
ON CHOOSE OF BUTTON-dot IN FRAME DEFAULT-FRAME /* . */
DO:
FILL-IN-1 = FILL-IN-1 + ".".  
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-equals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-equals C-Win
/*ON = OF BUTTON-equals IN FRAME DEFAULT-FRAME /* = */*/
/*DO:                                                 */
/*    return FILL-IN-1  .                             */
/*END.                                                */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-equals C-Win
ON CHOOSE OF BUTTON-equals IN FRAME DEFAULT-FRAME /* = */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  icnb1 = FILL-IN-1.
  RETURN FILL-IN-1 .
     
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-minus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-minus C-Win
/*ON - OF BUTTON-minus IN FRAME DEFAULT-FRAME /* - */*/
/*DO:                                                */
/*FILL-IN-1 = FILL-IN-1 + " " + "-" + " ".           */
/*display FILL-IN-1 with frame FRAME-A.              */
/*display FILL-IN-1 with frame DEFAULT-FRAME.        */
/*END.                                               */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-minus C-Win
ON CHOOSE OF BUTTON-minus IN FRAME DEFAULT-FRAME /* - */
DO:
FILL-IN-1 = FILL-IN-1 + " " + "-" + " ".    
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-multiply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-multiply C-Win
/*ON * OF BUTTON-multiply IN FRAME DEFAULT-FRAME /* * */*/
/*DO:                                                   */
/*FILL-IN-1 = FILL-IN-1 + " " + "*" + " ".              */
/*display FILL-IN-1 with frame FRAME-A.                 */
/*display FILL-IN-1 with frame DEFAULT-FRAME.           */
/*END.                                                  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-multiply C-Win
ON CHOOSE OF BUTTON-multiply IN FRAME DEFAULT-FRAME /* * */
DO:
FILL-IN-1 = FILL-IN-1 + " " + "*" + " ".    
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-plus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-plus C-Win
/*ON + OF BUTTON-plus IN FRAME DEFAULT-FRAME /* + */*/
/*DO:                                               */
/*FILL-IN-1 = FILL-IN-1 + " " + "+" + " ".          */
/*display FILL-IN-1 with frame FRAME-A.             */
/*display FILL-IN-1 with frame DEFAULT-FRAME.       */
/*END.                                              */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-plus C-Win
ON CHOOSE OF BUTTON-plus IN FRAME DEFAULT-FRAME /* + */
DO:
FILL-IN-1 = FILL-IN-1 + " " + "+" + " ".    
display FILL-IN-1 with frame FRAME-A. 
display FILL-IN-1 with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON LEAVE OF FILL-IN-1 IN FRAME DEFAULT-FRAME
DO:
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DSP C-Win 


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
  DISPLAY FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-1 BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-divide BUTTON-4 BUTTON-5 
         BUTTON-6 BUTTON-multiply BUTTON-7 BUTTON-8 BUTTON-9 BUTTON-minus 
         BUTTON-dot BUTTON-0 BUTTON-equals BUTTON-plus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculate C-Win 


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION displayCharacter C-Win 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION displaynumbers C-Win 


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

