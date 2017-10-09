
/*------------------------------------------------------------------------
    File        : srcDefs.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Oct 08 11:40:37 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE QUERY qCustomer FOR Customer .
DEFINE DATA-SOURCE srcCustomer FOR QUERY qCustomer
Customer KEYS (Name).
DEFINE DATA-SOURCE srcOrder    FOR Order.
DEFINE DATA-SOURCE srcOrdlLine FOR OrderLine.