
/*------------------------------------------------------------------------
    File        : ttDefs.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Oct 08 11:18:48 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE ttCustomer LIKE   Customer  BEFORE-TABLE ttCustomerBefore.
DEFINE TEMP-TABLE ttOrder LIKE      Order     BEFORE-TABLE ttOrderBefore.
DEFINE TEMP-TABLE ttOrderLine LIKE  OrderLine BEFORE-TABLE ttOrderLineBefore.
