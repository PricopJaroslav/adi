
/*------------------------------------------------------------------------
    File        : dsDefs.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Oct 08 11:22:44 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Include Temp-table Definitions */


DEFINE DATASET dsCustOrdOrdline FOR ttCustomer, ttOrder, ttOrderLine
    DATA-RELATION drCustOrd     FOR ttCustomer, ttOrder  RELATION-FIELDS (CustNum,CustNum)
    DATA-RELATION drOrdOrdLine  FOR ttOrder, ttOrderLine RELATION-FIELDS (Ordernum,Ordernum).
