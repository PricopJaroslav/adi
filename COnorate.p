
/*------------------------------------------------------------------------
    File        : COnorate.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Sep 29 13:21:03 EEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*
3. Comenzile care nu au fost onorate in maxim o saptamana
*/
/*
3. Comenzile care nu au fost onorate in maxim o saptamana   
*/

DEFINE BUFFER BufOrder FOR Order . 

FOR EACH BufOrder WHERE BufOrder.OrderDate + 7 < BufOrder.ShipDate .
DISPLAY BufOrder.Ordernum 
        BufOrder.OrderDate
        BufOrder.ShipDate. 
END.