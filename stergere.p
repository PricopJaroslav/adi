/*
3. stergeti ultimul client din Newark, precum si comenzile lui
*/

FIND LAST Customer WHERE Customer.City = "Newark" EXCLUSIVE-LOCK NO-ERROR.

    FOR EACH Order WHERE Order.CustNum = Customer.CustNum EXCLUSIVE-LOCK.

/*         FOR EACH OrderLine WHERE OrderLine.Ordernum = Order.Ordernum EXCLUSIVE-LOCK.*/
/*        DELETE OrderLine.                                                            */
/*                                                                                     */
/*END.                                                                                 */

DELETE Order.
END.
DELETE Customer.