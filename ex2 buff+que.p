/*
2. Afisati numele tuturor angajatilor (Employee) care locuiesc pe strada "Springs Rd".
*/

DEFINE BUFFER BufEmployee FOR Employee.

DEFINE QUERY QueEmployee FOR BufEmployee.

OPEN QUERY QueEmployee
	FOR EACH BufEmployee WHERE BufEmployee.Address MATCHES "*Springs Rd" NO-LOCK.
GET FIRST QueEmployee.

DO WHILE NOT QUERY-OFF-END('QueEmployee'):
    	DISPLAY 
			BufEmployee.LastName 
			BufEmployee.FirstName
			WITH FRAME CustFrame 15 DOWN.
		GET NEXT QueEmployee.
		DOWN WITH FRAME CustFrame.
END.