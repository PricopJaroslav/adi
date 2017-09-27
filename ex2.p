/*
2. Afisati numele tuturor angajatilor (Employee) care locuiesc pe strada "Springs Rd".
*/


FOR EACH Employee WHERE Employee.Address MATCHES "*Springs Rd" NO-LOCK:
    DISPLAY 
		Employee.LastName 
		Employee.FirstName.
END.