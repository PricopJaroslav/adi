/*
    • Creati o functie care citeste toate recordurile dintro 
    tabela la alegere si le tipareste intru fisier txt cate 
    un record pe linie, creati apoi o a 2 a functie care sa 
    citeasca acest fisier si sa creeze in baza de date un record 
    pentru fiecare linie din fisier
*/
DEFINE BUFFER BufCustomer FOR Customer.
/*
Functia tipareste recordurile dintr-o tabela intr-un fisier .txt
Returneaza un mesaj
*/
FUNCTION Writetxt RETURN CHARACTER:
    OUTPUT TO OutCust.txt.
    
    FOR EACH BufCustomer WHERE BufCustomer.State = "CJ" NO-LOCK:
        EXPORT BufCustomer.
    END.
    
    OUTPUT CLOSE.
    
    RETURN "Customers writen to .txt file".
END.

/*
Functia citeste un fisier si creaza un record pt fiecare linie din fisier
Returneaza un mesaj
*/
FUNCTION Readtxt RETURN CHARACTER:
    INPUT FROM OutCust.txt.

    DISABLE TRIGGERS FOR LOAD OF BufCustomer.

    REPEAT:
        CREATE BufCustomer.
        IMPORT BufCustomer.
    END.

    INPUT CLOSE.
    
    RETURN "Customers imported from .txt file".
END.

DISPLAY Writetxt() FORMAT "X(30)".

FOR EACH BufCustomer WHERE BufCustomer.State = "CJ" EXCLUSIVE-LOCK:
    DELETE BufCustomer.
END.
DISPLAY Readtxt()  FORMAT "X(40)".