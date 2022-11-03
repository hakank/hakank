
predicates
    sl†_hop(string,string, string)


clauses
    sl†_hop(X,Y, Z) if
         concat(X, Y, Z).            
goal
    write("1 "),readln(X), write("2 "),readln(Y),
    sl†_hop(X,Y,Z), 
    str_len(Z, A), write(A, " "), frontstr(3, Z, B, C),
    write(Z, "\n", B, "\n", C).










