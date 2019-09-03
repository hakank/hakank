/*

  Knight tour problem using circuit/1 in SWI Prolog

  This model implements a knight tour for NxN matrices (where N*N is even)
  using circuit/1.
  
  Note: N must be even to be able to use circuit/1 since every move must alternate 
  between a black and white square.
  When N is odd (and thus N*N is odd) then there is one more black (or white)
  square which makes this impossible.
  
  Adding support for non square matrices as well when Rows*Cols are left as an
  exercise. See my Picat model http://hakank.org/picat/knight_tour_circuit.pl for
  inspiration...

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        N = 16,
        time(once(run_tour(N))),
        nl.

go2 :-
        between(2,2,40,N),
        writeln(n=N),
        time(once(run_tour(N))),
        fail,
        nl.

go2.

%%
%% wrapper for running and printing
%%
run_tour(N) :-
        knight(N,X),
        % writeln("X:"),
        % print_matrix2(X),        
        extract_tour(X,Tour),
        writeln("Tour:"),
        print_matrix2(Tour),
        nl.



%%
%% knight(N,X) 
%%
%% Find a "circuit matrix" basis for a knight tour of a NxN matrix
%% Note: This is NOT the tour. We must convert
%% it to a tour with extract_tour/2.
%% 
knight(N, X) :-
        (N mod 2 #= 1
        ->
         writeln("N must be even. Sorry about that.")
         ;
         true
         ),
        N2 #= N*N,
        new_matrix(N,N,1..N2,X),
        flatten(X,XVars),

        %% restrict the domains of each square
        findall([I,J],
                (between(1,N,I),
                 between(1,N,J)
                ),
                IJs),
        maplist(restrict_domains(X,N,N),IJs),

        circuit(XVars),

        labeling([ffc],XVars).



%%
%% extract the knight tour from the "circuit matrix" X
%%
extract_tour(X, Tour) :-
        length(X, N),
        N2 #= N*N,
        new_matrix(N,N,1..N2,Tour),
        matrix_element(Tour,1,1,1),
        Next #= 1,
        extract_tour_(1,N,Next,X,Tour),
        nl.

/*
  Next = X[1,1],
  I = 1+((Next-1) div Rows),
  J = 1+((Next-1) mod Cols)
  Tour[I,J] := K,
  Next := X[I,J]
*/
extract_tour_(K,N,_Next,_X,_Tour) :- K #> N*N .
extract_tour_(K,N,Next,X,Tour) :-
        % calculate the new I and J values given Next
        I2 #= 1+((Next-1) div N),
        J2 #= 1+((Next-1) mod N),
        %% Tour[I2,J2] = K
        matrix_element(Tour,I2,J2,K),
        % Calculate the next K to check
        matrix_element(X,I2,J2,NextK),
        K2 #= K+1,
        extract_tour_(K2,N,NextK,X,Tour).
        

print_matrix2(X) :-
        maplist(print_row,X),
        nl.

print_row(Row) :-
        maplist(format("~w "),Row),
        nl.


/*
  foreach(I in 1..Rows, J in 1..Cols)
     D = [-1,-2,1,2],
     Dom = [ (I+A-1)*Cols + J+B : A in D, B in D, 
              abs(A) + abs(B) == 3, member(I+A,1..Rows), member(J+B,1..Cols)],
     Dom.len > 0,
     X[I,J] :: Dom
  end,
*/
restrict_domains(X,Rows,Cols,[I,J]) :-
        D = [-1,-2,1,2],

        findall(T,
                (member(A,D),
                 member(B,D),
                 abs(A) + abs(B) #= 3,
                 IA #= I+A,
                 JB #= J+B,
                 IA #>= 1,
                 IA #=< Rows,
                 JB #>= 1,
                 JB #=< Cols,
                 T #= (I+A-1)*Cols + J+B
                ),
                Dom),
        matrix_element(X,I,J,XIJ),
        list_domain_disjunction(Dom,DomDisj),
        XIJ in DomDisj.

