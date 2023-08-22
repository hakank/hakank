% https://open.kattis.com/problems/magicalmysteryknight
% 3s
% 9.6s Hard

% NOPE. To messy for now...

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    writeln(l=L),
    maplist(writeln,L),
    maplist(ins2,L,1..64),
    % label(L),
    
    writeln(labeled=L),
    nl.
main.

ins2(L,Domain) :-
    writeln(ins2(L,Domain)),
    L ins Domain.

row([L|Ls]) --> integer(L)," ", row(Ls).
row([L]) --> integer(L).
row([]) --> [].

p([L|Ls]) --> row(L0),{L0 \= [], c(L0,[],L)},"\n",p(Ls).
p([]) --> [].

c([],L,L).
c([H|T],L0,[C|L]) :-
    (H =:= -1 -> C = _ ; C = H),
    c(T,L0,L).


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


%%
%% matrix_element(X, I, J, Val)
%%
%% Matrix[I,J] = Val
%%
matrix_element(X, I, J, Val) :-
        matrix_element2(X,I,J,Val).

%%
%% Different approaches.
%% matrix_element2/4 and matrix_element5/4 seems to work best.
%%
% matrix_element1(X, I, J, Val) :-
%         element(I, X, Row),
%         element(J, Row, Val).

matrix_element2(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


%%
%% circuit_path(Circuit,Path)
%%
%% As circuit/1 but with the path as second parameter.
%%
circuit_path(X,Z) :-
   length(X,N),
   length(Z,N),
   Z ins 1..N,

   %
   % The main constraint is that Z[I] must not be 1 
   % until I = N, and for I = N it must be 1.
   %
   all_different(X),
   all_different(Z),
   % all_distinct(X), % slower
   % all_distinct(Z), % slower

   % put the orbit of x[1] in in z[1..n]
   element(1,X,X1),
   element(1,Z,Z1),
   X1 #= Z1,
   
   % when I = N it must be 1
   element(N,Z,ZN),
   ZN #= 1,

   %
   % Get the orbit for Z.
   %
   numlist(2,N,Is),
   maplist(orbit(X,Z),Is).

%%
%% new_matrix(NumRows, NumCols, Domain, Matrix)
%%
%% Create a matrix of dimension NumRows x NumCols and with domain Domain.
%%
new_matrix(NumRows, NumCols, Domain, Matrix) :-
        length(Cols,NumCols),
        length(Matrix,NumRows),
        maplist(same_length(Cols),Matrix),
        domain_matrix(Matrix,Domain).

%%
%% domain_matrix([],_)
%%
%% Ensure the domain of Domain
%%
domain_matrix([],_).
domain_matrix([L1|LRest],Domain) :-
        L1 ins Domain,
        domain_matrix(LRest, Domain).
