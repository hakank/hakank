/*

  Test of extract_from_indices(2d)/3 in SWI Prolog

  - extract_from_indices/3 is  reversible (go/0)
  - extract_from_indices2d/3 is NOT reversible (go2/0).

  See hakank_utils.pl for implementation.

  Later note: Perhaps these instead should be called
    - matrix_elements/3   (matrix_elements(X,Is,Xs)
    - matrix_elements2d/3 (matrix_elements(X,IJs, Xs)
  instead?

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 10,
        length(X, N),
        X ins 1..N,
        
        length(Y, N),
        N1 #= N+1,
        Y ins 0..N1,
        
        all_different(X),
        decreasing(X),
        maplist(add1,X,Y),
        
        findall(I,(between(1,N,I), I mod 2 #= 1),Is),
        writeln(is=Is),
        extract_from_indices(Is, X, Xs),
        writeln(xs_before_solve=Xs),

        %%
        %% reversible: From Xs and X -> Is: Yes
        %%
        extract_from_indices(Is2, X, Xs),
        extract_from_indices(Is3, X, Y),

        %%
        %% reversible: From Is and Xs -> X?
        %% Yes: though in this case it only finds the odd values and
        %% fills out with "random" 1s
        %% x2=[10,1,8,1,6,1,4,1,2,4]
        %%
        length(X2,N),
        X2 ins 1..N,
        extract_from_indices(Is, X2, Xs),

        flatten([X,X2], Vars),
        label(Vars),
        writeln(x=X),
        writeln(xs=Xs),
        writeln(is2=Is2),        
        writeln(y=Y),
        writeln(is3=Is3),
        writeln(x2=X2),
        nl.


%%
%% Is extract_from_indices2d/3 reversible?
%% Answer: Nope!
%%
go2 :-
        writeln("extract_from_indices2d/3 is NOT reversible!"),
        N = 4,
        N2 #= N*N,
        new_matrix(N,N, 1..N2, X),
        flatten(X,Vars),

        all_different(Vars),
        % diagonal sums
        findall([I,I], between(1,N,I), IJs),
        writeln(ijs=IJs),
        extract_from_indices2d(IJs,X,[], Diagonal1),
        writeln(diagonal1=Diagonal1),
        length(IJs2,N),
        flatten(IJs2,IJs2Flatten),
        IJs2Flatten ins 1..N2,
        %% Nope, this don't work:
        %% ERROR: Type error: `integer' expected, found `[_56150,_56156]' (a list)
        %% extract_from_indices2d(IJs2,X,[], Diagonal1),
        
        append(Vars,Diagonal1, Vars2),
        append(Vars2,IJs2Flatten, Vars3),        
        % writeln(vars=Vars3),
        label(Vars3),
        
        print_matrix(X),
        writeln(diagonal1=Diagonal1),
        writeln(ijs2=IJs2),
        nl.

add1(X,Y) :- Y #= 1+(X + 1) mod 10.
