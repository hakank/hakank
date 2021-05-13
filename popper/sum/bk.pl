cons(A,B,C):-
    append([A],B,C).

sum(A,B,C):-
    C is A+B.

empty([]).

zero(0).

%popper_program_validation(P, PO, NO, Cons):-
%  writeln(P),
%  writeln(PO),
%  writeln(NO),
%  %flush_output,
%  Cons = [banish,generalisation].
