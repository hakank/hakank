zero(0).

succ(X, Y) :-
  X is Y - 1, Y > 0.

%popper_program_validation(P) :- print(P),true.
