% Prolog predicates for tokenized reading from stdin.
%
% Provides the following predicates:
% read_string(S): reads a string token
% read_int(I): reads an integer token
% read_atom(A): reads an atom
%
% For all three predicates, the result is unified with end_of_file
% if the end of the input stream was reached.
%

% hakank: The first part is from kattio.pl

:- module(kattutils, [
    read_string/1,
    read_int/1,
    read_atom/1,

    % hakank:
    collect_lines_int/1,
    collect_lines_string/1
    ]).

read_string(S) :-
    read_token_codes(S).

read_int(I) :-
    read_token_codes(Codes),
    (Codes == end_of_file -> I = Codes ; number_codes(I, Codes)).

read_atom(A) :-
    read_token_codes(Codes),
    (Codes == end_of_file -> A = Codes ; atom_codes(A, Codes)).


% Internal predicate for getting the next token

read_token_codes(end_of_file) :-
    peek_code(end_of_file), !.

read_token_codes(Codes) :-
    peek_code(C),
    \+ code_type(C, space), !,
    read_token_codes_helper(Codes).

read_token_codes(T) :-
    get_char(_), !,
    read_token_codes(T).


read_token_codes_helper([C0|C]) :-
    peek_code(C0),
    \+ code_type(C0, space), !,
    get_code(C0),
    read_token_codes_helper(C).

read_token_codes_helper([]).


%
% hakank: My utils
%
% Collect the lines from the input into the list Lines.
%

% For ints
collect_lines_int(Lines) :-
    read_int(X),
    collect_lines_int(X,[],Lines).

collect_lines_int(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_int(X,Lines0,[X|Lines]) :-
    read_int(X2),
    collect_lines_int(X2,Lines0,Lines).

% Read a list of ints
read_ints(L) :-
    read_int(I),
    read_ints(I,[],L).
read_ints(end_of_file,L,L).
read_ints(I,L0,[I|L]) :-
    read_int(I2),
    read_ints(I2,L0,L).

% For strings
collect_lines_string(Lines) :-
    read_string(X),
    collect_lines_string(X,[],Lines).

collect_lines_string(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_string(X,Lines0,[X|Lines]) :-
    read_string(X2),
    collect_lines_string(X2,Lines0,Lines).


% For atoms
collect_lines_atom(Lines) :-
    read_atom(X),
    collect_lines_atom(X,[],Lines).

collect_lines_atom(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_atom(X,Lines0,[X|Lines]) :-
    read_atom(X2),
    collect_lines_atom(X2,Lines0,Lines).

