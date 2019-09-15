/*

  Euler problem 42 in SWI Prolog

  """
  The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
  so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its 
  alphabetical position and adding these values we form a word value. For example, 
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
  is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
  containing nearly two-thousand common English words, how many 
  are triangle words?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).
:- use_module(library(readutil)).

go :- 
        L = [
             euler42a
            ],
        run_problems(L).
             
%%
%% 0.01s
%%
euler42a :-
        read_file_to_string("euler42_names.txt",Str,[]),
        re_replace('"'/g,"",Str,Str2),        
        split_string(Str2,",","",Lines),
        numlist(1,100,Is),
        maplist(triangle_number,Is,Ts),
        findall(Ss,
                (member(S,Lines),
                 upper_string_to_digit_list(S,D),
                 sum_list(D,Ss),
                 memberchk(Ss,Ts)
                ),
                L
               ),
        length(L,Len),
        writeln(Len).

triangle_number(N,T) :-
        T is (N*(N+1)) div 2.

upper_string_to_digit_list(N,L) :-
        atom_codes(N,L2),
        maplist(to_upper_alpha,L2,L).

to_upper_alpha(N,Alpha) :-
        Alpha #= N - 64.
