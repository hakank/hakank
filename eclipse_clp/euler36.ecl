%
% Problem 36
% """
% The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
% in both bases.
% 
% Find the sum of all numbers, less than one million, which are palindromic 
% in base 10 and base 2.
%
% (Please note that the palindromic number, in either base, may not include leading zeros.)
% """
%
% Answer: 872187 
% Testing all numbers: 0.74s
% Testing just the odd numbers: 0.38s
% 

:- lib(listut).

go :-
  problem36.

% convert a list of ASCII integer to a list numbers
listnum(List,Num) :- 
        (foreach(L, List),
         foreach(N, Num) do
             N is L - 48
        ).

% checks for a palindromic list
% palindromic(List) :-
%         list(List),
%         myReverse(List, Rev),
%         List == Rev.

palindromic(List) :-
        reverse(List,List).


% (0.38s)
problem36 :-
        ( for(I,1,1000000,2),
          fromto(0,In,Out,Sum) do
              number_string(I,I10),
              string_list(I10,I10List),
              listnum(I10List,I10Num),
              ( palindromic(I10Num) -> 
                    (
                        % convert to radix 2
                        % swi:sformat(I2,"~2R",I), % nice
                        sprintf(I2,"%2R",I), % nice
                        string_list(I2,I2List),
                        listnum(I2List,I2Num),
                        (palindromic(I10Num), palindromic(I2Num)) ->
                            Out is In + I
                    ;
                            Out = In
                    )
              ;
                    Out = In
              )
        ),
        writeln(Sum).
