/*

  Simple spreadsheet (using clpr) in SWI Prolog

  From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.
  Cf Winston "Artificial Intelligence", 3rd edition, page 235 
  (not the same values though)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpr)).

go :-
        { 
          B1 = 0.17,
          B4 = 3.5,
          B5 = 1.7,
          C4 = 1.5,
          C5 = 4.5,
          D4 = B4 * C4,
          D5 = B5 * C5,
          E7 = D4 + D5,
          E8 = E7 * (1.0 + B1)
        },
        writeln([b1=B1,
                 b4=B4,
                 b5=B5,
                 c4=C4,
                 c5=C5,
                 d4=D4,
                 d5=D5,
                 e7=E7,
                 e8=E8]),

      nl.
