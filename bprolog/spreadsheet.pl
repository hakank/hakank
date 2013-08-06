/*

  Simple spreadsheet (using lp_solve) in B-Prolog.

  From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.
  Cf Winston "Artificial Intelligence", 3rd edition, page 235 
  (not the same values though)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-

        Low = 0.0,
        Up  = 20.0,

        LD = [B1,B4,B5,C4,C5,D4,D5,E7,E8],
        lp_domain(LD,Low,Up), 

        B1 = 0.17,
        B4 = 3.5,
        B5 = 1.7,
        C4 = 1.5,
        C5 = 4.5,
        D4 $= B4 * C4,
        D5 $= B5 * C5,
        E7 $= D4 + D5,
        E8 $= E7 * (1.0 + B1),

        lp_solve(LD),

        Names = ['B1','B4','B5','C4','C5','D4','D5','E7','E8'],
        foreach((N,V) in (Names,LD),
                format("~w: ~2f\n", [N,V])
               ),
        nl.

