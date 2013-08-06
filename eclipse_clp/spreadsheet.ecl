/*

  Simple spreadsheet in ECLiPSe.
 
  From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.
  Cf Winston "Artificial Intelligence", 3rd edition, page 235 
  (not the same values though)

  Compare with the the following models:
  * MiniZinc: http://www.hakank.org/minizinc/spreadsheet.mzn
  * Comet   : http://www.hakank.org/comet/spreadsheet.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

go :-

        Low = 0.0,
        Up  = 1000.0,

        LD = [B1,B4,B5,C4,C5,D4,D5,E7,E8],
        LD :: Low..Up, 

        B1 $= 0.17,
        B4 $= 3.5,
        B5 $= 1.7,
        C4 $= 1.5,
        C5 $= 4.5,
        D4 $= B4 * C4,
        D5 $= B5 * C5,
        E7 $= D4 + D5,
        E8 $= E7 * (1.0 + B1),


        locate(LD, 0.001),

        writeln([b1:B1,b4:B4,b5:B5,c4:C4,c5:C5,d4:D4,d5:D5,e7:E7,e8:E8]).

