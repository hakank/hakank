/*

  Set comparisons.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set RAINBOW = {"red", "orange", "yellow", "green", "blue", "purple"};
set BRIGHT = {"yellow", "orange"};
set DARK = {"blue", "brown", "black"};

display BRIGHT diff RAINBOW;
display RAINBOW symdiff DARK;
display BRIGHT diff DARK;
display BRIGHT union RAINBOW;




