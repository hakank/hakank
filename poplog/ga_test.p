/* 

   test av ga. se HELP ga

*/
compile('/home/hakank/Poplib/init.p');

;;; uses teaching;
uses ga;

;;; This works...
;;; load '/home/hakank/poplog/current_poplog/pop/current-poplog/pop/packages/teaching/lib/ga.p';

;;; enkelt exempel från HELP GA:
define test_fitness_function( chrom );
   lvars chorm, bit_str = bit_string(chrom), fit = 0, counter;

   for counter from 2 to bit_number(chrom) do
       if ( bit_str(counter-1) /= bit_str(counter) ) then
          fit + 1 -> fit;
       endif;
   endfor;
   fit -> fitness(chrom);
enddefine;

vars c = conschromosome( undef, { 1 0 1 0 }, 4 );
test_fitness_function( c );
fitness(c) ==>

vars best = genetic_algorithm( "test_fitness_function", 30, 8, 50, 
                               0.7, 0.01, 0, false, true, undef );
bit_string(best) ==>
fitness(best) ==>


;;; slut på det enkla exemplet

