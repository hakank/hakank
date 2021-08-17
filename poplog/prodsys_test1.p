/*
  
   Test of Prodsys in Pop-11

   See HELP PRODYS.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
uses teaching;

lib prodsys;

nil -> rulebase;

false -> repeating;
false -> chatty;

/* Determine the type of dish */

rule get_dish [wine property main_dish is unknown];
lvars dish;
pr('Is the main dish fish, meat, or poultry');
readline()->dish;
add([wine property main_dish is ^^dish]);
remove([wine property main_dish is unknown]);
endrule;

/* The next three rules determine the colour of the wine */

rule colour1 [wine property main_dish is fish];
add([wine property chosen_colour is white certainty 0.9]);
add([wine property chosen_colour is red certainty 0.1]);
endrule;

rule colour2 [wine property main_dish is poultry];
add([wine property chosen_colour is white certainty 0.9]);
add([wine property chosen_colour is red certainty 0.3]);
endrule;

rule colour3 [wine property main_dish is meat];
add([wine property chosen_colour is red certainty 0.7]);
add([wine property chosen_colour is white certainty 0.2]);
endrule;

/* This rule is fired if the user does not state the main dish */
        
rule dish_unknown [wine property main_dish is      ];
add([wine property chosen_colour is red certainty 0.5]);
add([wine property chosen_colour is white certainty 0.5]);
endrule;

/* Discover which colour of wine the user prefers */

rule find_colour [wine property preferred_colour is unknown];
lvars preference;
pr('Do you prefer red or white wine');
readline()->preference;
add([wine property preferred_colour is ^^preference
     certainty 1.0]);
remove([wine property preferred_colour is unknown]);
endrule;


/* This rule is fired if the user does not express a preference */
        
rule no_preference
[wine property preferred_colour is   certainty 1.0];
add([wine property preferred_colour is red certainty 0.5]);
add([wine property preferred_colour is white certainty 0.5]);
endrule;

/* The next two rules merge the user's preference with the program's
*  choice of colour (based on the type of dish)
*/

rule merge1 [wine property chosen_colour is ?colour1 certainty ?cert1]
[wine property preferred_colour is ^colour1 certainty ?cert2];
add([wine property colour is ^colour1 certainty
     ^(cert1 + (0.4 * cert2 * (1 - cert1)))]);
remove ([wine property chosen_colour is ^colour1
         certainty ^cert1]);
remove ([wine property preferred_colour is ^colour1
         certainty ^cert2]);
endrule;

/* Cannot reconcile colours (ie. no preferred_colour for a particular
                                     * colour)
*/

rule merge2 [wine property chosen_colour is ?colour certainty ?cert];
remove ([wine property chosen_colour is ^colour certainty ^cert]);
add ([wine property colour is ^colour certainty ^cert]);
endrule;

/* Print out the suggested wine.
The special condition beginning with "where" specifies that
this rule only applies if cert2 is greater than cert1.
This ensures that the colour with the greater certainty is
printed out.
A "where clause" can be included in any rule, but only at the
end of the condition; there can be only one per rule.  It
consists of the word "where", followed by any POP-11 expression,
which ends at the semi-colon at the end of the condition.
The expression must return true or false, and the rule is only
fired if in addition to all the patterns being present, the
"where" expression returns true.
*/

rule print_wine
[wine property colour is ?colour1 certainty ?cert1]
[wine property colour is ?colour2 certainty ?cert2]
where cert2 > cert1;
remove([wine property colour is ^colour1 certainty ^cert1]);
remove([wine property colour is ^colour2 certainty ^cert2]);
[I would suggest a ^colour2 wine with a certainty of ^cert2]=>
endrule;


/* Default rule, fired if certainties for red and white are equal.
   Again, a "where clause" is used to check part of the condition,
   namely that colour1 and colour2 are different. */

rule either
[wine property colour is ?colour1 certainty ?cert1]
[wine property colour is ?colour2 certainty ^cert1]
where colour1 /= colour2;
remove([wine property colour is ^colour1 certainty ^cert1]);
remove([wine property colour is ^colour2 certainty ^cert1]);
[Either a red or a white wine would be appropriate]=>
endrule;


[[wine property main_dish is unknown]
 [wine property preferred_colour is unknown]] -> database;


run();

database==>;