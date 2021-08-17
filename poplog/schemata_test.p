/*

   Test of schemata.

   From TEACH SCEMATA .

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

lib schema;
lib someschemata;

'bookticket:'=>
bookticket==>

'\nstory4:'=>
story4==>

story4 -> database;
'\nscheck(bookticket)'=>
scheck(bookticket);

'\nsame'=>
same==>

'\nmissing'=>
missing==>

'\nextra'=>
extra==>


'\n\Using schoose:'=>
'\nstory1:'=>
story1==>

'\nsorry:'=>
sorry==>

'\ntrue -> tracing'=>
true -> tracing;

story1 -> database;
'schoose([sorry bookticket]):'=>
schoose([sorry bookticket]) =>

'\nsame:'=>
same==>

'\nmissing:'=>
missing==>

'\nextra:'=>
extra==>


'\n\nstory3:'=>
story3==>
'\n\nidentify(story3)'=>
identify(story3);