/*

   Test of mycin shell in Pop-11.

   See HELP EXPERTS.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/

compile('/home/hakank/Poplib/init.p');

uses teaching;
;;; 'See HELP EXPERTS\n'=>;
lib emycin;


[
  [ [age young] [species human] [sex male] => 1 [word boy] ]
  [ [age young] [species human] [sex female] => 1 [word girl] ]
  [ [age old] [species human] [sex male] => 1 [word man] ]
  [ [age old] [species human] [sex female] => 1 [word woman] ]
  [ [age young] [species dog] => 1 [word puppy] ]
  [ [age old] [species dog] [sex female] => 1 [word bitch] ]
  [ [age old] [species dog] => 0.5 [word dog] ]
  [ [legs 2] => 1 [species human] ]
  [ [legs 4] => -1 [species human] ]
  [ [legs 4] => 0.5 [species dog] ]
  [ [height short] => 0.5 [species dog] ]
  [ [height tall] => 0.5 [species human] ]
] -> system;

system==>;

'\n\nAnswer like this (in order): 2, "short", 1'==>;
'or ask with: "why" to see the reasoning\n'==>;
emycin("species",system);
