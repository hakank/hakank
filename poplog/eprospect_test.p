/*

   Test of eprospect in Pop-11.

   See HELP EXPERTS.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
;;; uses teaching;

;;; uses eprospect;
lib eprospect;

   [
     [ [age young] [species human] [sex male] => 10 0.1 [word boy] ]
     [ [age young] [species human] [sex female] => 10 0.1 [word girl] ]
     [ [age old] [species human] [sex male] => 10 0.1 [word man] ]
     [ [age old] [species human] [sex female] => 10 0.1 [word woman] ]
     [ [age young] [species dog] => 10 0.1 [word puppy] ]
     [ [age old] [species dog] [sex female] => 10 0.1 [word bitch] ]
     [ [age old] [species dog] => 5 0.1 [word dog] ]
     [ [legs 2] => 10 0.1 [species human] ]
     [ [legs 4] => 0.1 10 [species human] ]
     [ [legs 4] => 5 0.1 [species dog] ]
     [ [height short] => 5 0.1 [species dog] ]
     [ [height tall] => 5 0.1 [species human] ]
   ] -> system;

system==>;

;;; I don't understand this. It asks about this anyway...
[ 
  [[height short] 5] 
  [[height tall] -5] 
  [[legs 2] 5] 
  [[legs 4] -5]
  [[age old] -5]  
  [[age young] 5]
  ]->priors;

eprospect(system,priors);