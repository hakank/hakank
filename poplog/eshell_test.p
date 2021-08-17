/*

   Test of eshell in Pop-11.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
;;; uses teaching;
lib eshell;

vars rules;
[
    [[west wind] => 0.6 [rain]]
    [[south wind] => 0.8 [warm]]
    [[high clouds] => 0.9 [high pressure]]
    [[not [rain]] => 1 [dry]]
    [[not [high pressure]] => 0.6 [west wind]]
    [[clear sky] => 0.9 [high pressure]]
    [[low pressure] => 0.6 [rain]]
    [[not [high pressure]] => 1 [low pressure]]
    [[low clouds] => 0.8 [rain]]
    [[summer] [warm] => 0.2 [rain]]
    [[not [high clouds]] [not [low clouds]] => 1 [clear sky]]
    [[not [winter]] => 1 [summer]]
    [[winter] [high pressure] => 0.9 [cold]]
    [[winter] => 0.7 [cold]]
    [[and [summer] [not [high pressure]]] => 0.6 [cold]]
    [[not [cold]] => 1 [warm]]
    [[dry] [warm] => 1 [dry warm]]
    [[rain] [warm] => 1 [wet warm]]
    [[dry] [cold] => 1 [dry cold]]
    [[rain] [cold] => 1 [wet cold]]
    ] -> rules;

'type in sentences like "west wind", "low pressure", "low clouds" (without quotes)\n'=>;
'and end with an empty line. After that: type "help" to get some help'=>;

setup(rules);
true -> chatty;
run();


