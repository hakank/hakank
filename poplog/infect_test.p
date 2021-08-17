/*

  Infect example in Pop-11.

  From TEACH INFECT
  
  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

;;; Verbose version
define infect1(person);
    vars other;
    [^^person is being infected] =>
    if present([^^person has flu]) then
        [^^person already has flu] =>
    else
        add([^^person has flu]);
        [about to see if ^^person kisses anyone] =>
        if present([^^person kisses ??other]) then
            [yes -- ^^person kisses ^^other] =>
            [^^other is to be infected too] =>
            infect(other);
            [continuing to infect ^^person having finished with ^^other] =>
        else
            [no -- ^^person is chaste] =>
        endif;
    endif;
    [^^person - and friends - all infected now] =>
enddefine;

;;; Not verbose and using foreach...
define infect(person);
    vars other;
    unless present([^^person has flu]) then
        add([^^person has flu]);
        foreach [^^person kisses ??other] do
            infect(other);
        endforeach;
    endunless;
enddefine;


define setup();
    [] -> database;
    add([john kisses mary]);
    add([mary kisses bill]);
    add([bill kisses jane]);
    add([albert kisses ethel]);
enddefine;

setup();
database ==>


'\n\nlet us infect steve'=>
infect([steve]);
database ==>

'\n\nlet us infect albert'=>
infect1([albert]);
database ==>
