/*
   Sat Jun 20 10:23:18 2009/hakank@bonetmail.com

   Test of super

*/
compile('/home/hakank/Poplib/init.p');

;;; 2009-08-06: Aaron Sloman has identified the problem
;;;             and will fix it soon in the release.
;;;             Until them I did Aaron's fix in
;;;             ./pop/v15.63/pop/lib/lib/super.p
;;;             by commenting two sysVARS(x,0);
;;;

;;; added by recommendation from Aaron Sloman (20090805):
;;; 1 -> popsyscall;

compile('/home/hakank/Poplib/init.p');
uses super;

;;; testing
compile('/home/hakank/poplog/me/myforevery.p');

;;; from help super
;;; this works!
alladd([
        [rule sentence nounphrase verbphrase]
        [rule nounphrase determiner noun]
        [rule verbphrase verb nounphrase]
        [lexicon determiner the]
        [lexicon noun man]
        [lexicon noun computer]
        [lexicon verb hated]
        [ifneeded [recognize ?category ?given ?leaving]
         [rule ?category ?x ?y]
         [recognize ?x ?given ?temp]
         [recognize ?y ?temp ?leaving]]
        [ifneeded [recognize ?category [?word ??leaving] ?leaving]
         [lexicon ?category ?word]]
        ]);

;;; This works!
vars x,y;
lookup([recognize nounphrase [the man hated the computer] ?x]);
x=>;
;;;     ** [hated the computer]
present([recognize ?x [the man hated the computer] []]) =>
;;;** <true>
x =>
;;;** sentence


add([member ?x [?x ??y]]);
add([ifneeded [member ?x [?y ??z]] [member ?x ?z]]);

'present([member x [a b c d x]])=>'=>;
present([member x [a b c d x]])=>
;;; ** <true>
'present([member x [a b c d c]])=>'=>;
present([member x [a b c d c]])=>
;;; ** <false>

foreach [member ?x [a b c d e f g h i]] do it=> endforeach;


add([male håkan]);
add([male kalle]);
add([male nisse]);
add([female anna]);
add([female eva]);
add([single håkan]);
add([single kalle]);

add([ifneeded [bachelor ?x] [male ?x] [single ?x]]);

present([bachelor ?x]);
x=>;
'bachelors: ';which([x], [[bachelor ?x]])=>

'This below do work now!'=>;

;;; from 
;;;     help which
;;; This works now
newdatabase([
             [ on a b]
             [ on b c]
             [ on e f]
             [ on f g]
             [ on h g]
             [ifneeded [above ?x ?y] [on ?x ?y]]
             [ifneeded [above ?x ?y] [on ?x ?z] [above ?z ?y]]
             ]);

'on ?x g: ';which("x", [[on ?x g]])=>
;;; ** [f h]

'on ?x ?y: ';which([x y], [[on ?x ?y]])=>

;;; Travel example
'Travel example'=>;

;;; Note: This must be a tree, not a (circular) graph!
newdatabase([
             [ path a b]
             [ path b c]
             [ path c d]
             [ path a d]

             [ path e f]
             [ path f g]
             [ path g h]
   
             [ifneeded [route ?x ?y] [path ?x ?y]]
             [ifneeded [route ?x ?y] [path ?x ?z] [route ?z ?y] ]
             ]);


'route ?x d: '=>;which("x", [[route ?x d]])=>
;;; ** [c a a b]


lvars routes;
'route ?x ?y: '=>;which([x y], [[route ?x ?y]])->routes;
routes=>;
;;; ** [[a b] [a c] [a d] [b c] [b d] [c d] [e f] [e g] [e h] [f g] [f h] [g h]]

define route_sort(l1,l2); 
   alphabefore(l1(1),l2(1)) and alphabefore(l1(2),l2(2)) 
enddefine;

pr('sorted: ');
syssort(routes, route_sort)=>