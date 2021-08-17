/*

   Example of tparse.

   See HELP TPARSE.
   """
   This is a variant of LIB GRAMMAR which is somewhat more general in that
   it will find all the parses of a list of words corresponding to a
   non-terminal. In order to do this it makes use of the POP-11 process
   mechanism. (See HELP * PROCESS).
   """ 

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

lib tparse;
vars grammar lexicon;
[
 [s  [np vp]   ]
 [vp [v np] [v np prep np]    ]
 [np [pn] [det noun] [np and np] [np prep np]]
 ] -> grammar;

[
 [noun   cat dog mouse man girl boy book tree]
 [pn     fred aaron steve john sharon]
 [v      liked killed thanked bought ate put]
 [prep   on over in at under]
 [det    each every the a some]
 ] -> lexicon;

setup(grammar,lexicon);
'\n\nshowparses("s", [the man ate the dog ]);'=>
showparses("s", [the man ate the dog ]);

'\n\nshowparses("s", [the man ate the dog and the mouse]);'=>
showparses("s", [the man ate the dog and the mouse]);

'\n\nlistparses("s", [the man ate the dog and the mouse])';
listparses("s", [the man ate the dog and the mouse])==>
  
;;; (Warning - this may take a minute or two to complete)
'\n\nlistparses("s", [the man put the cat on the book in the tree])'=>
listparses("s", [the man put the cat on the book in the tree]) ==>