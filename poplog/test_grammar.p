/* 

  Generative grammar for English

  Inspired from mygram.p.

*/
uses teaching;
uses grammar;

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



;;; donouns completes unknown nouns
true -> donouns;

setup(grammar, lexicon);

repeat 20 times 
  generate(grammar, lexicon)->x; 
  s(x)==>; 
  x==>;
  x.flatten==>; 
  '\n\n'=>;
endrepeat;
