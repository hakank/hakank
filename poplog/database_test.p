/*
  From /home/hakank/poplog/books/Week6.pdf

*/

[] -> database;
alladd( 
      [
        [child nicola paul]
        [daughter helen sarah]
        [father jack mark]
        [father peter jill]
        [father steven fred]    
        [gender betty female]
        [gender bill male]      
        [gender bob male]
        [gender david male]     
        [gender fred male]
        [gender harriet female] 
        [gender helen female]   
        [gender jack male]
        [gender jane female]    
        [gender jill female]    
        [gender mark male]      
        [gender nicola female]  
        [gender peter male]
        [gender sarah female]   
        [gender steven male]    
        [sibling bob jill]      
        [sibling david jane]    
        [sibling helen bill]
        [sibling jack fred]     
        [sibling jane mark]     
        [spouse fred sarah]     
        [spouse helen paul]     
        [spouse jill jack]
        [spouse peter harriet]  
        [spouse steven betty]   
        ]);

vars x, y, relation;

'simple foreach'=>;
foreach ![sibling ?x ?y] do [^x is a sibling to ^y]=> endforeach;

;;; use matches of
;;; hmm, this is different from running it from the prompt.
'more advanced foreach (uses matchesof)'=>;
foreach [?relation ?x ?y] do 
    if relation matchesoneof [father daughter spouse] then 
        [^x is a ^relation to ^y]==> 
    endif; 
endforeach;

'forevery [[gender ?x male] [sibling ?x ?y]] do them=> endforevery;'=>;
forevery [[gender ?x male] [sibling ?x ?y]] do them=> endforevery;
;;; ** [[gender jack male] [sibling jack fred]]
;;; ** [[gender david male] [sibling david jane]]
;;; ** [[gender bob male] [sibling bob jill]]

;;; testing which

'which("x", [[gender ?x male]]);'=>
which("x", [[gender ?x male]]); x=>

'which([x y], [[gender ?x male] [gender ?y female] [spouse ?x ?y]]);'=>;
which([x y], [[gender ?x male] [gender ?y female] [spouse ?x ?y]])=>;


