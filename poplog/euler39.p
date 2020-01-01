/*

  Euler problem 39
  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
  
  {20,48,52}, {24,45,51}, {30,40,50}
  
  For which value of p <= 1000, is the number of solutions maximised?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define problem39;

    lvars m = 1000,
         best_p = 0,
         best_cc = 0;

    lvars p a b;
    fast_for p from 1 to m do 
        lvars cc = 0;
        fast_for a from 1 to p div 2 do
            fast_for b from a to (p-a) div 2 do
                ;;; if a+b > p then
                ;;;     nextloop;
                ;;; endif;
                lvars c = sqrt(a**2+b**2);
                if a + b + c = p then
                    cc+1->cc;
                else 
                    nextloop;
                endif;

                if best_cc < cc then 
                    cc->best_cc;
                    p->best_p;
                    ;;; [p ^p cc ^cc]=>
                endif;

            endfast_for;
        endfast_for;
    endfast_for;

    best_p=>

enddefine;

'problem39()'=>
problem39();


