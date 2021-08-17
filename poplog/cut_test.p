/*
   Fri Aug  7 15:40:08 2009/hakank@bonetmail.com

*/
compile('/home/hakank/Poplib/init.p');

;;; http://www.cs.bham.ac.uk/research/projects/poplog/string_ops/auto/cut.p

;;; Test code.
lvars i, j, p, q;
for p from 1 to 4 do
    for q from max( 1, p - 1 ) to 3 do
        for i in [% p, p - 4 %] do
            for j in [% q, q - 4 %] do
                [%
                    {% i, j %};
                    cut( i, j, { a b c } );
                    cut( i, j, [ a b c ] );
                    cut( i, j, "abc" );
                %] =>
            endfor
        endfor;
        nl( 1 );
    endfor
endfor;
