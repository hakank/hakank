% From Progol 4.1 examples/book.pl  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positive examples

% Original:
pos(book(1,[camoes,lusiadas,asa])).
pos(book(2,[pessoa,mensagem,asa])).
pos(book(3,[vergilioFerreira,contaCorrente,asa])).
pos(book(4,[saramago,levantadoDoChao,caminho])).
pos(book(5,[saramago,memorialDoConvento,caminho])).
pos(book(6,[eca,osMaias,lello])).
pos(book(7,[eca,aReliquia,lello])).

neg(book(1,[camoes,lusiadas,caminho])).
neg(book(1,[saramago,lusiadas,asa])).
neg(book(2,[pessoa,lusiadas,asa])).
neg(book(3,[vergilioFerreira,levantadoDoChao,asa])).
neg(book(4,[saramago,contaCorrente,caminho])).

