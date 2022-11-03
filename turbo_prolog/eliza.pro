/* Fr†n The Art of Prolog sid 232   */
/*domains
	llist     = list*
	list      = symbol*


predicates

	eliza
	eliza(symbol)
	eliza(llist)
	pattern(llist, llist)
	match(llist, llist, llist)
	reply(list)
	read_word_list(llist)
        read_word_list(llist, llist) 
	word_chars(llist, llist, llist)
	word_char(char)
	read_word(list, list, list)
	end_of_word_char(char).
        atom(symbol)
	lookup(llist, llist, integer)		
	integer(integer)
	append(list, list, list)
	important(symbol)	
	name(list, list)	
        not_word_char(char).
	fill_char(char)      
clauses
*/
eliza if
    read_word_list(Input), eliza(Input), !.
  
 eliza(bye) if
    write("Goodbye. I hope I have helped you").
     
 eliza(Input) if
    pattern(Stimulus, Response),
    match(Stimulus, Dictionary, Input),
    match(Response, Dictionary, Output),
    reply(Output),
    read_word_list(Input1),
    !, eliza(Input1).
    
 match([N|Pattern], Dictionary, Target) if
    integer(N), lookup(N, Dictionary, LeftTarget),
    append(LeftTarget, RightTarget, Target),
    match(Pattern, Dictionary, RightTarget).
 
 match([Word|Pattern], Dictionary, [Word|Target]) if
    atom(Word), match(Pattern, Dictionary, Target).
 
 match([], Dictionary, []).
 
 lookup(Key, [[Key, Value]|Dictionary], Value).
 lookup(Key, [[Key1, Value1]|Dictionary], Value) if 
     Key <> Key1, lookup(Key, Dictionary, Value).
      
 pattern([i, am, 1], [how, long, have, you, been, 1, "?"]).
 pattern([1, you, 2], [what, makes, you, think, i, 2, you, "?"]).   
 pattern([i, like, 1], [does, anyone, else, in, your, family, like, 1, "?"]).
 pattern([i, feel, 1], [do, you, often, feel, that, way, "?"]).
 pattern([1, X, 2], [can, you, tell, me, more, about, X]) if
         important(X).
 pattern([1], [please, go, on]).
 
 important(father).   
 important(mother).
 important(sister).
 important(son).
 important(brother).
 important(daughter).
 
 reply([Head|Tail]) if
    write(Head), write(' '), reply(Tail).
 reply([]) if
    nl.
    
 read_word_list(Ws) if
     readchar(C),
     read_word_list(C,Ws).
     
 read_word_list(C, [W|Ws]) if
     word_char(C),
     read_word(C, W, C1),
     read_word_list(C1, Ws).
       
 read_word_list(C, []) if
     end_of_word_char(C).
     
 read_word(C, W, C1) if
     name(W, Cs).
 
 word_chars(C, [C|Cs], C0) if
      word_char(C),
      !,
      readchar(C1),
      word_chars(C1, Cs, C0).

 word_chars(C, [], C) if
      not_word_char(C).
      
 word_char(C) if 97 <= C, C <= 122.       % Lower case letter
 word_char(C) if 65 <= C, C <= 90.        % Upper case letter  
 word_char(95).                           % Underscore 
  fill_char(32).                           % Blank  
 
 end_of_word_char(46).                    % Period
 
 append([], Ys, Ys).
 append([X|Xs], Ys, [X|Zs]) if
       append(Xs, Ys, Zs).
       
 atom(_) if !.
 integer(I) if !. 
 name(X,Y) if !.
 not_word_char(X) if !.
                        
          
 
       
      
       
       


