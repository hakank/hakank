/*

   Some tests of evans (analogy) in Pop-11.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
;;; compile('/home/hakank/Poplib/init.p');
uses teaching;

[] -> dictionary; ;;; erase dictionary first

lib analogy;

/*
   [[man [male sex person] [adult age person]]
    [woman [female sex person] [adult age person]]
    [boy [male sex person] [young age person]]
    [girl [female sex person] [young age person]]
    [child [young age person]]
    [person [person isa person]]
    [stallion [male sex horse] [adult age horse]]
    [mare [female sex horse] [adult age horse]]
    [colt [male sex horse] [young age horse]]
    [filly [female sex horse] [young age horse]]
    [foal [young age horse]]
    [horse [horse isa horse]]]
*/

dictionary -> dictionary_org;

[
 [wife [female sex person] [adult age person] [yes married person]]
 [husband [male sex person] [adult age person] [yes married person]]
 [bachelor [male sex person] [adult age person] [no married person]]
 [bacheless [female sex person] [adult age person] [no married person]]

 [mother [female sex person] [adult age person] [yes has_children person]] 
 [father [male sex person] [adult age person] [yes has_children person]] 


 ] -> new;

new <> dictionary -> dictionary;

dictionary==>;

;;;
;;; my_process and my_evans are stolen from analogy.p
;;; and changed so we can batch it.
;;; 
define my_processpic(pic);
	vars database pic picval;
        dict_lookup(pic) -> database;
        pr('\nHere is the dictionary definition of '><pic><'\n');
	database==>
	return(database)
enddefine;


define my_evans(A,B,C,D1,D2,D3);
	vars database bestmatch difab;
	vars pic_a pic_b pic_c pic_d1 pic_d2 pic_d3;
	vars g1unused g2unused force;
	vars diffab diffcd1 diffcd2 diffcd3;
	vars score1 score2 score3;

	my_processpic(A) -> pic_a;
	my_processpic(B) -> pic_b;
	my_processpic(C) -> pic_c;

        

	erase(comppic(pic_a,pic_b,[A],[B])) -> diffab;
	nl(2);
        prlist([i predict that the answer matches]);
        nl(1);
        predict(pic_a,pic_b,pic_c)==>

	'\nNow need to see each of the alternatives for D\n'.pr;
	my_processpic(D1) -> pic_d1;
	my_processpic(D2) -> pic_d2;
	my_processpic(D3) -> pic_d3;

        vars my_dict;
        [ [A ^A ^pic_a] [B ^B ^pic_b] [C ^C ^pic_c] 
          [D1 ^D1 ^pic_d1] [D2 ^D2 ^pic_d2] [D3 ^D3 ^pic_d3] 
          ] -> my_dict;
        
        '\n\nshow_case:'=>;
        my_dict==>;

	erase(comppic(pic_c,pic_d1,[C],[D1])) -> diffcd1;
	erase(comppic(pic_c,pic_d2,[C],[D2])) -> diffcd2;
	erase(comppic(pic_c,pic_d3,[C],[D3])) -> diffcd3;
	prlist([Now the moment of truth]);
	erase(comppic(diffab,diffcd1,[the differences between A and B],
			[the differences between C and D1]) -> score1);
	erase(comppic(diffab,diffcd2,[the differences between A and B],
			[the differences between C and D2]) -> score2);
	erase(comppic(diffab,diffcd3,[the differences between A and B],
			[the differences between C and D3]) -> score3);
	nl(2);

        vars difference_scores = [^score1 ^score2 ^score3];
	prlist([the difference
			of the differences score ^score1 ^score2 ^score3 respectively]);
	prlist([so the answer is]);
	sp(1);

        vars answer;
	if score1 < score2 then
	    if score1 < score3 then 
                diffcd1 -> diffcd3; 
                "D1".pr; 
                "D1" -> answer; 
            else 
                "D3".pr; 
                "D3"->answer;
            endif
	elseif score2 < score3 then 
            diffcd2 -> diffcd3; 
            "D2".pr;
            "D2" -> answer;
        else 
            "D3".pr;
            "D3" -> answer;
	endif;
	nl(2);

        vars answer_full;
        vars answer_letter;

        my_dict matches [==  [^answer ?answer_letter ?answer_full] == ]==>;
        [picture: ^answer]=>
        [what: ^answer_letter]=>;
        [definition: ^answer_full]==>;
        [options: ^D1 ^D2 ^D3]==>
        [difference_scores: ^^difference_scores]=>;

        [^A is to ^B as ^C is to ^answer_letter]==>;
enddefine;


[now load evans()]=>;

;;;      A to B    as C   to      D1     D2     D3
my_evans("man","woman",  "boy",   "girl","mare","filly");
