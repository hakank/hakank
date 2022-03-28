/*

  Wordle solver in SWI Prolog

  Note: This program requires a wordlist (here called wordle_small.txt).

  Two Wordle related wordlist can be found at
  https://github.com/dlew/wordle-solver/tree/main/src/main/resources
  - target: the small wordlist, 2315 words (here called wordle_small.txt)
  - wordlist: the large wordlist, 12971 words

  (This is a port of my Picat program http://hakank.org/picat/wordle2.pi .)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/
:- use_module(library(readutil)).

/*
  Some realistic tests.

*/
go :-

        wordle("...n.",["","","","",""],"slat"),
        % ->  [crone,brine,crony,briny,prone,corny,borne,prune,drone,phone,brink,phony,bound,pound,grind,frond,found,bring,drink,being,whine,fiend,chunk,whiny,prong,mound,horny,urine,round,drunk,irony,doing,wound,hound,opine,wring,rhino,downy,wrong,wrung,ebony,ovine,dying,young,owing,eying,vying,eking,penne,penny,bunny,funny,going,ninny,ozone,icing]

        wordle(".r.n.",["","","","",""],"slatcoe"),
        % -> [briny,brink,grind,bring,drink,drunk,wring,wrung]
  
        wordle("...st",["s","","","",""],"flancre"),
        % -> [moist,hoist,ghost,midst,joist,joust,boost,twist]
  
        wordle(".r.n.",["","","","",""],"slatcoe"),
        % -> [briny,brink,grind,bring,drink,drunk,wring,wrung]
  
        wordle(".r.n.",["","","","",""],"slatcoebiy"),
        % -> [drunk,wrung]
  
        wordle(".run.",["","","","",""],"slatcoebiydk"),
        % ->  [wrung]

        wordle(".l...",["","","a","","t"],"sn"),
        % -> [alter,ultra,altar]

        % 2022-03-16: Wordle word of today
        wordle(".....",["","","a","","t"],"sln"),
        % [cater,triad,tread,today,tamer,party,matey,taper,faith,tardy,batch,water,tapir,patch,hater,warty,haute,taker,patio,tweak,bathe,amity,match,acute,earth,watch,tacky,ratio,actor,datum,after,topaz,quota,extra,catty,batty,tatty,patty,catch,fatty,eater,terra,ratty,aorta,theta,hatch,tibia,taboo,tabby,taffy,cacti,attic]
        
        % wordle(".....",["","","","",""],""),
        % -> All words...

        nl.


%
% wordle(Words,CorrectPos,CorrectChar,NotInWord)
% - Words: the wordlist/candidates
% - CorrectPos: correct character in correct position,
%   Example: n is in position 4 and t is in position 5: "...nt": 
% - CorrectChar: correct character but in wrong position.
%   Example: a is not in position 1, l is not in position 2: ["a","l","","",""]
% - NotInWord: characters not in word.
%   Example: s, a, and t are not in the word: "sat"
%
wordle(CorrectPos, CorrectChar, NotInWord) :-
        writeln(wordle(words,CorrectPos,CorrectChar,NotInWord)),
        N = 5,
        File = "wordle_small.txt", % 2314 words
        read_words(File,N,Words),

        wordle(Words, CorrectPos, CorrectChar, NotInWord,[],Candidates),
        length(Candidates,CandidatesLen),
        writeln(candidates=Candidates),
        writeln(len=CandidatesLen),
        
        (Candidates \= [] ->
         Candidates = [Suggestion|_], writeln(suggestion=Suggestion)
        ;
         true
        ),
  
        nl.

%
% The main engine:
% Loop through all words and check if they are in the scope.
% 
wordle([],_CorrectPos,_CorrectChar,_NotInWord, AllWords,Sorted) :-
        sort_candidates(AllWords,Sorted).
wordle([Word|Words],CorrectPos1,CorrectChar1,NotInWord1, AllWords0,AllWords) :-
        string_chars(CorrectPos1,CorrectPos),
        string_chars(NotInWord1,NotInWord),
        maplist(string_chars,CorrectChar1,CorrectChar),
        ( (correct_pos(Word,CorrectPos),
           correct_char(Word,Word,CorrectChar),
           not_in_word(Word,NotInWord)) ->
          append(AllWords0,[Word],AllWords1),
          wordle(Words, CorrectPos, CorrectChar, NotInWord, AllWords1,AllWords)
        ;
          wordle(Words, CorrectPos, CorrectChar, NotInWord, AllWords0,AllWords)
        ).

%
% Correct position.
%
% Ensure that all chars != "." are in correct position.
% 
correct_pos([],[]).
correct_pos([C|Cs],[C2|CorrectPos]) :-
        (C == C2 ; C2 == '.'),
        correct_pos(Cs,CorrectPos).

%
% Correct char.
% Ensure that the character is in the word, but not
% in the given position.
% 
correct_char(_,_,[]).
correct_char(Word,[W|WordRest],[CC|CorrectChars]) :-
        correct_char_(Word,W,CC), % check each character in CorrectChars
        correct_char(Word,WordRest,CorrectChars).

%
% Helper function for correct_char/2.
% Check each character in CorrectChar against each
% character in the candidate word.
%
correct_char_(_,_,[]).
correct_char_(Word,W,[C|CorrectChars]) :-
        W \= C,
        memberchk(C,Word),
        correct_char_(Word,W,CorrectChars).

%
% Characters not in word.
% Ensure that the given chararacter are not in the
% candidate word.
% 
not_in_word(_Word,[]).
not_in_word(Word,[C|Cs]) :-
        \+ memberchk(C,Word),
        not_in_word(Word,Cs).

%
% Sort the candidates.
%
sort_candidates(Candidates,Sorted) :-
        % The probability order for each position in the word.
        % Reversed and with missing chars.
        % See http://hakank.org/picat/wordle.pi (go2/0) for the method to get this.
        Alphas1 = ["zyjkquinovhewlrmdgfaptbcsx",
                   "zqfkgxvbsdymcwptnhulieroaj",
                   "qjhzkxfwyvcbpmgdstlnrueoia",
                   "xyzbwhfvpkmdguotcrilasnejq",
                   "uzxbiwfcsgmpoakdnhlrtyejqv"
                  ],
        maplist(string_chars,Alphas1,Alphas),
        score_words(Candidates,Alphas,[],Scores),
        keysort(Scores,Sorted1),
        reverse(Sorted1,Sorted2),
        pairs_values(Sorted2,SortedList),
        maplist(string_chars,Sorted,SortedList).


%
% Score all words
% 
score_words([],_Alpha,Scores,Scores).
score_words([Word|Words],Alphas,Scores0,[Score-Word|Scores]) :-
        % We boost words with distinct characters.
        list_to_set(Word,Unique),
        length(Word,WordLen),
        length(Unique,UniqueLen),
        (WordLen == UniqueLen -> Score1 = 100 ; Score1 = 0),
        score_word(Word,Alphas,Score1,Score),
        score_words(Words,Alphas,Scores0,Scores).

% Score each character in a word
score_word([],_Alpha,Score,Score).
score_word([C|Cs],[Alpha|Alphas],Score0,Score) :-
        nth1(N,Alpha,C),        % find the position of this character
        S is N / 2,
        Score1 is Score0 + S,
        score_word(Cs,Alphas,Score1,Score).

% Print an empty board
empty() :-
        println("wordle(\".....\",[\"\",\"\",\"\",\"\",\"\"],\"\")").

read_words(File,Len, Words) :-
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "",WordsRead),
        include(strlen(Len),WordsRead,Words1),
        maplist(string_chars,Words1,Words).

strlen(Len,Word) :-
        string_length(Word,Len).


