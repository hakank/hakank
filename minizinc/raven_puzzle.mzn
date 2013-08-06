%
% Puzzle in Minizinc
% 
% In the swedish blog Raven writes the following (translation follows)
% http://www.raven.nu/blog/2008/02/29/internationellt-pa-kontoret/
% 
% """
% Vi hade ett ganska internationellt möte idag på jobbet. Vi var sju stycken. Mötet var 
% på engelska som faktiskt är vanligare än att ha det på svenska.
% Det roliga är när man summerar vilka språk vi totalt så kunde vi
% 
% 7 kunde engelska  språk 1
% 4 kunde svenska   språk 2
% 2 kunde persiska        3
% 1 kunde ryska           4 
% 1 kunde indiska         5 
%
% Jag skulle inte bli förvånad om det någon kunde fler språk med det räcker va?
% """

% Start english (approx.) translation:
%
% We had quite an international meeting at work today. We were seven participants. The meeting was in english which is more common than in swedish. The fun part is when we sums which languages we know:
%
% 7 knows english  (language 1)
% 4 knows swedish  (language 2)
% 2 knows persian  (language 3)
% 1 knows russian  (language 4)
% 1 knows hindi    (language 5)
%
% It wouldn't surprise me if we know even more languages. But this is enough, isn't it?
%
% End english (approx.) translation
%
% The problem is now to investigate which language each person knows, 
% given the summary above. (Cf Venn diagram.)
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: num_persons = 7;   % number of persons
int: num_languages = 5; % number of languages

% the distribution matrix: what language does a person know
array[1..num_persons, 1..num_languages] of var 0..1: mat;

% Summary of the number of persons who knows a language.
array[1..num_languages] of 1..num_persons: num_person_per_language = [7,4,2,1,1];

% number of language each person knows
array[1..num_persons] of var 0..num_languages: num_lang;


% solve satisfy;
solve maximize num_lang[1];
% solve minimize num_lang[1];

constraint

    forall(i in 1..num_languages) (
       sum(p in 1..num_persons) (mat[p,i]) = num_person_per_language[i]
    ) 
    /\
    % Symmetry breaking: Add number of language per person in the mat matrix
    % ...
    forall(p in 1..num_persons) (
       num_lang[p] = sum(l in 1..num_languages) ( mat[p,l])
    )
    /\
    % ... and this array should be sorted in order
    forall(p in 1..num_persons-1) (
      num_lang[p] >= num_lang[p+1]
    )
;


output [
    if i = 1 /\ j = 1 then "\n" ++ show(num_lang) ++ "\n" else "" endif ++
    if num_persons mod j = 0 then "\n" else " " endif ++
    show(mat[i,j]) 
    | i in 1..num_persons, j in 1..num_languages
] ++ ["\n"];

