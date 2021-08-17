/* testing of id3.p */

uses teaching;
load /home/hakank/poplog/me/id3.p;

/* testing */
;;; Consider the following set of tests for identifying animals.
;;; Each test in the list of tests has the form
;;; [TEST_TYPE N value1 value2 value3 value4 ...]

;;; Each animal description (see below) is a list of values for tests,
;;; and N, the index, for the type, is an integer which indicates that
;;; this test_type always has its value in position  N.

;;; detta är alltså strukturen på data
vars animal_tests =
     [
      ['Number of legs' 1 0 2 4 6 8]
      ;;; The first 1 means that the value for this test must be the
      ;;; first item in an instance description. I.e. the index is 1.
      ['Number of wings' 2 0 2 4]
      ;;; The index is 2.
      ['External skeleton' 3 yes no]
      ['Lives in water' 4 yes no]
      ['Can fly' 5 yes no]
      ['Can talk' 6 yes no]
  ];
     

;;; You could consider how many additional tests would be needed for
;;; identifying animals, e.g. insects of different types.

;;; Now some biological data (not very realistic!)
vars animal_instances =
  [;;;legs wings ext  water fly  talk
    [[ 0    0    no   yes   no   no]  fish]
    [[ 0    0    no   yes   no   yes] mermaid]
    [[ 2    0    no   no    no   yes] human]
    [[ 2    2    no   yes   no   no] penguin]
    [[ 6    4    yes  no    yes  no] insect]
    [[ 6    4    yes  no    no   no] beetle]
    [[ 8    0    yes  no    no   no] spider]
    [[ 6    0    yes  no    no   no] damaged_spider]  ;;; don't believe this.
  ];

'animal_instances:'=>
animal_instances==>;

'\n\ninduce_rules:'=>
induce_rules(animal_tests, animal_instances) ==>

'\n\nrev(animal_instances):'=>
rev(animal_instances)==>;

'\n\ninduce_rules:'=>
induce_rules(rev(animal_tests), animal_instances) ==>

uses showtree;
;;; showtree(induce_rules(animal_tests, animal_instances));
;;; showtree(induce_rules(rev(animal_tests), animal_instances));


vars
     ;;; The tests are got from three tests, which may have varying
     ;;; values
     tests =
     [
      [test1 1 t11 t12 t13]   ;;; a test with three possible outcomes
      [test2 2 t21 t22]       ;;; a test with only two
      [test3 3 t31 t32 t33 t34]   ;;; a test with four possibilities
      ],

     ;;; now some examples of instances, where each instance is a patient
     ;;; with some recorded test values and a bug found in that patient
     instances =
     [
      [[t11 t21 t31] bug1]
      [[t11 t21 t32] bug2]
      [[t12 t22 t33] bug3]
      [[t12 t22 t34] bug4]
      [[t13 t21 t31] bug1]
      [[t13 t21 t32] bug3]
      [[t13 t22 t33] bug3]
      ];
     
;;; You could try adding more data, keeping the instances
;;; consistent. Or more test types, or test values.
     
;;; Now induce some rules
induce_rules(tests, instances) ==>
    
;;;    showtree( induce_rules(tests, instances) );

;;; Now try with some inconsistent data
vars
     tests2 =
     [
      [test1 1 t11 t12 t13]
      [test2 2 t21 t22]
      ],
     instances2=
     [
      [[t11 t21] bug1]
      [[t11 t21] bug3]   ;;; inconsistent with previous case
      [[t11 t22] bug2]
      [[t12 t22] bug3]
      ];
     
induce_rules(tests2, instances2) ==>

;;; We can make a prettier display
;;; '\n\nshowtree:'=>
;;;;showtree(induce_rules(tests2, instances2));


;;; try it with the tests reversed:
;;;    showtree(induce_rules(rev(tests2), instances2));


;;; These are interactive versions of the trees.

;;; classify_instance(
;;;         induce_rules(
;;;             [[colour 1 red green ] [shape 2 square oblong]],
;;;             [[[red square] SmallScrew] [[red oblong] BigScrew]
;;;                 [[green square] SmallNail] [[green oblong] BigNail]])) =>


;;; classify_instance(induce_rules(tests, instances))=>
;;;classify_instance(induce_rules(tests2, instances2))=>
;;; classify_instance(induce_rules(animal_tests, animal_instances))=>


;;; From Weka's zoo.arff example
;;; Well, it is very fast, but seems to be INCONSISTENT, and it's very hard
;;; to make any thing out of it since it's so huge.
vars zoo_instances = [
;;;; hair feathers eggs milk airborne aquatic predator toothed backbone breathes venomous fins legs tail domestic catsize type
[[true false false true false false true true true true false false 4 false false true 1] aardvark]
[ [true false false true false false false true true true false false 4 true false true 1] antelope]
[ [false false true false false true true true true false false true 0 true false false 4] bass]
[ [true false false true false false true true true true false false 4 false false true 1] bear]
[ [true false false true false false true true true true false false 4 true false true 1] boar]
[ [true false false true false false false true true true false false 4 true false true 1] buffalo]
[ [true false false true false false false true true true false false 4 true true true 1] calf]
[ [false false true false false true false true true false false true 0 true true false 4] carp]
[ [false false true false false true true true true false false true 0 true false false 4] catfish]
[ [true false false true false false false true true true false false 4 false true false 1] cavy]
[ [true false false true false false true true true true false false 4 true false true 1] cheetah]
[ [false true true false true false false false true true false false 2 true true false 2] chicken]
[ [false false true false false true true true true false false true 0 true false false 4] chub]
[ [false false true false false false true false false false false false 0 false false false 7] clam]
[ [false false true false false true true false false false false false 4 false false false 7] crab]
[ [false false true false false true true false false false false false 6 false false false 7] crayfish]
[ [false true true false true false true false true true false false 2 true false false 2] crow]
[ [true false false true false false false true true true false false 4 true false true 1] deer]
[ [false false true false false true true true true false false true 0 true false true 4] dogfish]
[ [false false false true false true true true true true false true 0 true false true 1] dolphin]
[ [false true true false true false false false true true false false 2 true true false 2] dove]
[ [false true true false true true false false true true false false 2 true false false 2] duck]
[ [true false false true false false false true true true false false 4 true false true 1] elephant]
[ [false true true false true false false false true true false false 2 true false true 2] flamingo]
[ [false false true false false false false false false true false false 6 false false false 6] flea]
[ [false false true false false true true true true true false false 4 false false false 5] frog]
[ [false false true false false true true true true true true false 4 false false false 5] frog]
[ [true false false true true false false true true true false false 2 true false false 1] fruitbat]
[ [true false false true false false false true true true false false 4 true false true 1] giraffe]
[ [true false false true false false true true true true false false 2 false true true 1] girl]
[ [false false true false true false false false false true false false 6 false false false 6] gnat]
[ [true false false true false false false true true true false false 4 true true true 1] goat]
[ [true false false true false false false true true true false false 2 false false true 1] gorilla]
[ [false true true false true true true false true true false false 2 true false false 2] gull]
[ [false false true false false true false true true false false true 0 true false false 4] haddock]
[ [true false false true false false false true true true false false 4 true true false 1] hamster]
[ [true false false true false false false true true true false false 4 true false false 1] hare]
[ [false true true false true false true false true true false false 2 true false false 2] hawk]
[ [false false true false false true true true true false false true 0 true false false 4] herring]
[ [true false true false true false false false false true true false 6 false true false 6] honeybee]
[ [true false true false true false false false false true false false 6 false false false 6] housefly]
[ [false true true false false false true false true true false false 2 true false false 2] kiwi]
[ [false false true false true false true false false true false false 6 false false false 6] ladybird]
[ [false true true false true false false false true true false false 2 true false false 2] lark]
[ [true false false true false false true true true true false false 4 true false true 1] leopard]
[ [true false false true false false true true true true false false 4 true false true 1] lion]
[ [false false true false false true true false false false false false 6 false false false 7] lobster]
[ [true false false true false false true true true true false false 4 true false true 1] lynx]
[ [true false false true false true true true true true false false 4 true false true 1] mink]
[ [true false false true false false true true true true false false 4 true false false 1] mole]
[ [true false false true false false true true true true false false 4 true false true 1] mongoose]
[ [true false true false true false false false false true false false 6 false false false 6] moth]
[ [false false true false false true true true true true false false 4 true false false 5] newt]
[ [false false true false false true true false false false false false 8 false false true 7] octopus]
[ [true false false true false false true true true true false false 4 true false false 1] opossum]
[ [true false false true false false false true true true false false 4 true false true 1] oryx]
[ [false true true false false false false false true true false false 2 true false true 2] ostrich]
[ [false true true false true false false false true true false false 2 true true false 2] parakeet]
[ [false true true false false true true false true true false false 2 true false true 2] penguin]
[ [false true true false true false false false true true false false 2 true false false 2] pheasant]
[ [false false true false false true true true true false false true 0 true false true 4] pike]
[ [false false true false false true true true true false false true 0 true false false 4] piranha]
[ [false false true false false false true true true true true false 0 true false false 3] pitviper]
[ [true false true true false true true false true true false false 4 true false true 1] platypus]
[ [true false false true false false true true true true false false 4 true false true 1] polecat]
[ [true false false true false false false true true true false false 4 true true true 1] pony]
[ [false false false true false true true true true true false true 0 true false true 1] porpoise]
[ [true false false true false false true true true true false false 4 true false true 1] puma]
[ [true false false true false false true true true true false false 4 true true true 1] pussycat]
[ [true false false true false false true true true true false false 4 true false true 1] raccoon]
[ [true false false true false false false true true true false false 4 true true true 1] reindeer]
[ [false true true false false false true false true true false false 2 true false true 2] rhea]
[ [false false false false false false true false false true true false 8 true false false 7] scorpion]
[ [false false true false false true false true true false false true 0 true false false 4] seahorse]
[ [true false false true false true true true true true false true 0 false false true 1] seal]
[ [true false false true false true true true true true false true 2 true false true 1] sealion]
[ [false false false false false true true true true false true false 0 true false false 3] seasnake]
[ [false false true false false true true false false false true false 0 false false false 7] seawasp]
[ [false true true false true true true false true true false false 2 true false false 2] skimmer]
[ [false true true false true true true false true true false false 2 true false false 2] skua]
[ [false false true false false false true true true true false false 0 true false false 3] slowworm]
[ [false false true false false false false false false true false false 0 false false false 7] slug]
[ [false false true false false true false true true false false true 0 true false false 4] sole]
[ [false true true false true false false false true true false false 2 true false false 2] sparrow]
[ [true false false true false false false true true true false false 2 true false false 1] squirrel]
[ [false false true false false true true false false false false false 5 false false false 7] starfish]
[ [false false true false false true true true true false true true 0 true false true 4] stingray]
[ [false true true false true true false false true true false false 2 true false true 2] swan]
[ [false false true false false false false false false true false false 6 false false false 6] termite]
[ [false false true false false true false true true true false false 4 false false false 5] toad]
[ [false false true false false false false false true true false false 4 true false true 3] tortoise]
[ [false false true false false false true true true true false false 4 true false false 3] tuatara]
[ [false false true false false true true true true false false true 0 true false true 4] tuna]
[ [true false false true true false false true true true false false 2 true false false 1] vampire]
[ [true false false true false false false true true true false false 4 true false false 1] vole]
[ [false true true false true false true false true true false false 2 true false true 2] vulture]
[ [true false false true false false false true true true false false 2 true false true 1] wallaby]
[ [true false true false true false false false false true true false 6 false false false 6] wasp]
[ [true false false true false false true true true true false false 4 true false true 1] wolf]
[ [false false true false false false false false false true false false 0 false false false 7] worm]
[ [false true true false true false false false true true false false 2 true false false 2] wren]

];

vars zoo_tests = 
   [
    [hair 1 false  true]
    [feathers 2 false  true]
    [eggs 3 false  true]
    [milk 4 false  true]
    [airborne 5 false  true]
    [aquatic 6 false  true]
    [predator 7 false  true]
    [toothed 8 false  true]
    [backbone 9 false  true]
    [breathes 10 false  true]
    [venomous 11 false  true]
    [fins 12 false  true]
    [legs 13 0 1 2 3 4 5 6 7 8 9]
    [tail 14 false  true]
    [domestic 15 false true]
    [catsize 16 false true]
    [type 17 1 2 3 4 5 6 7]
    
];

;;; '\n\ninduce_rules:'=>
;;; induce_rules(zoo_tests, zoo_instances) ==>

;;; This seems to work!
classify_instance(induce_rules(zoo_tests, zoo_instances))=>
