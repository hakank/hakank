/* 

  Zebra puzzle in JavaScript.

  From example zebra-brute-force.stlx
  (via my http://hakank.org/picat/zebra_brute_force.pi )
  """
  This program solves the zebra puzzle, which is stated below.
  
  01. There are five houses. Each house is painted a different color, 
      and their inhabitants are of different nationalities, own different 
      pets, drink different beverages and smoke different brands of 
      cigarettes.
  02. The Englishman lives in the red house.
  03. The Spaniard owns the dog.
  04. Coffee is drunk in the green house.
  05. The Ukrainian drinks tea.
  06. The green house is immediately to the right of the ivory house.
  07. The Gold smoker owns snails.
  08. Kools are smoked in the yellow house.
  09. Milk is drunk in the middle house.
  10. The Norwegian lives in the first house.
  11. The man who smokes West lives in the house next to the man with the fox.
  12. Kools are smoked in a house next to the house where the horse is kept.
  13. The man who smokes Luckies drinks orange juice.
  14. The Japanese smokes Camel.
  15. The Norwegian lives next to the blue house.
  Who drinks water? Who owns the zebra?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {all_permutations} = require('./js_utils.js');


// This approach was inspired by SetlX's example zebra-brute-force.stlx
// Is house h1 right of house h2?
function isRightOf(h1, h2) {
    return h1 === h2 + 1;
}

// Is house h1 next to house h2?
function nextTo(h1, h2) {
    return Math.abs(h1 - h2) === 1;
}
/*
  Answer:
  colors:        3,5,4,1,2
  nationalities: 3,4,2,5,1
  drinks:        5,2,3,4,1
  pet:           4,3,1,2,5
  cigarettes:    3,1,2,4,5
  water: 1,zebra: 5

*/
function zebra1() {
    console.log("zebra1");
    const first  = 1;
    const middle = 3
    let result = [];
    let orderings = all_permutations([1,2,3,4,5]);    
    for (let [red, green, ivory, yellow, blue] of orderings) {
        if (isRightOf(green, ivory)) {
            for (let [english, spaniard, ukrainian, japanese, norwegian] of orderings) {
                if (english === red && norwegian === first && nextTo(norwegian, blue)) {
                    for (let [coffee, tea, milk, orange, water] of orderings) {
                        if (coffee === green && ukrainian === tea && milk === middle) {
                            for (let [gold, kools, west, luckies, camel] of orderings) {
                                if (kools === yellow && luckies === orange && japanese === camel) {
                                    for (let [dog, snails, fox, horse, zebra] of orderings) {
                                        if (    spaniard === dog 
                                                && gold === snails 
                                                && nextTo(west, fox) 
                                                && nextTo(kools, horse)
                                           ) 
                                        {
                                            console.log(`colors:        ${[red, green, ivory, yellow, blue]}`);
                                            console.log(`nationalities: ${[english, spaniard, ukrainian, japanese, norwegian]}`);
                                            console.log(`drinks:        ${[coffee, tea, milk, orange, water]}`);
                                            console.log(`pet:           ${[dog, snails, fox, horse, zebra]}`);
                                            console.log(`cigarettes:    ${[gold, kools, west, luckies, camel]}`);
                                            result += ["water: " + water, "zebra: " + zebra];
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return result;

}


console.log(zebra1());


