/* 

  Playing cards in JavaScript.

  http://rosettacode.org/wiki/Playing_cards
  """
  Task

  Create a data structure and the associated methods to define and 
  manipulate a deck of playing cards.

  The deck should contain 52 unique cards.

  The methods must include the ability to:

  - make a new deck
  - shuffle (randomize) the deck
  - deal from the deck
  - print the current contents of a deck 

  Each card must have a pip value and a suit value which constitute 
  the unique value of the card. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {shuffle,randomInt} = require('./js_utils.js');


class Deck {
    // Init
    constructor(id) {
        this._id = id;
        // this._suits = ["C","H","S","D"];
        this._suits = ["♠","♥","♦","♣"];
        // Note: T = 10
        this._values = ["A","2","3","4","5","6","7","8","9","T","J","Q","K"];
        
        this._deck = [];
        for(let s of this._suits) {
            for(let v of this._values) {
                // this._deck.push([s,v]);
                this._deck.push(s+" "+v);                
            }
        }

        this._size  = this._deck.length;
    }

    // Getters
    get id()     { return this._id;     }
    get values() { return this._values; }
    get suits()  { return this._suits;  }
    get deck()   { return this._deck.toString();   }
    get size()   { this._size = this._deck.length; return this._deck.length; }

    // Methods
    shuffle() {
        this._deck = shuffle(this._deck);
    }

    deal(num_cards) {
        if (num_cards > this.size) {
            throw new Error(`Sorry: the deck has only ${this._size} cards left!`);
        } else {
            const hand = this._deck.splice(0,num_cards);
            return hand;
        }
        // Recalculate size
        this._size = this._deck.length;
    }
    
    toString() {
        return `Deck(${this._id}): ` + this._deck.join(" ");
    }

}


function testDeck() {

    const deck = new Deck(0);
    console.log("new deck:");
    console.log(deck.deck);
    
    deck.shuffle();
    console.log("after shuffling:");
    console.log(deck.deck);
    console.log(deck.toString());
    
    let hand = deck.deal(5);
    console.log("hand:",hand);
    console.log(deck.deck);
    
    try {
        while(hand = deck.deal(5)) {
            console.log("hand:", hand, " cards left: ", deck.size);
        }
    } catch(e) {
        console.log(e);
    }
    
    console.log("cards left:", deck.deck);
    console.log();

    
    const deck2 = new Deck(1);
    deck2.shuffle();
    console.log(`new deck: ${deck2}`);
    try {
        const s = 2;
        while(deck2.size >= s) {
            hand = deck2.deal(s);            
            console.log("hand:", hand, " cards left: ", deck2.size);
        }
    } catch(e) {
        console.log(e);
    }
}

testDeck();
