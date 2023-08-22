# From https://github.com/mpfeifer1/Kattis/blob/master/cardtrick2.py
def main():
    n = int(raw_input())

    while n > 0:
        n -= 1
        cards = int(raw_input())
        deck = []

        for i in range(cards, 0, -1):
            deck.insert(0, i)

            for j in range(0, i):
                deck.insert(0, deck.pop())

        for i in deck:
            print str(i) + " ",

        print ""

if __name__ == "__main__":
    main()
    
