1. najdemo type karte
2. preverimo tiste, ki matchajo types
3. orderamo

5
4 1
3 2
3 1 1
2 2 1
2 1 1 1
1 1 1 1 1

# CAMEL CARDS

Vsak hand zmaga toliko kot je njen **bid amount × rank**. Rank je pač,
kam se uvrsti glede na ostale karte. Zadnji hand ima rank 1, medtem ko
ima najmočnejša length(hand). 

Trenutno imamo podatke v data frame:

    hand  bid
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483

## Urejanje glede na tip kart (prva runda)

Najprej orderamo karte glede na njihovo moč. Torej, glede na njihov tip.
Če se potem znajde več hands z istim tipom, jih moramo potem primerjati med
sabo.

### algoritem

Hranili bomo nov stolpec, ki se bo imenoval `type`.

    hand  bid type
    32T3K 765 ...
    T55J5 684 ...
    KK677 28  ...
    KTJJT 220 ...
    QQQJA 483 ...

Znotraj bodo shranjeni tipi od posamezne hand, tako kot je v besedilu

>AAAAA - five of a kind  = 7
>AAAAB - four of a kind  = 6
>AAABC - three od a kind = 5
>AAABB - full house      = 4
>AABBC - two pair        = 3
>AABCD - one pair        = 2
>ABCDE - high card       = 1

## Primerjanje med rokami (druga runda)

Gremo čez vsako karto, ko najdemo različno, preverimo katera roka ima
močnejšo karto - ta se postavi pred drugo.

>e.g hand 1 ima KK677 in hand 2 ima KTJJT. Obe sta two pair. Gledamo karte.
>Prvi karti sta K in K. Drugi karti sta K in T. Mismatch. K > T, zato dobi
>hand 1 višji rank kot hand 2.

## Moč rok in rezultat (tretja runda)

Na koncu množimo vsako roko (njen rank) z njenim bid amount. Seštejemo vse
produkte in vrnemo rezultat.


