val pile = adv.readFromFile("04.input")
// val pile = List(
//     "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
//     "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
//     "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
//     "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
//     "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
//     "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
// )

extension (b: Boolean)
    def toInt = if b then 1 else 0

// Part 1
def parseCards(cards: List[String]) = 
    cards.map(card =>
        val List(prefix, numbers) = card.split(":").toList
        val List(_, cardNumberString) = prefix.split(" +").toList
        val cardNumber = cardNumberString.toInt

        val List(winningUntrimmed, yoursUntrimmed) = numbers.split("\\|").toList
        val (winningString, yoursString) = (winningUntrimmed.trim(), yoursUntrimmed.trim())

        def parseToNumbers(s: String) = s.split(" +").toList.filter(_.nonEmpty).map(_.toInt)
        val (winning, yours) = (parseToNumbers(winningString), parseToNumbers(yoursString))
        
        val winningNumbers = yours.map(yourNumber => winning.contains(yourNumber)).foldLeft(0)(_ + _.toInt)
        winningNumbers
    )

def toPoints(n: Int) = if n >= 1 then 1 << (n - 1) else 0
val winningNumbersPerCard = parseCards(pile)
winningNumbersPerCard.map(toPoints).reduceLeft(_ + _)

// Part 2
def originalsAndPoints(card: Int, scratchcards: List[Int]): List[Int] = 
    if card == scratchcards.length then scratchcards
    else
        val matchingNo = winningNumbersPerCard(card)
        val currentCard = scratchcards(card)

        val before = scratchcards.take(card + 1)
        val nextCards = scratchcards.drop(card + 1).take(matchingNo)
        val after = scratchcards.drop(card + 1).drop(matchingNo)

        val newNextCards = nextCards.map(_ + currentCard)

        originalsAndPoints(card + 1, before ++ newNextCards ++ after)

originalsAndPoints(0, List.fill(pile.length)(1)).reduceLeft(_ + _)
