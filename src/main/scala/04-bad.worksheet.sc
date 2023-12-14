// winningNumbersPerCard
// val originals = List.fill(pile.length)(1)

// zipWithIndex
// slice
// recursive function

// at every call, you have a list of updates of size (N-1)
// think like dynamic programming
// try to solve it in one pass

// if currentWinningNumbers == 2, the update list is 
// (1 1) ++ (0 0 ... 0)
// (2 times) and (N-1-2 times)

def originalsAndCopies(updates: List[Int]): Int = 
    val current = pile.length - (updates.length + 1) // invariant: updates.length is always N-1
    // println("at card " + current)
    if updates.isEmpty then 1
    else 
        val currentWinningNumbers = winningNumbersPerCard(current)
        // println(currentWinningNumbers)
        // zip
        val newUpdates = List.fill(currentWinningNumbers)(1 + updates.head) ++ List.fill(updates.length - currentWinningNumbers)(0)
        // println(newUpdates)
        // println(updates.tail)
        // println(newUpdates + updates.tail)
        val mergedUpdates = newUpdates.zip(updates.tail).map(p => p._1 + p._2)

        // println("old updates: " + updates)
        // println("new updates: " + newUpdates)
        
        println(1 + updates.head)

        // println("merged updates: " + mergedUpdates)
        // println("val for current: " + (updates.head + 1))
        (1 + updates.head) + originalsAndCopies(mergedUpdates)

originalsAndCopies(List.fill(pile.length - 1)(0))

winningNumbersPerCard
val originals = List.fill(winningNumbersPerCard.length)(1).toArray

def scratchcards(currentCard: Int): Unit =
    if currentCard < (originals.length) then
        val currentWinningNumber = winningNumbersPerCard(currentCard)
        originals
            .slice(currentCard + 1, currentCard + 1 + currentWinningNumber)
            .map(_ + originals(currentCard))
            .copyToArray(originals, currentCard + 1)
        scratchcards(currentCard + 1)
    else
        ()

scratchcards(0)


originals.toList
originals.toList.reduceLeft(_ + _)



pile.length
winningNumbersPerCard.length


// def part2(card: Int, scratchcards: List[Int]): List[Int] =
//     if card == scratchcards.length then scratchcards
//     else 
//         // get winning numbers
//         val winningNumbers = winningNumbersPerCard(card)
//         val newNumber = scratchcards(card)
//         val newNumbers = scratchcards.slice(card + 1, card + 1 + winningNumbers + 1).map(_ + scratchcards(card))
//         val newScratchcards = 
//             scratchcards.slice(0, card + 1) 
//             ++ newNumbers 
//             ++ scratchcards.slice(card + 1 + winningNumbers + 1, 
//                 pile.length - (card + 1 + winningNumbers + 1))
        
//         println(newScratchcards)

//         part2(card + 1, newScratchcards)

// part2(0, List.fill(pile.length)(1))


def part2(card: Int, scratchcards: List[Int]): List[Int] = 
    if card == scratchcards.length then scratchcards
    else
        val matchingNo = winningNumbersPerCard(card)
        val before = scratchcards.take(card+1)
        val toReplace = scratchcards.drop(card+1).take(matchingNo)
        val after = scratchcards.drop(card+1).drop(matchingNo)

        val newReplace = toReplace.map(_ + scratchcards(card))

        part2(card + 1, before ++ newReplace ++ after)

part2(0, List.fill(pile.length)(1)).reduceLeft(_ + _)
