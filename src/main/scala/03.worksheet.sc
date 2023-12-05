val schematic = adv.readFromFile("03.input").toIndexedSeq
// val schematic = IndexedSeq(
//     "467..114..",
//     "...*......",
//     "..35..633.",
//     "......#...",
//     "617*......",
//     ".....+.58.",
//     "..592.....",
//     "......755.",
//     "...$.*....",
//     ".664.598.."
// )

extension[A] (s: Seq[A])
    def filterConsecutive(p: A => Boolean): Seq[Seq[A]] = 
        if s.isEmpty then Seq.empty
        else
            val (in, next) = s.dropWhile(!p(_)).span(p)
            in +: next.filterConsecutive(p)

    def safeApply(i: Int): Option[A] = 
        try 
            Some(s.apply(i)) 
        catch
            case e: IndexOutOfBoundsException => None

// Part 1
def parseNumbers(lines: IndexedSeq[String]) = 
    lines.zipWithIndex.flatMap((line, lineNo) =>
        // get all numbers
        val numbers = line.zipWithIndex.filterConsecutive(_._1.isDigit)

        // get all their adjacent squares
        numbers.filter(_.nonEmpty).map(number =>
            val oneBefore = number.head._2 - 1
            val oneAfter  = number.last._2 + 1
            
            // get the squares in the line above,
            // the line below
            // and next to the number in the current line
            val around = 
                lines.safeApply(lineNo - 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
                ++ lines.safeApply(lineNo + 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
                ++ line.safeApply(oneBefore).map(List(_)).getOrElse(Nil)
                ++ line.safeApply(oneAfter).map(List(_)).getOrElse(Nil)
            
            val symbolsAround = around.filter(!_.isDigit).filter(_ != '.')

            (number.map(_._1).mkString.toInt, symbolsAround)
        )
    )

parseNumbers(schematic).filter(_._2.nonEmpty).map(_._1).reduceLeft(_ + _)

// Part 2
def numbersWithLocation(lines: IndexedSeq[String]) = 
    lines.zipWithIndex.flatMap((line, lineNo) =>
        // get all numbers
        val numbersWithIndices = line.zipWithIndex.filterConsecutive(_._1.isDigit).filter(_.nonEmpty)
        
        numbersWithIndices.map(number => 
            val numberAsInt = number.map(_._1).mkString.toInt
            val startEnd = number.map(_._2)
            (numberAsInt, lineNo, startEnd)
        )
    )

val numbersIJ = numbersWithLocation(schematic)

def getNumber(i: Int, j: Int) = 
    numbersIJ.find(x => x._2 == i && x._3.contains(j)).map(_._1)

def gears(lines: IndexedSeq[String]) = 
    def getSurroundingChars(i: Int, j: Int) = 
        val oneBefore = j - 1
        val oneAfter  = j + 1
            
        // get the squares in the line above,
        // the line below
        // and next to the given position in the current line
        val above = lines.safeApply(i - 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
        val below = lines.safeApply(i + 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
        val left = lines(i).safeApply(oneBefore).map(List(_)).getOrElse(Nil)
        val right = lines(i).safeApply(oneAfter).map(List(_)).getOrElse(Nil)

        val aboveWithIndices = (above, i - 1, oneBefore to oneAfter)
        val belowWithIndices = (below, i + 1, oneBefore to oneAfter)
        val leftWithIndices  = (left, i, oneBefore)
        val rightWithIndices = (right, i, oneAfter)
        
        (aboveWithIndices, belowWithIndices, leftWithIndices, rightWithIndices)
    
    def parseThreeSymbols(chars: List[Char], lineNo: Int, js: Range) = 
        val List(a, b, c) = chars
        val (j1, j2, j3) = (js.start, js.start + 1, js.start + 2)

        (a, b, c) match
            case (a, b, c) if !a.isDigit && !b.isDigit && !c.isDigit => Nil
            case (a, b, c) if a.isDigit && !b.isDigit && !c.isDigit  => List((lineNo, j1))
            case (a, b, c) if !a.isDigit && b.isDigit && !c.isDigit  => List((lineNo, j2))
            case (a, b, c) if !a.isDigit && !b.isDigit && c.isDigit  => List((lineNo, j3))
            case (a, b, c) if !a.isDigit && b.isDigit && c.isDigit   => List((lineNo, j2))
            case (a, b, c) if a.isDigit && !b.isDigit && c.isDigit   => List((lineNo, j1), (lineNo, j3))
            case (a, b, c) if a.isDigit && b.isDigit && !c.isDigit   => List((lineNo, j1))
            case (a, b, c) if a.isDigit && b.isDigit && c.isDigit    => List((lineNo, j1))
    
    def gearNumbers(i: Int, j: Int) = 
        // checks if it's a gear, returns two gear number locations
        val (above, below, left, right) = getSurroundingChars(i, j)

        val numbersAbove = parseThreeSymbols.tupled(above)
        val numbersBelow = parseThreeSymbols.tupled(below)
        val numbersLeft  = if left._1.head.isDigit then List((left._2, left._3)) else Nil
        val numbersRight = if right._1.head.isDigit then List((right._2, right._3)) else Nil
        
        val numbersAround = numbersAbove ++ numbersBelow ++ numbersLeft ++ numbersRight

        if numbersAround.length != 2 then None
        else Some(numbersAround)

    val gearLocations = lines.zipWithIndex.flatMap((line, lineNo) =>
        val anyAsterisks = line.zipWithIndex.filter(_._1 == '*')

        anyAsterisks.map(asterisk =>
            gearNumbers(lineNo, asterisk._2)
        )
    )

    gearLocations.filter(_.isDefined).map(location =>
        location.get.map(getNumber.tupled(_).get).reduceLeft(_ * _)
    ).reduceLeft(_ + _)

gears(schematic)