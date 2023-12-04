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
            val around = lines.safeApply(lineNo - 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
                ++ lines.safeApply(lineNo + 1).getOrElse("").slice(oneBefore, oneAfter + 1).toList
                ++ line.safeApply(oneBefore).map(List(_)).getOrElse(Nil)
                ++ line.safeApply(oneAfter).map(List(_)).getOrElse(Nil)
            
            val symbolsAround = around.filter(!_.isDigit).filter(_ != '.')

            (number.map(_._1).mkString.toInt, symbolsAround)
        )
    )

parseNumbers(schematic).filter(_._2.nonEmpty).map(_._1).reduceLeft(_ + _)