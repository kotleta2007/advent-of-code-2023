val doc = adv.readFromFile("01.input")

def convertToInt(c: Char): Option[Int] =
    c match
        case x if Character.isDigit(x) => Some(x.toInt - '0')
        case _ => None

doc
    .map(line => line.map(convertToInt).toList.flatten)
    .map(ints => 10 * ints.head + ints.last)
    .reduceLeft(_ + _)
