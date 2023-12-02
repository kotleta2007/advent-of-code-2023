val doc = adv.readFromFile("01.input")

// Part 1

def convertToInt(c: Char): Option[Int] =
    c match
        case x if Character.isDigit(x) => Some(x.toInt - '0')
        case _ => None

val part1 = doc
    .map(line => line.map(convertToInt).toList.flatten)
    .map(ints => 10 * ints.head + ints.last)
    .reduceLeft(_ + _)

// Part 2

// parse once from the left
// once from the right
// take the leftmost element from the left
// and the rightmost element from the right

def wordToInt(w: String) = 
    w match
        case "one"   => 1
        case "two"   => 2
        case "three" => 3
        case "four"  => 4
        case "five"  => 5
        case "six"   => 6
        case "seven" => 7
        case "eight" => 8
        case "nine"  => 9
        case _ if w.forall(Character.isDigit) => w.toInt

def parseLine(line: String) = 
    val numberWords: List[String] = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    def validWord(word: String): Boolean =
        val isDigit = word.forall(Character.isDigit) && word.length == 1
        val isNumberWord = numberWords.contains(word)

        isDigit || isNumberWord
    
    def suffix(word: String): Option[String] = 
        if word.isEmpty then None
        else if validWord(word) then Some(word)
        else
            val smallerWord = word.slice(1, word.length())
            suffix(smallerWord)

    def parseTail(lineToParse: String, currentWord: String, parsedWords: List[String]): List[String] = 
        if lineToParse.isEmpty && suffix(currentWord).isEmpty then parsedWords
        else 
            if suffix(currentWord).nonEmpty then parseTail(lineToParse, "", suffix(currentWord).get :: parsedWords)
            else parseTail(lineToParse.tail, s"${currentWord}${lineToParse.head}", parsedWords)
    
    parseTail(line, "", Nil).reverse

def parseLineRight(line: String) = 
    val numberWords: List[String] = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    def validWord(word: String): Boolean =
        val isDigit = word.forall(Character.isDigit) && word.length == 1
        val isNumberWord = numberWords.contains(word)

        isDigit || isNumberWord
    
    def prefix(word: String): Option[String] = 
        if word.isEmpty then None
        else if validWord(word) then Some(word)
        else
            val smallerWord = word.slice(0, word.length() - 1)
            prefix(smallerWord)

    def parseTail(lineToParse: String, currentWord: String, parsedWords: List[String]): List[String] = 
        if lineToParse.isEmpty && prefix(currentWord).isEmpty then parsedWords
        else 
            if prefix(currentWord).nonEmpty then parseTail(lineToParse, "", prefix(currentWord).get :: parsedWords)
            else parseTail(lineToParse.tail, s"${lineToParse.head}${currentWord}", parsedWords)
    
    parseTail(line.reverse, "", Nil).reverse

doc
    .map(line => (wordToInt(parseLine(line).head), wordToInt(parseLineRight(line).head)))
    .map(ints => 10 * ints._1 + ints._2)
    .reduceLeft(_ + _)
