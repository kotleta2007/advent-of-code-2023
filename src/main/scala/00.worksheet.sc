val instructions = adv.readLineFromFile("00.input")

def convertToInt(c: Char) = 
    c match
        case '(' => +1
        case ')' => -1
        case _   => 0 // ignore all other characters

instructions.map(convertToInt).reduceLeft(_ + _)
