val instructions = adv.readLineFromFile("00.input")

def convertToInt(c: Char) = 
    c match
        case '(' => +1
        case ')' => -1
        case _   => 0 // ignore all other characters

// Part 1
instructions.myMap(convertToInt).myReduceLeft(_ + _)

// Part 2
instructions.myMap(convertToInt).myScanLeft(0)(_ + _).myTakeWhile(_ != -1).myLength

extension[A, B] (i: Seq[A])
    def myMap(f: A => B): Seq[B] = 
        if i.isEmpty then Seq.empty
        else 
            val newHead = f(i.head)
            val newTail = i.tail.myMap(f)
            newHead +: newTail
    
    def myReduceLeft(op: (A, A) => A): A = 
        if i.length == 1 then i.head
        else op(i.head, i.tail.myReduceLeft(op))
    
    def myScanLeft(z: B)(op: (B, A) => B): Seq[B] = 
        if i.isEmpty then Seq(z)
        else z +: i.tail.myScanLeft(op(z, i.head))(op)
    
    def myTakeWhile(p: A => Boolean): Seq[A] = 
        if i.isEmpty then Seq.empty
        else
            if p(i.head) then i.head +: i.tail.takeWhile(p)
            else i.tail.takeWhile(p)
    def myLength: Int = 
        if i.isEmpty then 0
        else 1 + i.tail.myLength
