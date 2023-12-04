val games = adv.readFromFile("02.input")
// val games = List(
//     "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
//     "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
//     "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
//     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
//     "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
// )

val reference = List((12, Color.Red), (13, Color.Green), (14, Color.Blue))

enum Color:
    case Red, Green, Blue

def parseGame(s: String) = 
    def convertToRLE(s: String) = 
        val List(count, colorName) = s.split(" ").toList
        val color = colorName match
            case "red" => Color.Red
            case "green" => Color.Green
            case "blue" => Color.Blue
        (count.toInt, color)
        
    val List(gameString, drawsString) = s.split(":").toList
    
    val draws = drawsString.split(";").toList.map(_.trim())
    val parsedDraws = draws.map(draw => draw.split(",").toList.map(colorDraw => convertToRLE(colorDraw.trim())))

    val game = gameString.split(" ").toList.last.toInt

    (game, parsedDraws)

def leq(draw1: List[(Int, Color)], draw2: List[(Int, Color)]): Boolean = 
    val comparisonPerColor = draw1.map(colorCount => 
        val sameColorIn2 = draw2.find(_._2 == colorCount._2)
        val countIn2 = sameColorIn2.map(_._1).getOrElse(0)

        colorCount._1 <= countIn2
    )
    comparisonPerColor.forall(identity)

// Part 1
games.map(game => 
    val (gameNumber, draws) = parseGame(game)
    (gameNumber, draws.forall(draw => leq(draw, reference)))
).filter(_._2).map(_._1).reduceLeft(_ + _)

// Part 2
games.map(game =>
    val (gameNumber, draws) = parseGame(game)
    var minRed = 0
    var minGreen = 0
    var minBlue = 0

    draws.foreach(draw => 
        val red = draw.find(_._2 == Color.Red).map(_._1).getOrElse(0)
        val green = draw.find(_._2 == Color.Green).map(_._1).getOrElse(0)
        val blue = draw.find(_._2 == Color.Blue).map(_._1).getOrElse(0)
        
        if red > minRed then minRed = red
        if green > minGreen then minGreen = green
        if blue > minBlue then minBlue = blue
    )

    minRed * minGreen * minBlue
).reduceLeft(_ + _)