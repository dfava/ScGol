/** Game of Life
* @author Daniel Fava
# http://github.com/dfava
*/
import scala.util.Random
import scala.io.Source

abstract class Board(param : Either[String, (Double, Int)]) {
    // Create a board by either reading a file (String with file name) 
    // or randomly (Double with prob, and int with board size)
    var cells : Array[Array[Char]] = param match {
        case Left(fname) => 
            { (for (line <- Source.fromFile(fname).getLines()) yield line.toCharArray).toArray }
        case Right((prob, size)) => {
            require(prob >= 0 && prob <= 1); require(size > 0)
            for { row <- Array.ofDim[Char](size, size) } yield randRow(prob, size)
        }
    }
    val neighbors = for {row <- 0 to cells.length-1} yield // Compute and store indexes of all neighbors
            for {col <- 0 to cells.length-1} yield createNeighborList(row, col)

    private def randRow(prob : Double, size : Int) : Array[Char] = { // Creates a row of cells randomly
        for { col <- Array.ofDim[Char](size) } yield if (Random.nextDouble() > prob) '.' else 'X'
    }

    def update() { // Update a board's configuration according to its neighbors score
        var newCells = Array.ofDim[Char](cells.length, cells.length)
        for {row <- 0 to cells.length-1} {
            newCells(row) = Array.ofDim[Char](cells.length)
            for {col <- 0 to cells.length-1} {
                var score = 0.0
                for {n <- neighbors(row)(col)} { score += (if (cells(n._1)(n._2) == '.') 0 else 1 * n._3) }
                newCells(row)(col) = nextCellState(row, col, score)
            }
        }
        cells = newCells
    }

    def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int, Double)]
    def nextCellState(row : Int, col : Int, neighbor_score : Double) : Char
    override def toString() : String // Force subclass to implement toString
}

class BoardR8(param : Either[String, (Double, Int)]) extends Board(param) {
    // A square board with the eight neighbor rule
    def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int, Double)] = {
        (for {
            offsetRow <- -1 to 1
            offsetCol <- -1 to 1
            if !(offsetRow == 0 && offsetCol == 0)
            row = cellRow + offsetRow
            col = cellCol + offsetCol
            if row >= 0 && col >= 0 && row < cells.length && col < cells.length
        } yield (row,col,1.0)).toList
    }

    def nextCellState(row : Int, col : Int, neighbor_score : Double) : Char = {
        if ((cells(row)(col) == 'X' && List(2,3).contains(neighbor_score))
            || (cells(row)(col) == '.' && neighbor_score == 3)) 'X' else '.'
    }

    override def toString() = { (for { row <- cells } yield row.mkString).mkString("\n") }
}

class BoardRH6(param : Either[String, (Double, Int)]) extends BoardR8(param) {
    // A hex board with the six neighbor rule
    override def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int, Double)] = {
        (for {
            offsetRow <- -1 to 1
            offsetCol <- -1 to 1
            if !(offsetRow == 0 && offsetCol == 0)
            if !(cellRow % 2 == 0 && List((-1,1), (1,1)).contains((offsetRow, offsetCol)))
            if !(cellRow % 2 == 1 && List((-1,-1), (1,-1)).contains((offsetRow, offsetCol)))
            row = cellRow + offsetRow
            col = cellCol + offsetCol
            if row >= 0 && col >= 0 && row < cells.length && col < cells.length
        } yield (row,col,1.0)).toList
    }

    override def toString() = { (for { rowIdx <- 0 to cells.length-1 }
            yield (if (rowIdx % 2 == 1) " " else "") + cells(rowIdx).mkString(" ")).mkString("\n") }
}

class BoardRH12(param : Either[String, (Double, Int)]) extends BoardRH6(param) {
    // A hex board with the twelve neighbor rule
    override def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int, Double)] = {
        // Same neighbors as in the hex6 configuration plus 6 tier2 neighbors
        val tier2 = List((cellRow-2, cellCol), (cellRow+2, cellCol)) ++ 
                (if (cellRow % 2 == 0) List((cellRow-1, cellCol-2), (cellRow-1, cellCol+1), (cellRow+1, cellCol-2), (cellRow+1, cellCol+1)) 
                    else List((cellRow-1, cellCol-1), (cellRow-1, cellCol+2), (cellRow+1, cellCol-1), (cellRow+1, cellCol+2)))
        super.createNeighborList(cellRow, cellCol) ++
            (for { // Make sure tier2 is still within the board (i.e. it isn't below 0 or above cells.length)
                (row, col) <- tier2
                if row >= 0 && col >= 0 && row < cells.length && col < cells.length
            } yield (row, col, 0.3))
    }

    override def nextCellState(row : Int, col : Int, neighbor_score : Double) : Char = {
        if ((cells(row)(col) == '.' && (2.3 < neighbor_score && neighbor_score < 2.9)) || 
            (cells(row)(col) == 'X' && (2.0 < neighbor_score && neighbor_score < 3.3))) 'X' else '.'
    }
}

object Board { // A generator of boards. Creates a board given a "board type" (6, 8, or 12)
    def construct(params : Either[String, (Double, Int)], boardType : String) : Board = boardType match {
        case "6" => new BoardRH6(params)
        case "8" => new BoardR8(params)
        case "12" => new BoardRH12(params)
    }
}

import sys.exit
object gol {
    val usage = """usage: gol [-h] [-6] [-8] [-12] [-size SIZE] [-f FNAME]
              [-g GENS] [-p GEN_PRINT] [-i PROB]

Game of Live

optional arguments:
  -h, -help             show this help message and exit
  -6                    use 6 neighbor rules on a hex grid (default: 6)
  -8                    use 8 neighbor rules on a rectangular grid (default: 6)
  -12                   use 12 neighbor rules on a hex grid (default: 6)
  -size SIZE            set grid size to SIZE by SIZE (default: 100)
  -f FNAME              read initial configuration from file
  -g GENS               number of generations to simulate (default: 10)
  -p GEN_PRINT          print every nth generation (default: 1)
  -i PROB               probabilty of cell alive initially (default: 0.5)"""

    def main(args: Array[String]) {
        val default = Map('size -> "100", 'gens -> "10", 'genPrint -> "1", 'prob -> "0.5", 'boardType -> "6")
        type OptionMap = Map[Symbol, String]
        def parse(map : OptionMap, list : List[String]) : OptionMap = {
            def isSwitch(s : String) = (s(0) == '-')
            list match {
                case Nil => map // Base case
                case "-size" :: value :: tail if value.toInt > 0 => parse(map ++ Map('size -> value), tail)
                case "-f" :: value :: tail => parse(map ++ Map('fname -> value), tail)
                case "-g" :: value :: tail if value.toInt >= 0 => parse(map ++ Map('gens -> value), tail)
                case "-p" :: value :: tail if value.toInt > 0 => parse(map ++ Map('genPrint -> value), tail)
                case "-i" :: value :: tail if value.toDouble >= 0 && value.toDouble <= 1 => parse(map ++ Map('prob -> value), tail)
                case (e @ option) :: tail if List("-6", "-8", "-12").contains(option) => if (map.contains('boardType)) { println("Info: Board configuration specified more than once.  Overriding previous rule."); } else {}; parse(map ++ Map('boardType -> e.slice(1,e.length)), tail)
                case option :: tail if List("-h", "-help").contains(option) => println(usage); exit(0)
                case option :: tail if List("-size", "-f", "-g", "-p", "-i").contains(option) => println("Invalid value passed to " + option); exit(1)
                case option :: tail => println("ERR: Unknown option " + option); exit(1)
            }
        }
        val options = try { parse(Map(), args.toList) } catch { case e : NumberFormatException => println("Invalid value passed to parameter"); exit(1) }
        if (options.contains('fname)) {
            if (options.contains('size)) println("Info: Ignoring -size. Reading size from file")
            if (options.contains('prob)) println("Info: Ignoring -i. Getting board from file")
        }
        val mergedOpts = default ++ options // Override the defaults
        val b = mergedOpts match {
            case opt if opt.contains('fname) => Board.construct(Left(opt('fname)), opt('boardType))
            case opt => Board.construct(Right((opt('prob).toDouble, opt('size).toInt)), opt('boardType))
        }
        for { gen <- 0 to mergedOpts('gens).toInt } {
            if (gen % mergedOpts('genPrint).toInt == 0) { println("Gen " + gen); println(b) }
            b.update();
        }
    }
}
