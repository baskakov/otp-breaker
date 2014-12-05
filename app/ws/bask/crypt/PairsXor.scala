package ws.bask.crypt

import ws.bask.crypt.PairsXor._

import scala.annotation.tailrec

object PairsXor {
  import StringHex._

  @tailrec
  def xorRows(rows: List[String], acc:Map[(String,String), String] = Map.empty): Map[(String,String), String] = {
    rows match {
      case row :: Nil => acc
      case Nil => acc
      case a :: xs => xorRows(xs, acc ++ xs.map(b => (a,b) -> StringHex.xorByKey(a, b)).toMap)
    }
  }

  def allPossiblePairs(rowNumber: Int) = rowPairsIndexes(0 until rowNumber toList)

  private def rowPairsIndexes(rows: List[Int], acc: Set[(Int,Int)] = Set.empty): Set[(Int,Int)] = rows match {
    case row :: Nil => acc
    case Nil => acc
    case a :: xs => rowPairsIndexes(xs, acc ++ xs.map(b => (a,b)).toSet)
  }
}

case class PairColumn(index: Int, pairs: List[Pair]) {
  lazy val guessedLetters: Set[LetterValue] = recursiveGuess(pairs)

  @tailrec
  private def recursiveGuess(pairs: List[Pair], acc: Set[LetterValue] = Set.empty[LetterValue]): Set[LetterValue] = pairs match {
    case mickey :: xs => recursiveGuess(xs, acc ++ xs.flatMap(mouse => mickey.joinGuess(mouse)).toList)
    case Nil => acc
  }
}

object PairColumns {
  def apply(asciiCiphers: List[List[Int]]) = {
    val rowSize = asciiCiphers.size
    val columnSize = asciiCiphers.map(_.size).max
    val pairIndexes = allPossiblePairs(rowSize)
    (0 until columnSize).map(columnIndex => PairColumn(columnIndex, {
      pairIndexes.map({
        case (rowA, rowB) => Pair(LetterValue(rowA, asciiCiphers(rowA)(columnIndex)), LetterValue(rowB, asciiCiphers(rowB)(columnIndex)))
      }).toList
    })).toList
  }
}

case class Pair(x: LetterValue, y: LetterValue) {
  lazy val xor = x.value ^ y.value

  lazy val xorBit = to7bitBinary(xor)

  lazy val guess = {
    if (xorBit.charAt(0) == '1') Or(32, 32 ^ xor)
    else if(xorBit.forall(_ == '0')) Same
    else AnyG
  }

  private def to7bitBinary(i: Int) = {
    val bin = i.toBinaryString
    if (bin.size < 7) (0 until 7 - bin.size).map(_ => "0").mkString("") + bin
    else bin
  }

  override def toString: String = "["+x.index.toString+","+y.index.toString+"]:"+guess.toString

  def isJoinedTo(other: Pair) =
    List(x.index, y.index, other.x.index, other.y.index).distinct.size == 3

  def joinGuess(other: Pair): List[LetterValue] = if(!isJoinedTo(other) || !guess.isInstanceOf[Or] || !other.guess.isInstanceOf[Or]) Nil else {

    val (leftLetterIndex, joinLetterIndex, rightLetterIndex) = {
      if(x == other.x) (y.index, x.index, other.y.index)
      else if(y == other.y) (x.index, y.index, other.x.index)
      else if(x == other.y) (y.index, x.index, other.x.index)
      else (x.index, y.index, other.y.index)
    }
    val (leftValue, joinValue, rightValue) = {
      val leftOr = guess.asInstanceOf[Or]
      val rightOr = other.guess.asInstanceOf[Or]
      if(leftOr.y == rightOr.x) (leftOr.x, leftOr.y,rightOr.y)
      else if(leftOr.y == rightOr.y) (leftOr.x, leftOr.y, rightOr.x)
      else if(leftOr.x == rightOr.x) (leftOr.y, leftOr.x, rightOr.y)
      else (leftOr.y, leftOr.x, rightOr.x)
    }

    List(
     LetterValue(leftLetterIndex, leftValue),
     LetterValue(joinLetterIndex, joinValue),
     LetterValue(rightLetterIndex, rightValue)
    )
  }

}

trait Guess

case class Or(x: Int, y: Int) extends Guess {
  override def equals(p1: scala.Any): Boolean = p1 match {
    case Or(a,b) => (a == x && b == y) || (a == y && b == x)
    case _ => false
  }

  override def toString: String = "("+x.toChar.toString+","+y.toChar.toString+")"
}

case object AnyG extends Guess

case object Same extends Guess

case class LetterValue(index: Int, value: Int)
