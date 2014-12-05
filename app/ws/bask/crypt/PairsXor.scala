package ws.bask.crypt

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

case class PairColumn(index: Int, pairs: Iterable[Pair])

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
