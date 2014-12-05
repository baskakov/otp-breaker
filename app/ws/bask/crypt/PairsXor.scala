package ws.bask.crypt

import scala.annotation.tailrec

object PairsXor {
  @tailrec
  def xorRows(rows: List[String], acc:Map[(String,String), String] = Map.empty): Map[(String,String), String] = {
    rows match {
      case row :: Nil => acc
      case Nil => acc
      case a :: xs => xorRows(xs, acc ++ xs.map(b => (a,b) -> StringHex.xorByKey(a, b)).toMap)
    }
  }
}
