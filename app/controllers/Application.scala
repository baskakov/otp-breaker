package controllers

import ws.bask.crypt._
import play.api._
import play.api.mvc._

import scala.annotation.tailrec

object Application extends Controller {

  import StringHex._
  import PairsXor._

  def index = Action { request =>
    val query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    val ciphertexts = query.get("ciphertexts").flatMap(_.headOption)
    val res = ciphertexts.map(c => break(c.split("\r\n").toList))
    Ok(views.html.index(ciphertexts, None, res))
  }

  private def break(ciphertexts: List[String]) = {
    val asciiCiphers = ciphertexts.map(hexToAscii)
    val rowSize = ciphertexts.size
    val columnSize = asciiCiphers.map(_.size).max
    val pairIndexes = allPossiblePairs(rowSize)
    val columnsData = (0 until columnSize).map(columnIndex => PairColumn(columnIndex, {
      pairIndexes.map({
        case (rowA, rowB) => Pair(LetterValue(rowA, asciiCiphers(rowA)(columnIndex)), LetterValue(rowB, asciiCiphers(rowB)(columnIndex)))
      }).toList
    })).toList
    val res = (0 until columnSize).toList.map(col => {
      (0 until rowSize).toList.map(rowA => (0 until rowSize).toList.map(rowB => {
        columnsData.find(_.index == col).flatMap(_.pairs.find({
          case p => p.x.index == rowB && p.y.index == rowA
        }))
      }))
    })
    res
  }
}


