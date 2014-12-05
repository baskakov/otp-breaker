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
    val res = ciphertexts.map(c => break(c.split("\r\n").toList).mkString("\r\n"))
    Ok(views.html.index(ciphertexts, res))
  }

  private def break(ciphertexts: List[String]) = {
    val asciiCiphers = ciphertexts.map(hexToAscii)
    val pairColumns = PairColumns(asciiCiphers)

    (0 until ciphertexts.size).toList.map(rowIndex => pairColumns.map(pairColumn => {
      pairColumn.guessedLetters.find(_.index == rowIndex).map(_.value.toChar.toString).getOrElse("?")
    }).mkString(""))
  }
}


