package controllers

import ws.bask.crypt._
import play.api._
import play.api.mvc._

import scala.annotation.tailrec

object Application extends Controller {
  import StringHex._

  def index = Action { request =>
    val query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    val ciphertexts = query.get("ciphertexts").flatMap(_.headOption)
    val res = ciphertexts.map(c => {
      val (msg, info) = break(c.split("\r\n").toList)
      val messagesText = msg.mkString("\r\n")
      messagesText -> info
    })
    Ok(views.html.index(ciphertexts, res.map(_._1), res.map(_._2).getOrElse(Nil)))
  }

  private def break(ciphertexts: List[String]) = {
    val xorVariants = PairsXor.xorRows(ciphertexts)
    val possibilities = xorVariants.foldLeft(Map[(String, Int), Guess]())({
      case (acc, ((a,b), xor)) =>
        val asciiXor = hexToAscii(xor).zipWithIndex
        asciiXor.foldLeft(acc)({
          case (acc, (asciiChar,index)) => {
            val binChar = to7bitBinary(asciiChar)
            println(binChar)
            val possible =
              if(binChar.charAt(0) == '1') Or(32, 32 ^ asciiChar)
              else AnyG
            println(possible.toString)
            val guessA = acc.get((a,index)).getOrElse(AnyG) + possible
            val guessB = acc.get((b,index)).getOrElse(AnyG) + possible
            acc ++ Map((a,index) -> guessA, (b,index) -> guessB)
          }
        })
    })
    val result = ciphertexts.map(cipher => {

      val psb = possibilities.filterKeys(_._1 == cipher).toList.sortBy(_._1._2).map({
        case ((_,i),p) => i.toString+": "+p.toString
      }).mkString("\r\n")

      val res = hexToAscii(cipher).zipWithIndex.map({
        case (char,index) => (possibilities.get((cipher, index)) match{
          case Some(Got(foundChar)) => foundChar.toChar.toString
          case _ => "_"
        })
      }).mkString("")

      psb -> res
    })

    result.map(_._2) -> result
  }

  trait Guess {
    def +(b: Guess): Guess
  }
  case object AnyG extends Guess {
    override def +(b: Guess): Guess = b
  }
  case class No(gss: List[Guess]) extends Guess {
    override def +(b: Guess): Guess = if(gss.contains(b)) this else No(gss :+ b)
  }
  case class Or(a: Int, b: Int) extends Guess {
    override def +(c: Guess): Guess = c match {
      case or@Or(d,e) => List(a,b).intersect(List(d,e)) match {
        case f :: g :: Nil => Or(f,g)
        case f :: Nil => Got(f)
        case Nil => No(this :: or :: Nil)
      }
      case Got(g) => if(g == a || g == b) Got(g) else No(this :: Got(g) :: Nil)
      case AnyG => this
      case No(gss) => if(gss.contains(this)) No(gss) else No(gss :+ this)
    }
  }
  case class Got(a: Int) extends Guess {
    override def +(b: Guess): Guess = this
  }

  private def to7bitBinary(i: Int) = {
    val bin = i.toBinaryString
    if(bin.size < 7) (0 until 7 - bin.size).map(_ => "0").mkString("") + bin
    else bin
  }



}

