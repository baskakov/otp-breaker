package controllers

import play.api._
import play.api.mvc._

import scala.annotation.tailrec

object Application extends Controller {
  import StringHex._
  import OTPBreaker._

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
    val xorVariants = xorRows(ciphertexts)
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

object OTPBreaker {
  @tailrec
  def xorRows(rows: List[String], acc:Map[(String,String), String] = Map.empty): Map[(String,String), String] = {
    rows match {
      case row :: Nil => acc
      case Nil => acc
      case a :: xs => xorRows(xs, acc ++ xs.map(b => (a,b) -> xorByKey(a, b)).toMap)
    }
  }
}

object StringHex {
  def xorByKey(keyHex: String, inputHex: String) = {
    inputHex.foldLeft(("", 0))({
      case ((xs, i), b) =>
        implicit def hexCharToInt(char: Char) = Integer.parseInt(char.toString, 16)
        println("zz "+b.toInt.toString)
        val msgChar = Integer.parseInt(b.toString, 16)
        val keyChar = Integer.parseInt(keyHex.charAt(i % keyHex.size).toString, 16)
        val xor = Integer.toHexString(msgChar ^ keyChar)
        (xs ++ xor, i + 1)
    })._1.toUpperCase
  }

  def textToHex: String => String = _.map(c => Integer.toHexString(c.toInt)).mkString("").toUpperCase

  def hexToText: String => String = hexToAscii.andThen(_.map(_.toChar.toString).mkString(""))

  def hexToAscii: String => Iterator[Int] = _.grouped(2).map(twoBytes => Integer.parseInt(twoBytes, 16))

  def asciiToHex: List[Int] => String = _.map(n => Integer.toHexString(n)).mkString("").toUpperCase
}