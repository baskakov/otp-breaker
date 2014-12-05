import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import ws.bask.crypt._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class OTPBreakerSpec extends Specification {

  "StringHex" should {

    val foobar = ("Foo Bar", "466F6F20426172", List(70,111,111,32,66,97,114))
    import StringHex._
    "Transform text to hex" in {
      textToHex(foobar._1) must_== foobar._2
    }

    "Transform hex to text" in {
      hexToText(foobar._2) must_== foobar._1
    }

    "Transform hex to ascii" in {
      hexToAscii(foobar._2) must_== foobar._3
    }

    "Transform ascii to hex" in {
      asciiToHex(foobar._3) must_== foobar._2
    }

    "Xor by key" in {
      val keyHex = "4642"
      val xored = xorByKey(keyHex, foobar._2)
      xored must_== "002D2962042334"
    }
  }

  "Pairs Xor" should {
    import PairsXor._

    "Find all pairs indexes" in {
      val rowNum = 3
      allPossiblePairs(rowNum) must_== Set((0,1),(0,2),(1,2))
    }
  }

  "Pair" should {
    "Do correct Or guess" in {
      val pair = Pair(LetterValue(0, 43), LetterValue(1,73))
      pair.guess must_== Or(32, 66)
    }

    "Do correct Any guess" in {
      val pair = Pair(LetterValue(0, 79), LetterValue(1,73))
      pair.guess must_== AnyG
    }

    "Do correct Same guess" in {
      val pair = Pair(LetterValue(0, 43), LetterValue(1,43))
      pair.guess must_== Same
    }

    "Return correct join flag" in {
      val pairA = Pair(LetterValue(0,226),LetterValue(1,132))
      val pairB = Pair(LetterValue(0,226),LetterValue(2,230))
      val pairC = Pair(LetterValue(1,132),LetterValue(2,230))
      val pairD = Pair(LetterValue(3,137),LetterValue(4,225))
      pairA.isJoinedTo(pairB) must beTrue
      pairA.isJoinedTo(pairC) must beTrue
      pairB.isJoinedTo(pairC) must beTrue
      pairA.isJoinedTo(pairD) must beFalse
      pairB.isJoinedTo(pairD) must beFalse
      pairC.isJoinedTo(pairD) must beFalse
    }

    "Return correct join values" in {
      val pairA = Pair(LetterValue(0,226),LetterValue(1,132))
      val pairB = Pair(LetterValue(0,226),LetterValue(2,230))
      val pairC = Pair(LetterValue(1,132),LetterValue(2,230))
      val pairD = Pair(LetterValue(3,137),LetterValue(4,225))
      pairA.joinGuess(pairD) must_== Nil
      pairA.joinGuess(pairB) must_== Nil
      pairA.joinGuess(pairC) must_== List(LetterValue(0,70), LetterValue(1,32), LetterValue(2,66))
    }
  }

  "Pair column should" should {
    "Correctly parse ciphers to column model" in {
      val ciphers = List(
        List(10,20,30),
        List(40,50,60),
        List(70,80,90)
      )
      val parsed = PairColumns(ciphers)
      parsed.size must_== 3
      parsed.map(_.index) must_== List(0,1,2)
      val pairs = parsed.map(_.pairs)
      pairs.forall(_.size == 3) must beTrue
      pairs.head must_== List(
        Pair(LetterValue(0,10),LetterValue(1,40)),
        Pair(LetterValue(0,10),LetterValue(2,70)),
        Pair(LetterValue(1,40),LetterValue(2,70))
      )
    }

    "Correctly guess letters" in {
      import StringHex._
      val letters = "F B"
      val hex = textToHex(letters)
      val key = "A4"
      val encryptLetters = hexToAscii(xorByKey(key, hex))
      val pairColumn = PairColumn(0, List(
        Pair(LetterValue(0,226),LetterValue(1,132)),
        Pair(LetterValue(0,226),LetterValue(2,230)),
        Pair(LetterValue(1,132),LetterValue(2,230))
      ))
      pairColumn.guessedLetters must_== Set(LetterValue(0,70), LetterValue(1,32), LetterValue(2,66))
    }
  }
}
