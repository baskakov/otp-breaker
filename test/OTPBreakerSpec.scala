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
  }
}
