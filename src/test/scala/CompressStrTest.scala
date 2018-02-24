import org.scalatest.{Matchers, FlatSpec}

class CompressStrTest extends FlatSpec with Matchers {

  import CompressStr._

  "compressStr" should "produce the right results" in {
    compressStr("QQQFAAABEEEDFFC") shouldBe "3QF3AB3ED2FC"
    compressStr("ABBCDD", 2) shouldBe "A2BC2D"
    compressStr("ABBBCCDDD", 3) shouldBe "A3BCC3D"
    compressStr("ABBBCCDDD", 4) shouldBe "ABBBCCDDD"
    compressStr("") shouldBe ""
  }

}
