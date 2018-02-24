import CompressStr._

def timed[A](block: => A) = {
  val start = System.nanoTime()
  val res = block
  val elapsedNano = System.nanoTime() - start
  (elapsedNano, res)
}

def genStr(length: Int) = {
  val sb = StringBuilder.newBuilder
  val r1 = new scala.util.Random(31)
  val r2 = new scala.util.Random(55)
  var charsLeft = length

  r1.alphanumeric.takeWhile { char =>
    if (charsLeft < 1) false
    else {
      val l = r2.nextInt(6) + 1
      val occurs = if (l > charsLeft) charsLeft else l
      sb.append(repeatChar(char, occurs))
      charsLeft -= occurs
      true
    }
  }.force

  sb.toString()
}

val testLength = 3000
val input = genStr(testLength)

(1 to 100).map { _ =>
  timed(compressStr2(input, 2))._1
}.sorted.sum / 100

(1 to 100).map { _ =>
  timed(compressStr(input, 2))._1
}.sorted.sum / 100

(1 to 100).map { _ =>
  timed(compressStr2(input, 2))._1
}.sorted.sum / 100

(1 to 100).map { _ =>
  timed(compressStr(input, 2))._1
}.sorted.sum / 100

// On my mac book (12'') the output looks like this:
// res0: Long = 3454933
// res1: Long = 408983
// res2: Long = 2900879
// res3: Long = 232742
// which means that the first (functional) implementation is substantially faster.