object CompressStr {

  def repeatChar(char: Char, length: Int) = {
    val sb = StringBuilder.newBuilder
    (1 to length).foreach(_ => sb += char)
    sb.toString()
  }

  // An implementation tat relies on functional data structures
  def compressStr(in: String, minOccurrency: Int = 2): String =
    if (in.isEmpty) in
    else {
      val sb = StringBuilder.newBuilder

      def appendChar(char: Char, occurrences: Int): Unit =
        if (occurrences < minOccurrency) sb.append(repeatChar(char, occurrences))
        else sb.append(s"$occurrences$char")

      val (lastChar, occurrences) = in.tail.foldLeft((in.head, 1)){
        case ((prevChar, occurs), char) =>
          if (prevChar == char) (char, occurs + 1)
          else {
            appendChar(prevChar, occurs)
            (char, 1)
          }
      }

      appendChar(lastChar, occurrences)
      sb.toString()
    }

  // A more java-like implementation. The reason I wrote it is that string compression should likely be fast
  // and I thought that a simple while and local vars would work faster.
  // Some basic perf tests (see performance.sc) I did did not confirm that though.
  def compressStr2(in: String, minOccurrency: Int = 2): String =
    if (in.isEmpty) in
    else {
      val sb = StringBuilder.newBuilder

      def appendChar(char: Char, occurrences: Int): Unit =
        if (occurrences < minOccurrency) sb.append(repeatChar(char, occurrences))
        else sb.append(s"$occurrences$char")

      val inArray = in.toCharArray
      var prevChar = inArray(0)
      var occurrences = 1
      var idx = 1
      val size = in.length

      while (idx < size) {
        val char = inArray(idx)
        idx += 1

        if (char == prevChar) occurrences += 1
        else {
          appendChar(prevChar, occurrences)
          prevChar = char
          occurrences = 1
        }
      }

      appendChar(prevChar, occurrences)
      sb.toString()
    }
}
