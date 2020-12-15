package nl.kevinvandervlist.aoc2015.day08

object MatchSticks {
  def one(in: List[String]): Int =
    in.map(_.length).sum - in.map(decode).map(_.length).sum

  def two(in: List[String]): Int =
    in.map(encode).map(_.length).sum - in.map(_.length).sum

  private def decode(raw: String): String = {
    val it = raw
      .slice(1, raw.length - 1)
      .iterator

    val mem = new StringBuilder

    var c: Char = '*'

    while (it.hasNext) {
      c = it.next()
      // Escape char, skip it
      if (c == '\\') {
        c = it.next()
        if (c == '"' || c == '\\') {
          mem.append(c)
        } else if (c == 'x' || c == 'X') {
          mem.append(Integer
            .parseInt(it.next().toString + it.next().toString, 16)
            .toChar
          )
        }
      } else {
        mem.append(c)
      }
    }
    mem.toString()
  }

  private def encode(raw: String): String = {
    val it = raw.iterator

    val mem = new StringBuilder
    mem.append('"')

    var c: Char = '_'

    while (it.hasNext) {
      c = it.next()
      if (c == '\\') {
        mem.append(c)
      } else if (c == '"') {
        mem.append('\\')
      }
      mem.append(c)
    }
    mem.append('"')
    mem.toString()
  }
}
