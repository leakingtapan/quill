package io.getquill.sources

object ExpandBindings {

  def apply(query: String, indexes: List[Int]) = {
    def gaps(i: List[Int] = 0 +: indexes): Map[Int, Int] =
      i match {
        case a :: b :: tail if (b != a + 1) =>
          gaps(b +: tail) + (a -> (b - a))
        case a :: tail =>
          gaps(tail)
        case Nil =>
          Map.empty
      }
    gaps() match {
      case List() => query
      case gaps =>
        def fill(idx: Int, chars: List[Char]): List[Char] =
          (chars, gaps.get(idx)) match {
            case ('?' :: tail, Some(gap)) =>
              val expanded = List.fill(gap)('?').mkString(", ").toList
              expanded ++ fill(idx + gap, tail)
            case ('?' :: tail, None) =>
              '?' +: fill(idx + 1, tail)
            case (head :: tail, _) =>
              head +: fill(idx, tail)
            case (Nil, _) =>
              Nil
          }
        fill(0, query.toList).mkString
    }
  }
}
