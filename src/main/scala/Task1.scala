import scala.annotation.tailrec

//[6,7, 1,2,3,9] -> "6-7,1-3,9"

object Task1 {
  
  def listToStringSeq(list:List[Int]):String =
    seqTupleList(list)
      .foldLeft(List.empty[String])((acc, elem) =>
        if (elem.head == elem.last)
          s"${elem.head}" :: acc
        else
          s"${elem.head}-${elem.last}" :: acc
      )
      .mkString(",")  

  @tailrec
  def seqTupleList(list: List[Int], acc: List[Range] = List.empty): List[Range] =
    list match
      case head :: tail =>
        if (acc.nonEmpty) {
          val last = acc.head.last
          if (head - last == 1) {
            seqTupleList(tail, (acc.head.head to (last + 1)) :: acc.tail)
          } else {
            seqTupleList(tail, (head to head) :: acc)
          }
        } else
          seqTupleList(tail, (head to head) :: acc)
      case Nil => acc
}
