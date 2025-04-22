object Main{
  def main(args:Array[String]): Unit = {

    val list = 6 :: 7 :: 1 :: 2 :: 3 :: 9 :: Nil
    println(Task1.listToStringSeq(list))

    val matrix: Array[Array[Int]] = Array(
      Array(0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 1, 1, 1, 0),
      Array(0, 1, 0, 0, 1, 0, 0),
      Array(0, 0, 1, 0, 1, 1, 0),
      Array(0, 0, 1, 1, 1, 0, 0),
      Array(0, 0, 1, 0, 1, 0, 0),
      Array(0, 0, 1, 0, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0),
    )
    Task2.run(matrix)
    
  }
}