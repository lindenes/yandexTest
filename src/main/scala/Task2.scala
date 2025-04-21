import scala.reflect.ClassTag

//поиск выхода
//0 0 0 0 0 0
//0 0 1 0 1 0
//0 1 A 1 0 0
//0 0 1 B 0 0
//0 0 0 0 0 0
//A - нет
//B - да
object Task2 {
  case class Matrix(rows:Array[Array[Int]], startedX:Int = 0, startedY:Int= 0) {

    var (sizeX, sizeY) = rows.length -> rows(0).length
    var currentXPosition = startedX
    var currentYPosition = startedY

    val stones = {
      val array = new Array[Array[Int]](sizeX)
      for(i <- 0 until sizeX){
        array(i) = new Array[Int](sizeY)
      }
      array
    }

    def get(x:Int, y:Int) = rows(x)(y)

    def get = rows(currentXPosition)(currentYPosition)

    def goLeft = {
      currentYPosition = currentYPosition - 1
      get(currentXPosition, currentYPosition)
    }

    def goRight = {
      currentYPosition = currentYPosition + 1
      get(currentXPosition, currentYPosition)
    }

    def goTop = {
      currentXPosition = currentXPosition - 1
      get(currentXPosition, currentYPosition)
    }

    def goBottom = {
      currentXPosition = currentXPosition + 1
      get(currentXPosition, currentYPosition)
    }

  }

  def run(rows:Array[Array[Int]]) = {
    val matrix = Matrix.apply(rows, 3, 3)
    var go = true
    val exitPos = List(
      0 -> Range(0, matrix.sizeY).toList,
      0 -> Range(0, matrix.sizeX).toList,
      matrix.sizeY -> Range(0, matrix.sizeY).toList,
      matrix.sizeX -> Range(0, matrix.sizeX).toList,
    )

    while (go){
      val topField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goTop
      val leftField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goLeft
      val rightField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goRight
      val bottomField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goBottom

      if (topField == 0)
        matrix.goTop
      else if (leftField == 0)
        matrix.goLeft
      else if (rightField == 0)
        matrix.goRight
      else if (bottomField == 0)
        matrix.goBottom
      else{
        println("Exit not exist")
        go = false
      }

      if (exitPos.exists {
        case (index, maybeIndexes) =>
          index == matrix.currentXPosition && maybeIndexes.contains(matrix.currentYPosition) ||
            index == matrix.currentYPosition && maybeIndexes.contains(matrix.currentXPosition)
      } && matrix.get != 1) {
        go = false
        println("found exit")
      }
      println(matrix.currentXPosition + " : " + matrix.currentYPosition + " -> " + matrix.get)
    }
  }
}
