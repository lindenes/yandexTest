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
    var (lastX, lastY) = 0 -> 0
    val exitPos = {
      val lines = Range(0, sizeX).toList
      val linesWithValue = lines.map(_ -> List(sizeY - 1))
      val firstLine = lines.head -> Range(0, sizeY).toList
      val lastLine = lines.last -> Range(0, sizeY).toList
      val changedHead = firstLine :: linesWithValue.tail
      val changedTail = lastLine :: changedHead.reverse.tail
      changedTail
    }

    def get(x:Int, y:Int) = rows(x)(y)

    def get = rows(currentXPosition)(currentYPosition)

    def goLeft = {
      lastX = currentXPosition
      lastY = currentYPosition
      currentYPosition = currentYPosition - 1
      this
    }

    def goRight = {
      lastX = currentXPosition
      lastY = currentYPosition
      currentYPosition = currentYPosition + 1
      this
    }

    def goTop = {
      lastX = currentXPosition
      lastY = currentYPosition
      currentXPosition = currentXPosition - 1
      this
    }

    def goBottom = {
      lastX = currentXPosition
      lastY = currentYPosition
      currentXPosition = currentXPosition + 1
      this
    }

    def onExit =
      exitPos.exists {
        case (index, maybeIndexes) =>
          index == currentXPosition && maybeIndexes.contains(currentYPosition) ||
            index == currentYPosition && maybeIndexes.contains(currentXPosition)
      } && get != 1
  }

  def run(rows:Array[Array[Int]]) = {
    val matrix = Matrix.apply(rows, 3, 3)
    var go = true

    while (go){
      val topField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goTop
      val leftField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goLeft
      val rightField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goRight
      val bottomField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goBottom

      if (topField.get == 0) {
        var goTop = true
        while(goTop){
          val topField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goTop
          if(topField.get == 0 )
            matrix.goTop
          else
            goTop = false

          println(matrix.currentXPosition + " : " + matrix.currentYPosition + " -> " + matrix.get)
          if (matrix.onExit) {
            goTop = false
            go = false
            println("found exit")
          }
        }
      }
      else if (leftField.get == 0)
        var goLeft = true
        while(goLeft){
          val leftField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goLeft
          if(leftField.get == 0)
            matrix.goLeft
          else goLeft = false
          println(matrix.currentXPosition + " : " + matrix.currentYPosition + " -> " + matrix.get)
          if (matrix.onExit) {
            goLeft = false
            go = false
            println("found exit")
          }
        }
      else if (rightField.get == 0){
        var goRight = true
        while(goRight){
          val rightField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goRight
          if(rightField.get == 0)
            matrix.goRight
          else goRight = false
          println(matrix.currentXPosition + " : " + matrix.currentYPosition + " -> " + matrix.get)
          if (matrix.onExit) {
            goRight = false
            go = false
            println("found exit")
          }
        }
      }
      else if (bottomField.get == 0)
        var goBottom = true
        while(goBottom){
          val bottomField = matrix.copy(matrix.rows, matrix.currentXPosition, matrix.currentYPosition).goBottom
          if(bottomField.get == 0)
            matrix.goBottom
          else goBottom = false
          println(matrix.currentXPosition + " : " + matrix.currentYPosition + " -> " + matrix.get)
          if (matrix.onExit) {
            goBottom = false
            go = false
            println("found exit")
          }
        }
      else{
        println("Exit not exist")
        go = false
      }

    }
  }
}
