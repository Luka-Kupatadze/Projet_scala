import scala.util.Random

class Jeu(val x : Int, val y : Int, nbr_mines : Int) {

  val matrix = Array.ofDim[Int](x, y)

  def incr_tab(m : Array[Array[Int]], i : Int, j : Int) : Unit = {
    if (m(i)(j) != -1) {
      m(i)(j) += 1
    }
  }


  def random_mines() : Unit = {
    var i = 0
    while (i < nbr_mines) {
      val coords = random_coords(matrix)
      if (matrix(coords._1)(coords._2) != -1) {
        matrix(x)(y) = -1
        get_neighbors(matrix,x,y).map( c => incr_tab(matrix, c._1, c._2))
        i += 1
      }
    }
  }

  def init_case(k : Int) : Case = {
    if (k == -1) {
      new Mine()
    } else {
      new Case(false, k)
    }
  }





























  def get_dimension(m: Array[Array[Int]]): (Int, Int) = {
    (m.length, m(0).length)
  }

  def is_inside(m: Array[Array[Int]], i: Int, j: Int): Boolean = {
    if (i >= 0 && i < m.length && j >= 0 && j < m(0).length) true
    else false
  }


  def get_neighbors(m: Array[Array[Int]], i: Int, j: Int): List[(Int, Int)] = {
    var neighbors = List[(Int, Int)]()
    for (x <- -1 to 1) {
      for (y <- -1 to 1) {
        if (is_inside(m, i + x, j + y) && (x != 0 || y != 0)) {
          neighbors = neighbors :+ (i + x, j + y)
        }
      }
    }
    neighbors
  }

  def random_coords(m: Array[Array[Int]]): (Int, Int) = {
    val i = scala.util.Random.nextInt(m.length)
    val j = scala.util.Random.nextInt(m(0).length)
    (i, j)
  }



}
