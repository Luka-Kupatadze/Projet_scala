package main.scala

import scala.util.Random

object fonctions {

  // Fonctions utilitaires
  
  def initialiseMatrixInt(x : Int , y : Int) : Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](y,x)
    for (i <- 0 until x) {
      for (j <- 0 until y) {
        matrix(j)(i) = 0
      }
    }
    matrix
  }

  def get_dimension(m: Array[Array[Case]]): (Int, Int) = {
    (m(0).length, m.length)
  }

  def is_inside[T](m: Array[Array[T]], i: Int, j: Int): Boolean = {
    if (j >= 0 && j < m.length && i >= 0 && i < m(0).length) true
    else false
  }


  def get_neighbors[T](m: Array[Array[T]], i: Int, j: Int): List[(Int, Int)] = {
    var neighbors = List[(Int, Int)]()
    for (x <- -1 to 1) {
      for (y <- -1 to 1) {
        if (is_inside(m, i + x, j + y) && (!(x == 0 && y == 0))) {
          neighbors = neighbors :+ (i + x, j + y)
        }
      }
    }
    neighbors
  }
  

  def random_coords[T](m: Array[Array[T]]): (Int, Int) = {
    val i = scala.util.Random.nextInt(m(0).length)
    val j = scala.util.Random.nextInt(m.length)
    (i, j)
  }

  def incr_tab(m : Array[Array[Int]], i : Int, j : Int) : Unit = {
    if (m(j)(i) != -1) {
      m(j)(i) += 1
    }
  }


  def random_mines(matrix : Array[Array[Int]], n: Int) : Unit = {
    var i = 0
    while ( i < n) {
      val coords = random_coords(matrix)
      if (matrix(coords._2)(coords._1) != -1) {
        matrix(coords._2)(coords._1) = -1
        get_neighbors(matrix, coords._1, coords._2).foreach(c => incr_tab(matrix, c._1, c._2))
        i += 1
      }
    }
  }

  def init_case(k : Int) : Case = {
    if (k == -1) {
      new Mine()
    } else {
      new Empty(k)
    }
  }

  def init_game(w : Int, h : Int , n : Int) : Array[Array[Case]] = {
      val matrixIntermediaire = initialiseMatrixInt(w,h)
      random_mines(matrixIntermediaire,n)
      val matrice = matrixIntermediaire.map(x => x.map( y => init_case(y)))
      matrice
  }




}
