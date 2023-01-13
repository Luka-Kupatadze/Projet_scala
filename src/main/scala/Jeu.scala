package main.scala

import scala.util.Random

class Jeu() {

  // Fonctions utilitaires
  
  def initialiseMatrix(x : Int , y : Int) : Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](x,y)
    for (i <- 0 until x) {
      for (j <- 0 until y) {
        matrix(i)(j) = 0
      }
    }
    matrix
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

  def incr_tab(m : Array[Array[Int]], i : Int, j : Int) : Unit = {
    if (m(i)(j) != -1) {
      m(i)(j) += 1
    }
  }


  def random_mines(matrix : Array[Array[Int]], n: Int) : Unit = {
    var i = 0
    while ( i < n) {
      val coords = random_coords(matrix)
      if (matrix(coords._1)(coords._2) != -1) {
        matrix(coords._1)(coords._2) = -1
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
      //initialise une matrice de dimension (w,h)
      //avec n mines
      val matrixIntermediaire = initialiseMatrix(w,h)
      random_mines(matrixIntermediaire,n)
      val matrice = matrixIntermediaire.map(x => x.map( y => init_case(y)))
    matrice
  }




}
