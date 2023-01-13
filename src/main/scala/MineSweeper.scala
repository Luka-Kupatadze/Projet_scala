package main.scala

import scala.io.StdIn

class MineSweeper(i : Int , j : Int , n : Int) {

  var matrice = new Jeu().init_game(i,j,n)


  def jouer() : Unit = {
    display()
    //lire les coordonnées et les transformer en (int,int)
    var coord = StdIn.readLine("Entrez les coordonnées de la case à dévoiler : ")
      .drop(1)
      .dropRight(1)
      .split(",")
      .map(_.toInt)
    interact(coord(0),coord(1))
  }

  def display() : Unit = {
    matrice.foreach(ligne => {
      ligne.foreach(colonne => {
        print(colonne.value)
      })
      println()
    })
  }

  def interact(i : Int , j : Int): Unit ={

  }

}
