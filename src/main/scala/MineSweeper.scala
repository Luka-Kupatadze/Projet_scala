package main.scala

import scala.io.StdIn

class MineSweeper(i : Int , j : Int , n : Int) {

  var jeu = new Jeu()
  var matrice = jeu.init_game(i,j,n)


  def jouer() : Unit = {
    display()
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
        if (colonne.decouvert) print("| " + colonne.value + " |")
        else print("|   |")
      })
      println()
    })
  }

  def interact(i : Int , j : Int): Unit ={
    if (matrice(i)(j).value == -1) {
      println("Vous avez perdu !")
      System.exit(0)
    }
    else {
      matrice(i)(j).decouvert = true
      if (matrice(i)(j).value == 0) {
        for (i <- -1 to 1; j <- -1 to 1){
          if (i != 0 || j != 0) {
            
            }
          }
        }
      }
      
    }
    jouer()
  }

