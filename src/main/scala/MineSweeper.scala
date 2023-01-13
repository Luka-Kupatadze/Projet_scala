package main.scala

import scala.io.StdIn

class MineSweeper(i : Int , j : Int , n : Int) {

  var matrice = fonctions.init_game(i,j,n)
  var coord_x = 0
  var coord_y = 0
  var case_restante = i*j-n

  def jouer() : Unit = {
    while(case_restante > 0) {
      display()
      println("Choississez un entier x entre 1 et " + i + " :")
      coord_x = StdIn.readInt() - 1 // -1 car les indices commencent à 0
      println("Choississez un entier y entre 1 et " + j + " :")
      coord_y = StdIn.readInt() - 1 // -1 car les indices commencent à 0
      println("Vous avez choisi la case (" + coord_x + "," + coord_y + ")")
      if (fonctions.is_inside(matrice, coord_x, coord_y)) {
        interact(coord_x, coord_y)
      }
      else {
        println("Cette case n'existe pas !")
      }

    }
  }

  def display() : Unit = {
    matrice.foreach(ligne => {
      print("| ")
      ligne.foreach(colonne => {
        print( colonne.value + " |")
      })
      println()
    })
    println()
    matrice.foreach(ligne => {
      println("---------"*(ligne.length-1))
      print("|")
      ligne.foreach(colonne => {
        if (colonne.decouvert) print( "   " + colonne.value + "   |")
        else print("       |")
      })
      println()
    })
  }

  def interact(i : Int , j : Int): Unit ={
    if (matrice(j)(i).value == -1) {
      println("Vous avez perdu !")
      System.exit(0)
    }
    else {
      matrice(j)(i).decouvert = true
      if (matrice(j)(i).value == 0) {
        println(fonctions.get_neighbors(matrice,i,j))
        fonctions.get_neighbors(matrice,i,j).foreach((x,y) => {
          if (!matrice(y)(x).decouvert) interact(x,y)
        })
        }
      }
    }

  }

