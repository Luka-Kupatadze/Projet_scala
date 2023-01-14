package main.scala

import scala.io.StdIn
import Console._


class MineSweeper() {

  var matrice = fonctions.init_game(2, 2, 1)
  var case_restante: Int = 0


  def menu(): Unit = {
    println("Vous allez jouer au démineur, les règles sont classiques, à chaque tour de jeu, vous pourrez choisir entre deux actions possibles:\n" +
      "\t1. Dévoiler une case\n" +
      "\t2. Poser/enlever un drapeau sur une case\n" +
      "Une case avec un drapeau ne peut pas être dévoilé, vous gagnez la partie si vous dévoilez toutes les cases qui ne contiennent pas de mine, vous perdez si vous dévoilez une case qui contient une mine.\n" +
      "Bonne chance !")
    println("\n\nVeuillez maintenant choisir une difficulté:\n\t1.facile -> 10 lignes, 10 colonnes et 20 bombes\n\t2.moyen -> 12 lignes, 15 colonnes et 48 bombes\n\t3.difficile -> 15 lignes, 20 colonnes et 100 bombes\n\t4.personnalisé -> vous cchoisissez le nombre de cases et de bombes")
    var diff = StdIn.readInt()
    while (diff < 0 || diff > 4) {
      println("le nombre saisis n'est pas dans l'intervalle [1,4], veuillez choisir a nouveau")
      println("\n\nVeuillez maintenant choisir une difficulté:\n\t1.facile -> 10 lignes, 10 colonnes et 20 bombes\n\t2.moyen -> 12 lignes, 15 colonnes et 48 bombes\n\t3.difficile -> 15 lignes, 20 colonnes et 100 bombes\n\t4.personnalisé -> vous cchoisissez le nombre de cases et de bombes")
      diff = StdIn.readInt()
    }
    difculty(diff)

  }

  def difculty(i: Int): Unit = {
    i match {
      case 1 =>{
        matrice = fonctions.init_game(10, 10, 20)
        case_restante = 100 - 20
      }
      case 2 =>{
        matrice = fonctions.init_game(15, 12, 48)
        case_restante = 180 - 48
      }
      case 3 => {
        matrice = fonctions.init_game(20, 15, 100)
        case_restante = 300 - 100
      }
      case 4 => {
        println("Veuillez saisir le nombre de lignes")
        var nb_ligne = StdIn.readInt()
        println("Veuillez saisir le nombre de colonnes")
        var nb_colonne = StdIn.readInt()
        println("Veuillez saisir le nombre de bombes")
        var nb_bombes = StdIn.readInt()
        if (nb_bombes > nb_ligne * nb_colonne) {
          println("Le nombre de bombes est trop grand, veuillez recommencer")
          difculty(4)
        }
        else if (nb_bombes <=0){
          println("Impossible d'avoir 0 ou moins de bombes, veuillez recommencer")
          difculty(4)
        }
        else {
          matrice = fonctions.init_game( nb_colonne,nb_ligne, nb_bombes)
          case_restante = nb_ligne * nb_colonne - nb_bombes
        }
        matrice = fonctions.init_game( nb_colonne,nb_ligne, nb_bombes)
        case_restante = nb_ligne * nb_colonne - nb_bombes
      }
    }
    println("Vous avez choisi la difficulté " + i + ", bonne chance")


    println("Voici la matrice de jeu:")
    display()
    println("Vous avez " + case_restante + " cases à découvrir")
    println("Veuillez choisir une action:\n\t1.Découvrir une case\n\t2.Mettre un drapeau")
    var action = StdIn.readInt()
    while (action < 1 || action > 2) {
      println("le nombre saisis n'est pas dans l'intervalle [1,2], veuillez choisir a nouveau")
      println("Veuillez choisir une action:\n\t1.Découvrir une case\n\t2.Mettre un drapeau")
      action = StdIn.readInt()
    }
    action_choisie(action)
  }


  def action_choisie(i: Int): Unit = {
    i match {
      case 1 => {
        println("Veuillez saisir la colonne de la case à découvrir")
        var colonne = StdIn.readInt() - 1
        while (colonne < 0 || colonne >= matrice(0).length) {
          println("La valeur choisi n'est pas dans l'intervalle [1," + matrice(0).length + "], choisissez à nouveau")
          colonne = StdIn.readInt() - 1
        }
        println("Veuillez saisir la ligne de la case à découvrir")
        var ligne = StdIn.readInt() - 1
        while (ligne < 0 || ligne >= matrice.length) {
          println("La valeur choisi n'est pas dans l'intervalle [1," + matrice.length + "], choisissez à nouveau")
          ligne = StdIn.readInt() - 1
        }
        interact(colonne, matrice.length - 1 - ligne)
      }

      case 2 => {
        println("Veuillez saisir la colonne de la case à marquer")
        var colonne = StdIn.readInt() - 1
        while (colonne < 0 || colonne >= matrice(0).length) {
          println("La valeur choisi n'est pas dans l'intervalle [1," + matrice(0).length + "], choisissez à nouveau")
          colonne = StdIn.readInt() - 1
        }
        println("Veuillez saisir la ligne de la case à marquer")
        var ligne = StdIn.readInt() - 1
        while (ligne < 0 || ligne >= matrice.length) {
          println("La valeur choisi n'est pas dans l'intervalle [1," + matrice.length + "], choisissez à nouveau")
          ligne = StdIn.readInt() - 1
        }
        marquer(colonne,matrice.length - 1 -  ligne)

      }
    }
  }


  def jouer(): Unit = {
    menu()
    while (case_restante > 0) {
      println("Voici la matrice de jeu:")
      display()
      print("il vous reste " + case_restante + " cases à découvrir")
      println("Veuillez choisir une action:\n\t1.Découvrir une case\n\t2.Mettre un drapeau")
      var action = StdIn.readInt()
      while (action < 1 || action > 2) {
        println("le nombre saisis n'est pas dans l'intervalle [1,2], veuillez choisir a nouveau")
        println("Veuillez choisir une action:\n\t1.Découvrir une case\n\t2.Mettre un drapeau")
        action = StdIn.readInt()
      }
      action_choisie(action)
    }
    display()
    println(case_restante)
    println("Vous avez gagné !")
  }

  def display(): Unit = {

    //triche : permet de tricher si on le souhaite
    /*
    matrice.foreach(ligne => {
      print("| ")
      ligne.foreach(colonne => {
        print(colonne.value + " |")
      })
      println()
    })
    println()
    */

    //définission de l'espacement par defaut entre l'axe des abscisse et le jeu
    var espacementGauche = " " * matrice.length.toString.length + "|"
    //début des abscisses
    println(YELLOW+ "^" + RESET)
    var n = matrice.length
    matrice.foreach(ligne => {
      println(YELLOW + "|" + RESET + "  " + espacementGauche.dropRight(1) + ("---------" * (ligne.length)).dropRight(ligne.length - 1))
      print(YELLOW + "|"+" " + (n)+ RESET + espacementGauche.drop(n.toString.length - 1))
      n -= 1
      ligne.foreach(colonne => {
        if (colonne.decouvert) {
          colonne.value match {
            case 0 => print(WHITE_B + "       " + RESET + "|")
            case 1 => print("   " + RED + colonne.value + RESET + "   |")
            case 2 => print("   " + GREEN + colonne.value + RESET + "   |")
            case 3 => print("   " + YELLOW + colonne.value + RESET + "   |")
            case 4 => print("   " + BLUE + colonne.value + RESET + "   |")
            case 5 => print("   " + MAGENTA + colonne.value + RESET + "   |")
            case 6 => print("   " + CYAN + colonne.value + RESET + "   |")
            case 7 => print("   " + BLACK + colonne.value + RESET + "   |")
            case 8 => print("   " + BLACK + colonne.value + RESET + "   |")
            case _ => print("       |")
          }
        }
        else if (colonne.flag) {
          print(RED_B + "   " + WHITE +"X"+ "   "+ RESET+"|")
        }
        else {
          print("       |")
        }
      })
      println()
    })
    println((YELLOW + "|" + RESET+"  " + espacementGauche.dropRight(1) + "---------" * (matrice(0).length)).dropRight(matrice(0).length - 1))
    print(YELLOW + "|" + RESET+"  " + espacementGauche.dropRight(1) + " ")
    var espacementBas = "   "
    var m = 1
    while (m <= matrice(0).length) {
      print(espacementBas.drop(m.toString.length - 1) +YELLOW + m + RESET + "    ")
      m = m + 1
    }
    println("")
    println(YELLOW + ("---" + "-"*espacementGauche.length + "---------" * (matrice(0).length)).dropRight(matrice(0).length - 1)+">" + RESET)

  }

  def interact(i: Int, j: Int): Unit = {
    if (matrice(j)(i).value == -1) {
      println("Vous avez perdu !")
      System.exit(0)
    }
    else if (matrice(j)(i).flag){
      println("Vous ne pouvez pas découvir une case marquée")
    }
    else {
      if (matrice(j)(i).decouvert == false) {
        matrice(j)(i).decouvert = true
        case_restante -= 1
        if (matrice(j)(i).value == 0) {
          fonctions.get_neighbors(matrice, i, j).foreach((x, y) => {
            if (!matrice(y)(x).decouvert) interact(x, y)
          })
        }
      }
      else println("Vous avez deja découvert cette case")
    }

  }

  def marquer(i: Int, j: Int): Unit = {
    if (matrice(j)(i).flag == false) {
      matrice(j)(i).flag = true
      println("Vous avez mis un drapeau sur la case (" + (i + 1) + "," + (j + 1) + ")")
    }
    else {
      matrice(j)(i).flag = false
      println("Vous avez enlevé le drapeau sur la case (" + (i + 1) + "," + (j + 1) + ")")
    }

  }

}