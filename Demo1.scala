package Demo

import scala.collection.mutable.ListBuffer
import scala.io.{Source, StdIn}


//Le but de l'objet Demo1 est de lire le fichier de test et d'exécuter le programme principal.
object Demo1 {

  def main(args: Array[String]): Unit = {
    // lire le fichier de test
    val source = Source.fromFile("src/main/resources/test/test.txt", "utf-8")
    // lire en lignes
    val lines: Iterator[String] = source.getLines()

    val list: List[String] = lines.toList

    val game = new Game(list)
    game.start()

    source.close()
    //La lecture termine et le tube IO est fermé.
  }

}


//La classe Game spécifie la logique globale du fonctionnement du jeu.
class Game(list: List[String]) {
  // objet carte
  private var gm: GameMap = _
  private var isContinue = true // Déterminer si le jeu doit continuer.
  private var cnt = 0 // Noter le nombre de tondeuses
  private val resultList = ListBuffer[String]()

  def start() = {
    //appeler la fonction initMap et initialiser l'objet carte.
    this.initMap()
    var i = 0

    while (isContinue) {
      cnt += 1
      var inputFlag = true // Vérifier si le format d'entrée est correct.
      var arr: Array[String] = null
      while (inputFlag) {
        // Diviser la ligne de commande par des espaces et placer-les dans un tableau.
        arr = list(i).split("\\s+")

        // Déterminer s'il s'agit de trois lettres.
        if (arr.length == 3) {
          inputFlag = false
        }
        i += 1
      }

      // Instancier une tondeuse
      val mower = new Mower(gm, arr(0).toInt, arr(1).toInt, arr(2))

      //Recevoir des instructions
      val options: Array[Char] = list(i).toCharArray
      //Exécuter l'instruction
      this.computeOptions(mower, options)

      //Enregistrer la position actuelle de la tondeuse.
      resultList.append(s"Tondeuse ${cnt}: ${mower.x} ${mower.y} ${mower.to}")

      //Déménagement tondeuse terminé.
      if (i == list.length - 1) {
        isContinue = false
        this.printResult()
      }


    }  //while is done.

  }   // start is done.

  // afficher le résultat final.
  def printResult() = {
    resultList.foreach(println)
  }

  // La tondeuse exécute les instructions
  def computeOptions(mower: Mower, options: Array[Char]) = {
    options.foreach(op => mower.setOption(op.toString))

  }

  def initMap() = {
    // Initialiser l'objet cartographique，l'abscisse et l'ordonnée de la tondeuse ne doivent pas dépasser 9
    gm = new GameMap(10, 10)
  }

}

// classe de carte
class GameMap(Xlen: Int, Ylen: Int) {

  val map = Array.ofDim[Int](Xlen, Ylen)

}


//classe de Mower. Il est responsable du mouvement et de la direction de la tondeuse.
class Mower(gameMap: GameMap, var x: Int, var y: Int, var to: String) {

  private val point: Array[String] = Array("W", "N", "E", "S")

  def setOption(op: String): Unit = op match {
    case "D" => this.changeTo("D")
    case "A" => this.move()
    case "G" => this.changeTo("G")
  }

  // Mouvement
  def move(): Unit = {

    to match {
      case "W" => {
        // déplacer vers l'ouest x-1
        if (x - 1 >= 0) {
          x -= 1
        }

      }
      case "N" => {
        // déplacer vers le nord y+1
        if (y + 1 < gameMap.map.length) {
          y += 1
        }
      }
      case "E" => {
        // déplacer vers l'est x+1
        if (x + 1 < gameMap.map(x).length) {
          x += 1
        }
      }
      case "S" => {
        // déplacer vers le sud y-1
        if (y - 1 != 0) {
          y -= 1
        }
      }
    }


  }

  // Changer la direction de la tondeuse
  def changeTo(op: String): Unit = {

    if (op.eq("D")) {
      // Obtenir l'index de la position actuelle
      val i = point.indexOf(to)
      // l'index de la position+1
      to = point(((i + 1) % point.length))
    } else {
      var i = point.indexOf(to)
      if (i - 1 < 0) {
        i = point.length - 1
      } else {
        i -= 1
      }
      to = point(i)
    }

  }


}
