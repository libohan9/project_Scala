package Demo

import scala.collection.mutable.ListBuffer
import scala.io.{Source, StdIn}

object Demo1 {

  def main(args: Array[String]): Unit = {
    // 读取文件
    val source = Source.fromFile("src/main/resources/test/test.txt", "utf-8")
    // 以行位单位读取
    val lines: Iterator[String] = source.getLines()

    val list: List[String] = lines.toList

    val game = new Game(list)
    game.start()

    source.close()
  }

}

class Game(list: List[String]) {
  // 地图对象
  private var gm: GameMap = _
  private var isContinue = true // 判断游戏是否需要继续
  private var cnt = 0 // 记录割草机的数量
  private val resultList = ListBuffer[String]()


  def start() = {
    // 初始化地图
    this.initMap()
    var i = 0

    while (isContinue) {
      cnt += 1
      var inputFlag = true // 判断输入的格式是否正确
      var arr: Array[String] = null
      while (inputFlag) {
        // 获取第一条 因为第一条是指定方位
        arr = list(i).split("\\s+")
        // 判断是否是三个参数
        if (arr.length == 3) {
          inputFlag = false
        }
        i += 1
      }

      // 实例化一台割草机
      val mower = new Mower(gm, arr(0).toInt, arr(1).toInt, arr(2))


      // 接收指令
      val options: Array[Char] = list(i).toCharArray
      // 执行指令
      this.computeOptions(mower, options)

      // 将当前位置保存
      resultList.append(s"割草机 ${cnt}: ${mower.x} ${mower.y} ${mower.to}")

      // 割草机运行完毕
      if (i == list.length - 1) {
        isContinue = false
        this.printResult()
      }


    }

  }

  // 打印最终结果
  def printResult() = {
    resultList.foreach(println)
  }

  // 割草机执行指令
  def computeOptions(mower: Mower, options: Array[Char]) = {
    options.foreach(op => mower.setOption(op.toString))


  }

  def initMap() = {
    // 初始化地图
    gm = new GameMap(10, 10)
  }

}

// 地图
class GameMap(Xlen: Int, Ylen: Int) {

  val map = Array.ofDim[Int](Xlen, Ylen)

}

class Mower(gameMap: GameMap, var x: Int, var y: Int, var to: String) {

  private val point: Array[String] = Array("W", "N", "E", "S")

  def setOption(op: String): Unit = op match {
    case "D" => this.changeTo("D")
    case "A" => this.move()
    case "G" => this.changeTo("G")
  }

  // 移动
  def move(): Unit = {

    to match {
      case "W" => {
        // 西移动 x-1
        if (x - 1 >= 0) {
          x -= 1
        }

      }
      case "N" => {
        // 北移动 y+1
        if (y + 1 < gameMap.map.length) {
          y += 1
        }
      }
      case "E" => {
        // 东移动 x+1
        if (x + 1 < gameMap.map(x).length) {
          x += 1
        }
      }
      case "S" => {
        // 南移动 y-1
        if (y - 1 != 0) {
          y -= 1
        }
      }
    }


  }

  // 改变方向
  def changeTo(op: String): Unit = {

    if (op.eq("D")) {
      // 得到当前面向位置的下标
      val i = point.indexOf(to)
      // 下标+1
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