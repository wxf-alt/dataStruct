package basics

import scala.collection.mutable.ArrayBuffer

/**
 * @Auther: wxf
 * @Date: 2023/2/10 16:33:09
 * @Description: SparseArrayDemo  稀疏数组 实现
 * @Version 1.0.0
 */
object SparseArrayDemo {
  // 五子棋盘大小
  val rowsNum: Int = 15
  val colsNum: Int = 15

  def main(args: Array[String]): Unit = {
    // 1. 初始化五子棋盘
    val chess: Array[Array[Int]] = initChessBoard()
    // 2. 五子棋盘转成稀疏数组
    val sparseArr: SparseArray = toSparseArray(chess)
    println(sparseArr)
    // 3. 稀疏数组转成五子棋盘
    val arr2: Array[Array[Int]] = sparse2Chess(sparseArr)
    printChess(arr2)

  }

  // 4. 打印棋盘
  def printChess(chess: Array[Array[Int]]): Unit = {
    for (row <- chess.indices; col <- chess(row).indices) {
      print(chess(row)(col) + " ")
      if (col == chess(row).length - 1) println()
    }
  }

  // 3. 把稀疏数组转换成二维棋盘
  def sparse2Chess(sparseArr: SparseArray): Array[Array[Int]] = {
    // 五子棋盘
    val arr: Array[Array[Int]] = Array.ofDim[Int](rowsNum, colsNum)
    sparseArr.buf.foreach(node => {
      arr(node.row)(node.col) = node.value
    })
    arr
  }

  // 2. 五子棋棋盘转成稀疏数组
  def toSparseArray(chess: Array[Array[Int]]): SparseArray = {
    val sparseArr: SparseArray = new SparseArray
    // 把 值不是 0 的存入到稀疏数组中
    for (row <- chess.indices; col <- chess(row).indices if chess(row)(col) != 0) {
      sparseArr.add(row, col, chess(row)(col))
    }
    sparseArr
  }

  // 1. 初始化五子棋棋盘
  def initChessBoard(): Array[Array[Int]] = {

    // 棋盘的每个位置自动会置为 0
    val arr: Array[Array[Int]] = Array.ofDim[Int](rowsNum, colsNum)

    // 放置几个棋子: 白=1 黑=2
    val white: Int = 1
    val black: Int = 2
    arr(1)(2) = white
    arr(2)(3) = black
    arr
  }

}

// 定义稀疏数组
class SparseArray {
  // 使用可变数组来存储 Node (行列值信息)
  val buf: ArrayBuffer[Node] = ArrayBuffer[Node]()

  // 判断稀疏数组是否为 Empty
  def isEmpty: Boolean = buf.isEmpty

  // 向稀疏数组中添加数据
  def add(row: Int, col: Int, value: Int): ArrayBuffer[Node] = {
    buf += Node(row, col, value)
  }

  override def toString: String = buf.toString

}

// 用来存储二维数组中非默认值的具体: 行, 列, 值 信息
case class Node(row: Int, col: Int, value: Int)