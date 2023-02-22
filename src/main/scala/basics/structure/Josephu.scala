package basics.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/14 11:42:14
 * @Description: Josephu   使用循环链表求解约瑟夫(Josephu )问题
 * @Version 1.0.0
 */
object Josephu {

  def main(args: Array[String]): Unit = {
    val last: Int = startGame(6, 1, 3)
    println(last)
  }

//  def startGame(n: Int, start: Int, number: Int) = {
//    // 创建循环链表
//    val list: CircleDoublelyLinkedList[Int] = new CircleDoublelyLinkedList[Int]
//    // 给链表初始化 n 个人
//    for (elem <- 1 to n) {
//      list.add(elem)
//    }
//    // 从2开始数
//    var startNode: list.Node = list.find(start).pre
//    while (list.head != list.tail) {
//      for (i <- 1 to number) {
//        startNode = startNode.next
//      }
//      // 找到需要删除的人，就是startNode
//      list.delete(startNode.value)
//      startNode = startNode.pre
//    }
//    startNode.value
//  }

  def startGame(n: Int, start: Int, num: Int) = {
    val list: CircleDoublelyLinkedList[Int] = new CircleDoublelyLinkedList[Int]
    // 1. 给链表初始化n个人(围成一圈)
    for (i <- 1 to n) {
      list.add(i)
    }

    var startNode: list.Node = list.find(start).pre
    while (list.head != list.tail) {
      for (i <- 1 to num) {
        startNode = startNode.next
      }
      // 找到枪毙那个人, 就是startNode
      list.delete(startNode.value)
      startNode = startNode.pre
    }
    startNode.value
  }

}