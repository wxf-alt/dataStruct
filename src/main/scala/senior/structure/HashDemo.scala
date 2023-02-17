package senior.structure

import basics.structure.DoubleLinkedList


/**
 * @Auther: wxf
 * @Date: 2023/2/16 20:12:25
 * @Description: HashDemo  Hash表(散列表) 实现
 * @Version 1.0.0
 */
object HashDemo {
  def main(args: Array[String]): Unit = {
    val table: HashTable[Int] = new HashTable[Int]
    table.add(10)
    table.add(20)
    table.add(10)
    table.printAll()
  }
}

class HashTable[T] {
  val initSize: Int = 10
  val arr: Array[DoubleLinkedList[T]] = new Array[DoubleLinkedList[T]](initSize)

  // 添加元素
  def add(e: T): Unit = {
    // 找到 e 应该去的那个链表所在数组中的位置索引
    val index: Int = e.hashCode().abs % initSize
    // 如果时第一次在这个位置添加元素，则应该添加链表
    if (null == arr(index)) arr(index) = new DoubleLinkedList[T]()
    arr(index).add(e)
  }

  def printAll(): Unit = {
    for (elem <- 0 until arr.length) {
      val list: DoubleLinkedList[T] = arr(elem)
      print(s"$elem：")
      if (null != list) {
        list.printAll
      }
      println()
    }
  }

}
