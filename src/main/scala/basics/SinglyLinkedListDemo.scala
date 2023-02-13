package basics

/**
 * @Auther: wxf
 * @Date: 2023/2/13 19:19:05
 * @Description: SinglyLinkedListDemo  单向链表 实现
 * @Version 1.0.0
 */
object SinglyLinkedListDemo {
  def main(args: Array[String]): Unit = {
    val list: SinglyLinkedList[Int] = new SinglyLinkedList[Int]
    list.add(1)
    list.add(10)
    list.add(100)
    list.add(1000)

    println(list.contain(200))
  }
}

class SinglyLinkedList[T] {

  case class Node(value: T, var next: Node)

  // 记录链表的头节点（必须）
  var head: Node = _
  // 记录尾节点（为了方便添加元素，可选）
  var tail: Node = _


  // 添加元素
  def add(ele: T): Unit = {
    // 1.将需要添加的值，封装到一个Node中
    if (head == null) {
      head = Node(ele, null)
      tail = head
    } else {
      // 2.让tail的next指针，指向新节点
      tail.next = Node(ele, null)
      // 3.再让tail只想最后一个元素
      tail = tail.next
    }
  }

  // 判断链表中元素是否存在
  def contain(ele: T): Boolean = {
    if (null == head) return false
    var temp: Node = head
    do {
      if (temp.value == ele) return true
      temp = temp.next
    } while (temp != null)
    false
  }


  def printAll(): Unit = {
    if (head != null) {
      var temp: Node = head
      do {
        println(temp.value)
        temp = temp.next
      } while (temp != null)
    }
  }

}












