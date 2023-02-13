package basics

/**
 * @Auther: wxf
 * @Date: 2023/2/13 19:50:26
 * @Description: DoubleLinkedListDemo  双向链表
 * @Version 1.0.0
 */
object DoubleLinkedListDemo {
  def main(args: Array[String]): Unit = {
    val list: DoubleLinkedList[Int] = new DoubleLinkedList[Int]
    list.add(10)
    list.add(20)
    list.add(30)
    list.add(40)
    list.add(50)
    println(list.delete(10))
    list.printAll()
  }
}

class DoubleLinkedList[T] {

  case class Node(value: T, var pre: Node, var next: Node) {
    override def toString: String = value + ""
  }

  var head: Node = _
  var tail: Node = _

  // 添加元素
  def add(ele: T): Unit = {
    val newNode: Node = Node(ele, null, null)
    if (null == head) {
      head = newNode
      tail = newNode
    } else {
      // 1.tail的next指向新节点
      tail.next = newNode
      // 2.新节点的pre指向tail
      newNode.pre = tail
      // 3.让tail指向新节点
      tail = newNode
    }
  }

  // 删除指定元素
  def delete(ele: T): Boolean = {
    // 1.找到要删除的元素
    val targetNode: Node = find(ele)
    if (null == targetNode) {
      false
    }
    else {
      // 删除元素的上一个节点
      val pre: Node = targetNode.pre
      // 删除元素的下一个节点
      val next: Node = targetNode.next

      if (head == tail) { // 只有一个节点
        head = null
        tail = null
      }
      else if (targetNode.eq(head)) { // 如果删除的是head节点
        next.pre = null
        head = next
      } else if (targetNode.eq(next)) { // 如果删除的是tail
        pre.next = null
        tail = pre
      } else {
        pre.next = next
        next.pre = pre
      }
      true
    }

  }

  def find(ele: T): Node = {
    var temp: Node = head
    while (null != temp) {
      if (temp.value == ele) {
        return temp
      }
      temp = temp.next
    }
    null
  }

  // 打印元素
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
