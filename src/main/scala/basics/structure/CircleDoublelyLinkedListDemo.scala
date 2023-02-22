package basics.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/13 20:21:48
 * @Description: CircleDoublelyLinkedListDemo  双向循环链表
 * @Version 1.0.0
 */
object CircleDoublelyLinkedListDemo {
  def main(args: Array[String]): Unit = {
    val list: CircleDoublelyLinkedList[Int] = new CircleDoublelyLinkedList[Int]
    list.add(10)
    list.add(20)
    list.add(30)
    list.add(40)
    list.printAll()
    println("================")
    println(list.delete(10))
    list.printAll()
    println("================")
    list.add(50)
    list.printAll()
  }
}

class CircleDoublelyLinkedList[T] extends DoubleLinkedList[T] {

  override def add(ele: T): Unit = {
    super.add(ele)
    // 让head和tail形成一个环
    head.pre = tail
    tail.next = head
  }

  override def delete(ele: T): Boolean = {
    if (super.delete(ele)) {
      // 重新构建一个环
      if (null != head) head.pre = tail
      if (null != tail) tail.next = head
      true
    } else {
      false
    }
  }

  override def find(ele: T): Node = {
    var temp: Node = head
    while (null != temp) {
      if (temp.value == ele) {
        return temp
      }
      temp = temp.next
      if (temp == head) { // 如果查找过程中重新回到头部，退出
        return null
      }
    }
    null
  }

  override def printAll(): Unit = {
    if (head != null) {
      var temp: Node = head
      do {
        println(temp.value)
        temp = temp.next
      } while (temp != null && temp != head)
    }
  }
}