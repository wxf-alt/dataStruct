package basics.structure

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

  // 删除元素
  def delete(ele: T): Boolean = {
    // 如果头节点为 null, 表示没有元素, 所以删除失败
    if (head == null) return false
    // 如果头节点就是要删除, 则删除当前的头节点, 并把下一个节点设置为头节点. 如果删除的节点也是尾节点,则需要更新尾节点
    if (head.value == ele) {
      if (head.eq(tail)) { // 必须比较是否为同一个对象. 等价于 java 的比较地址值是否相等
        tail = head.next // 更新尾节点
      }
      head = head.next // 更新头节点
      return true
    } else { // 如果头节点不是要删除的节点, 则遍历后面的节点
      var currentNode: Node = head // 当前节点
      var nextNode: Node = currentNode.next // 下一个节点
      while (nextNode != null) {
        if (nextNode.value == ele) { // 删除
          currentNode.next = nextNode.next // 让当前节点指向下一个节点的下一个节点
          if (nextNode.eq(tail)) { // 如果要删除的节点是尾节点, 尾节点需要指向当前节点
            tail = currentNode
          }
          return true
        }
        currentNode = nextNode
        nextNode = currentNode.next
      }
    }

    false
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