package basics

import scala.reflect.ClassTag

/**
 * @Auther: wxf
 * @Date: 2023/2/10 16:50:27
 * @Description: ArrayQueue 队列 实现
 * @Version 1.0.0
 */
object QueueDemo {
  def main(args: Array[String]): Unit = {
    val queue: ArrayQueue[Int] = new ArrayQueue[Int](5)
    queue.enqueue(10)
    queue.enqueue(20)
    queue.enqueue(30)
    queue.enqueue(40)
    queue.enqueue(50)
    println(queue.dequeue())
    println(queue.dequeue())
    queue.enqueue(60)
    println(queue.dequeue())
    println(queue.dequeue())
    println(queue.dequeue())
    println(queue.dequeue())
    println(queue.dequeue())
  }
}

// 如果要使用 泛型数组，需要给T添加上下文
class ArrayQueue[T: ClassTag](initSize: Int) {
  // 定义数组 用来存储数据  使用循环队列
  val arr: Array[T] = new Array[T](initSize)
  // 队头(出队的时候,这个位置的元素出去)
  var head: Int = 0
  // 队尾  表示队列的下一个元素的位置(入队的时候,新的元素放在这个位置)
  var tail: Int = 0
  // 队列中元素的个数(用来判断队列是否为空,或者满了)
  var count: Int = 0

  // 判断队列是否为空
  def isEmpty: Boolean = count == 0

  // 判断队列已经满了
  def isFull: Boolean = count == initSize

  // 入队
  def enqueue(ele: T): Unit = {
    // 1.判断队列是否满了,如果满了,无法入队
    if (isFull) throw new UnsupportedOperationException("队列已满,无法添加元素")
    // 2.如果没有满,进行入队. 将新元素放在 tail 位置
    arr(tail) = ele
    // 2.1 tail 应该更新(tail + 1)
    tail += 1
    // 2.2 队列中元素的个数 + 1
    count += 1

    // 已经到了最后一个位置，那下一次的元素存储在数组头部
    if (tail == initSize) tail = 0
  }

  // 出队
  def dequeue() = {
    // 1.判断队列是否为空
    if (isEmpty) throw new UnsupportedOperationException("队列为空,无法出队")
    // 2.将队头的位置出去
    val e1: T = arr(head)
    // 2.1 head加1
    head += 1
    // 2.2 总数减一
    count -= 1
    // 循环查询元素
    if (head == initSize) head = 0
    e1
  }

  def printQueue(): Unit = {
    var temp: Int = head
    for (elem <- 0 until count) {
      println(arr(temp))
      temp += 1
      if (temp == initSize) temp = 0
    }
  }

}