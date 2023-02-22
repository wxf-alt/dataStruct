package basics.structure

import scala.reflect.ClassTag

/**
 * @Auther: wxf
 * @Date: 2023/2/21 10:58:10
 * @Description: StackDemo
 * @Version 1.0.0
 */
object StackDemo {
  def main(args: Array[String]): Unit = {
    val stack: ArrayStack[Int] = new ArrayStack[Int](4)
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.push(4)

    println(stack.pop())
    println(stack.pop())
    println(stack.pop())
    println(stack.pop())
    println(stack.pop())

  }
}

// 使用数组模拟栈
class ArrayStack[T: ClassTag](val maxSize: Int) {
  private val arr: Array[T] = new Array[T](maxSize)
  // 栈顶()
  private var top: Int = -1

  // 栈是否空
  def isEmpty: Boolean = top == -1

  // 栈是否满
  def isFull: Boolean = top == maxSize - 1

  // 入栈
  def push(ele: T): Unit = {
    if (isFull) throw new UnsupportedOperationException("栈已经满员...")
    top += 1
    arr(top) = ele
  }

  // 出栈
  def pop(): Option[T] = {
    if (isEmpty) None
    else {
      val res: T = arr(top)
      top -= 1
      Some(res)
    }
  }

  // 获取栈顶元素, 但不弹出栈顶元素
  def peak(): Option[T] = {
    if (isEmpty) None
    else {
      Some(arr(top))
    }
  }
}