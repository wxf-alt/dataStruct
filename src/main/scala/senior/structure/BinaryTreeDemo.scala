package senior.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/17 10:57:18
 * @Description: BinaryTreeDemo 二叉树 实现 + 二叉树遍历
 * @Version 1.0.0
 */
object BinaryTreeDemo {
  def main(args: Array[String]): Unit = {
    val root: BinaryTree[Int] = new BinaryTree[Int](10)
    root.isRoot = true
    root.left = new BinaryTree[Int](9)
    root.right = new BinaryTree[Int](20)
    root.right.left = new BinaryTree[Int](15)
    root.right.right = new BinaryTree[Int](35)

    root.preForeach(x => print(x + "->"))
    println()
    root.infixForeach(x => print(x + "->"))
    println()
    root.postForeach(x => print(x + "->"))
  }
}

// 表示一个 二叉树
class BinaryTree[T](value: T) {

  // 左子树
  var left: BinaryTree[T] = _
  // 右子树
  var right: BinaryTree[T] = _
  // 节点存储的值

  var isRoot: Boolean = false

  // 添加一个节点
  def add(ele: T): Unit = ???

  // 前序遍历
  // 顺序：当前节点->左->右
  def preForeach(op: T => Unit): Unit = {
    op(value)
    if (null != left) left.preForeach(op)
    if (null != right) right.preForeach(op)
  }

  // 中序遍历
  // 顺序：左->当前节点->右
  def infixForeach(op: T => Unit): Unit = {
    if (null != left) left.infixForeach(op)
    op(value)
    if (null != right) right.infixForeach(op)
  }

  // 后序遍历
  // 顺序：左->右->当前节点
  def postForeach(op: T => Unit): Unit = {
    if (null != left) left.postForeach(op)
    if (null != right) right.postForeach(op)
    op(value)
  }

}