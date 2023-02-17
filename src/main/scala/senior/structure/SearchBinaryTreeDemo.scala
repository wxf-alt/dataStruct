package senior.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/17 14:22:02
 * @Description: SearchBinaryTreeDemo  排序二叉树 实现
 * @Version 1.0.0
 */
object SearchBinaryTreeDemo {
  def main(args: Array[String]): Unit = {

    // 添加测试
    val tree: SearchBinaryTree[Int] = initTree
    tree.infixForeach(x => print(x + "->"))
    println()

    //    // 查找测试
    //    val value: TreeNode[Int] = tree.search(10)
    //    val left: Any = if (null != value.left) value.left.value else ""
    //    val right: Any = if (null != value.right) value.right.value else ""
    //    //    println(value.left.value + "->" + value.value + "->" + value.right.value)
    //    println(left + "->" + value.value + "->" + right)
    //    println(value)

    // 删除测试
    tree.delete(60)
    tree.infixForeach(x => print(x + "->"))
    println()
    println(tree.root.value)

  }

  def initTree() = {
    val arr: Array[Int] = Array(50, 60, 70, 58, 57, 40, 45, 30, 20, 80)
    val tree: SearchBinaryTree[Int] = new SearchBinaryTree[Int]
    for (elem <- arr) {
      tree.add(elem)
    }
    tree
  }

}

// 排序二叉树
class SearchBinaryTree[T: Ordering] {

  // 使用冥界召唤 Ordeing[T] 隐式值
  private val ord: Ordering[T] = implicitly[Ordering[T]]

  var root: TreeNode[T] = _

  // 添加节点
  def add(v: T): Unit = {
    if (null == root) {
      root = new TreeNode[T](v)
    } else root.add(v) // 如果要添加的节点不是root，交给root节点去完成
  }

  // 删除节点
  def delete(v: T): Boolean = {
    if (null == root) false
    else if (ord.equiv(v, root.value)) { // 如果删除的是根节点
      if (null == root.left && null == root.right) { // 表示只有根节点,不存在左右节点
        root = null
      } else if (null != root.left && null != root.right) { // 左右节点都存在
        // 找到 右子树 中最小的节点,将他作为 root 进行存储
        // 删除右子树中最小的节点,将最小节点的值赋值给root节点
        root.value = root.right.deleteMin()
      } else if (null != root.left && null == root.right) { // 左节点存在,右节点不存在
        root = root.left // root 指向left
        root.p = null // 现在root的父节点指向 null
      } else { // 左节点不存在,右节点存在
        root = root.right // root 指向right
        root.p = null // 现在root的父节点指向 null
      }
      true
    } else { // 如果删除的不是根节点
      root.delete(v)
    }
  }


  // 查找元素
  def search(v: T): TreeNode[T] = {
    // 空树直接返回null
    if (null == root) null
    root.search(v)
  }

  // 中序遍历
  // 顺序：左->当前节点->右
  def infixForeach(op: T => Unit): Unit = {
    if (null != root) {
      if (null != root.left) root.left.infixForeach(op)
      op(root.value)
      if (null != root.left) root.right.infixForeach(op)
    }
  }

}

// 树存储的节点
class TreeNode[T: Ordering](var value: T) {

  // 使用冥界召唤 Ordeing[T] 隐式值
  private val ord: Ordering[T] = implicitly[Ordering[T]]

  // 左节点
  var left: TreeNode[T] = _
  // 右节点
  var right: TreeNode[T] = _
  // 父节点
  var p: TreeNode[T] = _

  // 添加节点
  def add(v: T): Boolean = {
    if (ord.lteq(v, value)) { // 要么添加到左子树，要添加的值小于等于当前 root的值
      if (null == left) { // 如果左节点是null，则新添加的值直接成为左节点，否则交给左节点继续处理
        left = new TreeNode(v)
        left.p = this // 左节点的父节点应该是当前节点
      }
      else left.add(v) // 如果左节点不是null，交给左节点继续处理
    } else { // 要么添加到右子树，要添加的值大于当前 root值
      if (null == right) {
        right = new TreeNode(v)
        right.p = this // 右节点的父节点应该是当前节点
      }
      else right.add(v)
    }
    true
  }

  // 删除节点（不是根节点）
  def delete(v: T): Boolean = {
    // 1.当前节点就是要删除的节点
    if (ord.equiv(v, value)) {
      // 1.1 如果删除的是叶子节点
      // 1.1.1 判断当前节点是否为父节点的左子树还是右子树
      var isLeft: Boolean = true // 假设是左儿子
      if (null != p && this.eq(p.right)) { // 父节点不为null；并且当前节点 = 他父亲的右儿子
        // 那么他就为右儿子
        isLeft = false
      }
      // 1.1.2 判断当前节点是否叶子节点;然后删除叶子节点
      if (null == this.left && null == this.right) { // 1.1 如果删除的是叶子节点
        if (isLeft) p.left = null
        else p.right = null
      } else if (null != this.left && null != this.right) { // 1.2 删除的节点的 左右 都存在
        this.value = this.right.deleteMin()
      } else { // 1.3 删除的节点 只有一方存在
        // 找到非空子节点
        val nonNullChildNode: TreeNode[T] = if (null != left) left else right
        // 让非空子节点的父节点指向当前节点的父节点
        nonNullChildNode.p = this.p
        // 让 p 的left或者right 指向 当前非空节点
        if (isLeft) p.left = nonNullChildNode
        else p.right = nonNullChildNode
      }
      true
    } else if (ord.lt(v, value)) { // 2.要删除的数据 小于 当前节点
      if (null == left) false // 表示删除元素不存在
      else left.delete(v)
    } else { // 3.要删除的数据 大于 当前节点
      if (null == right) false // 表示删除元素不存在
      else right.delete(v)
    }
  }

  // 删除最小的节点
  def deleteMin(): T = {
    // 假设当前节点是最小节点
    var minNode: TreeNode[T] = this
    while (null != minNode.left) {
      minNode = minNode.left
    }
    // 调用节点的删除方法进行删除
    val v1: T = minNode.value
    minNode.delete(v1)
    // 返回最小值
    v1
  }

  // 查找数据
  def search(v: T): TreeNode[T] = {
    // 先判断当前节点是否是要找的节点
    if (ord.equiv(v, value)) this
    // 如果不是，判断是否小于当前节点，如果小于去left找
    else if (ord.lt(v, value) && null != left) left.search(v)
    // 如果大于去 right 找
    else if (ord.gt(v, value) && null != right) right.search(v)
    else null
  }

  // 遍历树
  def infixForeach(op: T => Unit): Unit = {
    if (null != left) left.infixForeach(op)
    op(value)
    if (null != right) right.infixForeach(op)
  }

  override def toString: String = s"value = ${value}"
}
