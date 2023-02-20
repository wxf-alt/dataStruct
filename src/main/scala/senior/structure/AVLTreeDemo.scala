package senior.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/17 14:22:02
 * @Description: AVLTreeDemo  平衡二叉树 实现
 * @Version 1.0.0
 */
object AVLTreeDemo {
  def main(args: Array[String]): Unit = {
    // 添加测试
    val tree: AVLTree[Int] = initTree
    tree.infixForeach(x => print(x + "->"))
    println()
    println(tree.height)
  }

  def initTree(): AVLTree[Int] = {
    // 失衡节点 4；右右失衡。需要进行左旋
    //    val arr: Array[Int] = Array(10, 4, 16, 1, 20, 7, 8, 6, 9) // 右右失衡（需要左旋）
    //    val arr: Array[Int] = Array(4, 3, 6, 5, 7, 8) // 根节点失衡
    //    val arr: Array[Int] = Array(10, 12, 8, 9, 7, 6) // 左左失衡（需要右旋）
//    val arr: Array[Int] = Array(10, 11, 7, 6, 8, 9) // 左右失衡（先进行左旋，再进行右旋）
    val arr: Array[Int] = Array(10, 8, 16, 18, 14, 12) // 右左失衡（先进行右旋，再进行左旋）
    val tree: AVLTree[Int] = new AVLTree[Int]
    for (elem <- arr) {
      tree.add(elem)
    }
    tree
  }

}

// 排序二叉树
class AVLTree[T: Ordering] {

  // 使用冥界召唤 Ordeing[T] 隐式值
  private val ord: Ordering[T] = implicitly[Ordering[T]]

  var root: AVLNode[T] = _

  // 添加节点
  def add(v: T): Unit = {
    if (null == root) {
      root = new AVLNode[T](v)
    } else {
      root.add(v) // 如果要添加的节点不是root，交给root节点去完成
      if (null != root.p) {
        root = root.p
      }
    }
  }

  def height: Int = if (null == root) -1 else root.height()

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
  def search(v: T): AVLNode[T] = {
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
class AVLNode[T: Ordering](var value: T) {

  // 使用冥界召唤 Ordeing[T] 隐式值
  private val ord: Ordering[T] = implicitly[Ordering[T]]

  // 左节点
  var left: AVLNode[T] = _
  // 右节点
  var right: AVLNode[T] = _
  // 父节点
  var p: AVLNode[T] = _

  // 添加节点
  def add(v: T): Boolean = {
    if (ord.lteq(v, value)) { // 要么添加到左子树，要添加的值小于等于当前 root的值
      if (null == left) { // 如果左节点是null，则新添加的值直接成为左节点，否则交给左节点继续处理
        left = new AVLNode(v)
        left.p = this // 左节点的父节点应该是当前节点
      }
      else left.add(v) // 如果左节点不是null，交给左节点继续处理
    } else { // 要么添加到右子树，要添加的值大于当前 root值
      if (null == right) {
        right = new AVLNode(v)
        right.p = this // 右节点的父节点应该是当前节点
      }
      else right.add(v)
    }
    rotate()
    true
  }

  // 平衡
  def rotate(): Unit = {
    // 左左失衡  --》右旋
    // 当前 左树的高度 - 右树的高度 > 1；并且 左子树 的 左 - 右 > 0
    if (leftHeight - rightHeight > 1 && left.leftHeight - left.rightHeight > 0) {
      println(s"左左失衡：${value}")
      rightRotate()
      return
    }
    // 右右失衡 --》左旋
    // 当前 左树的高度 - 右树的高度 < -1；并且 右子树 的 左-右 < 0
    if (leftHeight - rightHeight < -1 && right.leftHeight - right.rightHeight < 0) {
      println(s"右右失衡：${value}")
      println("平衡前的高度" + height())
      leftRotate()
      println("平衡后的高度" + height())
      return
    }
    // 左右失衡 --》先左旋 再右旋
    if (leftHeight - rightHeight > 1 && left.leftHeight - left.rightHeight < 0) {
      println(s"左右失衡：${value}")
      // 当前失衡的节点的左节点做 左旋
      left.leftRotate
      // 再对 当前节点做 右旋
      rightRotate()
      return
    }
    // 右左失衡 --》先右旋 再左旋
    if (leftHeight - rightHeight < -1 && right.leftHeight - right.rightHeight > 0) {
      println(s"右左失衡：${value}")
      // 当前失衡的节点的右节点做 右旋
      right.rightRotate()
      // 再对 当前节点做 左旋
      leftRotate()
      return
    }

  }

  // 右旋
  def rightRotate(): Unit = {
    // 1.获取需要交换的节点
    val tempP: AVLNode[T] = p
    val tempLeft: AVLNode[T] = left // 当前节点的左节点
    val tempLeftRight: AVLNode[T] = tempLeft.right // 当前左节点的右子节点
    // 2.进行转换
    // 2.1 让当前节点的左节点 指向 当前左节点的子右节点
    left = tempLeftRight
    // 2.2 让当前节点的成为左节点的右节点
    tempLeft.right = this
    // 2.3 让当前父节点的子节点指向当前节点的左节点
    if (null != tempP && tempP.left == this) {
      tempP.left == tempLeft
    } else if (null != tempP) {
      tempP.right = tempLeft
    }
    // 2.4 更新父节点
    if (null != tempLeftRight) tempLeftRight.p = this
    this.p = tempLeft
    tempLeft.p = tempP
  }

  // 左旋
  def leftRotate(): Unit = {
    // 1.先缓存需要变换的节点
    val temp: AVLNode[T] = p
    val temprRight: AVLNode[T] = this.right
    val temprRightLeft: AVLNode[T] = right.left
    // 2.进行旋转
    // 2.1 让当前节点的 右节点 指向 原来右节点的左节点
    right = temprRightLeft
    // 2.2 让原来的右节点的左节点 指向 当前节点
    temprRight.left = this
    // 2.3 让当前节点的父节点的左节点 指向 原来的右节点
    if (null != temp && temp.left == this) { // 表示当前节点是他爹的左节点
      temp.left = temprRight
    } else if (null != temp) {
      temp.right = temprRight
    }
    // 2.4 建立父节点关系
    if (temprRightLeft != null) temprRightLeft.p = this
    this.p = temprRight
    temprRight.p = temp
  }

  // 获取 树 的高度
  def height(): Int = leftHeight.max(rightHeight) + 1

  def leftHeight: Int = if (null == left) -1 else left.height()

  def rightHeight = if (null == right) -1 else right.height()


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
        val nonNullChildNode: AVLNode[T] = if (null != left) left else right
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
    var minNode: AVLNode[T] = this
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
  def search(v: T): AVLNode[T] = {
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
