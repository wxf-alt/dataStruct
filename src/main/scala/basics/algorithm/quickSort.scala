package basics.algorithm

/**
 * @Auther: wxf
 * @Date: 2023/2/14 20:23:41
 * @Description: quickSort  快排实现
 * @Version 1.0.0
 */
object quickSort {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(30, 50, 70, 60, 10, 20)
    sort(arr)
    println(arr.mkString(","))
    val arr1: Array[Int] = Array(30, 50, 70, 60, 10, 20)
    println(scalaQuickSort(arr1).toBuffer)
    println(scalaQuickSortList(arr1.toList))

  }

  // 原地排序
  def sort(arr: Array[Int]): Unit = {

    quick(arr, 0, arr.length - 1)

    // 对快排进行分区
    def partition(arr: Array[Int], left: Int, right: Int): Int = {
      // 基准值
      val p: Int = arr(left)
      // 左指针
      var low: Int = left
      // 右指针
      var high: Int = right

      while (low < high) { // 只要左指针还在右指针的左边就一直循环查找 交换数据
        // 左指针向右跑；跑到 大于基准值的时候停下来
        while (low <= right && arr(low) <= p) { // 左指针不要超过右指针
          low += 1
        }
        // 右指针向左跑；跑到小于基准值的时候停下来
        while (high >= left && arr(high) > p) { // 右指针不要超过左指针
          high -= 1
        }
        // 交换 左右指针的元素
        if (low < high) {
          swap(arr, low, high)
        }
      }
      // 返回 基准值 的正确位置
      swap(arr, left, high)
      high
    }

    // 排序; left 开始的坐标，right 结素的坐标[left，right]
    def quick(arr: Array[Int], left: Int, rigth: Int): Unit = {
      // 如果左指针与右指针已经重合，或者到了右指针的右边。无需排序
      if (left >= rigth) return
      // 基准值的位置  10 保证左边的元素全部小于位置10，右边的元素全部大于位置10
      val mid: Int = partition(arr, left, rigth)
      // 对左边排序
      quick(arr, left, mid - 1)
      // 对右边排序
      quick(arr, mid + 1, rigth)
    }


  }

  // 不是原地排序，而实返回排好序的新的数组，原来的数组不做任务变化
  def scalaQuickSort(arr: Array[Int]): Array[Int] = {
    arr match {
      case Array(p, rest@_*) =>
        // 找到小于 p 的所有元素
        val left = scalaQuickSort(rest.filter(_ <= p).toArray)
        // 找到大于 p 的所有元素
        val right = scalaQuickSort(rest.filter(_ > p).toArray)
        (left :+ p) ++ right
      case _ => Array()
    }
  }

  // list 进行排序
  def scalaQuickSortList(list: List[Int]): List[Int] = {
    list match {
      case p :: rest =>
        scalaQuickSortList(rest.filter(_ <= p)) ::: p :: scalaQuickSortList(rest.filter(_ > p))
      case Nil => Nil
    }
  }

}
