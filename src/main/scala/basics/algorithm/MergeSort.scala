package basics.algorithm


/**
 * @Auther: wxf
 * @Date: 2023/2/15 15:27:11
 * @Description: MergeSort 归并排序 实现
 * @Version 1.0.0
 */
object MergeSort {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(30, 50, 70, 60, 10, 20)
    mergrSort(arr)
    println(arr.mkString(","))
  }

  def mergrSort(arr: Array[Int]) = {
    sort(arr, 0, arr.length - 1)

    // [start,stop]
    def sort(arr: Array[Int], start: Int, stop: Int): Unit = {
      if (start >= stop) return
      // 分
      val mid: Int = (start + stop) / 2
      // 对左边排序
      sort(arr, start, mid)
      // 对右边进行排序
      sort(arr, mid + 1, stop)
      // 治（合并）
      merge1(arr, start, mid, stop)
    }

    // 合并
    def merge(arr: Array[Int], start: Int, mid: Int, stop: Int): Unit = {
      // 截取已经有序的数组 [start,mid]
      val left: Array[Int] = arr.slice(start, mid + 1)
      // 截取已经有序的数组 [mid + 1,stop]
      val right: Array[Int] = arr.slice(mid + 1, stop + 1)
      var leftIndex: Int = 0 // left数组索引
      var rightIndex: Int = 0 // right数组索引
      for (i <- start to stop) { // i表示原数组中的索引
        if (leftIndex == left.length) { // 如果左边取完了
          arr(i) = right(rightIndex)
          rightIndex += 1
        } else if (rightIndex == right.length) { // 如果右边取完了
          arr(i) = left(leftIndex)
          leftIndex += 1
        } else if (left(leftIndex) <= right(rightIndex)) {
          arr(i) = left(leftIndex)
          leftIndex += 1
        } else {
          arr(i) = right(rightIndex)
          rightIndex += 1
        }
      }


    }

    // 合并（添加一个哨兵）
    def merge1(arr: Array[Int], start: Int, mid: Int, stop: Int): Unit = {
      // 截取已经有序的数组 [start,mid)
      val left: Array[Int] = arr.slice(start, mid + 1) :+ Int.MaxValue
      // 截取已经有序的数组 [mid + 1,stop)
      val right: Array[Int] = arr.slice(mid + 1, stop + 1) :+ Int.MaxValue
      var leftIndex: Int = 0 // left数组索引
      var rightIndex: Int = 0 // right数组索引
      for (i <- start to stop) { // i表示原数组中的索引
        if (left(leftIndex) <= right(rightIndex)) {
          arr(i) = left(leftIndex)
          leftIndex += 1
        } else {
          arr(i) = right(rightIndex)
          rightIndex += 1
        }
      }
    }

  }

}