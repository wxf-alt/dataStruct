package basics.algorithm

/**
 * @Auther: wxf
 * @Date: 2023/2/14 19:19:35
 * @Description: BubbleSort   冒泡排序 实现
 * @Version 1.0.0
 */
object BubbleSort {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(30, 50, 70, 60, 10, 20)
    println(arr.mkString(","))
    sort(arr)
    println(arr.mkString(","))
  }

  // 数组原地排序
  def sort(arr: Array[Int]): Unit = {
    // 表示 需要对多少个元素进行排序
    for (j <- 0 until arr.length - 1) {
      // 排序 找到最大值
      for (i <- 0 until arr.length - 1 - j) {
        if (arr(i) > arr(i + 1)) {
          swap(arr, i, i + 1)
        }
      }
    }
  }

}