package basics.algorithm

/**
 * @Auther: wxf
 * @Date: 2023/2/14 20:15:07
 * @Description: selectionSort  选择排序 实现
 * @Version 1.0.0
 */
object selectionSort {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(30, 20, 10, 60, 50, 70)
    println(arr.mkString(","))
    sort(arr)
    println(arr.mkString(","))
  }

  def sort(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length - 1) { // 一共 len 个元素, 只需要找到 len-1 个就可以了, 剩下一个位置自动正确
      var minIndex: Int = i // 选中第 i 个元素为第 i 小(只记录索引即可, 成功之后交换一次元素即可)
      for (j <- i + 1 until arr.length) { // 让第 i 个元素, 逐次与 i + 1 位置元素比较
        if (arr(j) < arr(minIndex)) minIndex = j // 如果有比 minIndex 的位置更小的元素, 就记录下新的索引
      }
      //把 i 位置和 minIndex 位置的元素交换
      if (i != minIndex) {
        swap(arr, i, minIndex)
      }
    }
  }

}
