import scala.util.Random

/**
 * @Auther: wxf
 * @Date: 2023/2/16 11:47:50
 * @Description: mergeSortTest
 * @Version 1.0.0
 */
object MergeSortTest {

  def main(args: Array[String]): Unit = {
    val random: Random = new Random()
    val arr: Array[Int] = (1 to 5).map(_ => random.nextInt(1000)).toArray
    println(arr.mkString(","))
    mergrSort(arr)
    println(arr.mkString(","))
  }

  def mergrSort(arr: Array[Int]) = {
    sort(arr, 0, arr.length - 1)
  }

  def sort(arr: Array[Int], start: Int, stop: Int): Unit = {
    if (start >= stop) return
    val mid: Int = (start + stop) / 2
    sort(arr, start, mid)
    sort(arr, mid + 1, stop)
    // 合并
    merge(arr, start, mid, stop)
  }

  def merge(arr: Array[Int], start: Int, mid: Int, stop: Int): Unit = {
    // 将有序的数据截取出来
    val left: Array[Int] = arr.slice(start, mid + 1) :+ Int.MaxValue
    val right: Array[Int] = arr.slice(mid + 1, stop + 1) :+ Int.MaxValue
    // 定义数组遍历索引
    var leftIndex: Int = 0
    var rightIndex: Int = 0
    for (elem <- start to stop) {
      if (left(leftIndex) <= right(rightIndex)) {
        arr(elem) = left(leftIndex)
        leftIndex += 1
      } else {
        arr(elem) = right(rightIndex)
        rightIndex += 1
      }
    }
  }

}
