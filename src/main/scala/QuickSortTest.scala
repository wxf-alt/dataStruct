import scala.util.Random

/**
 * @Auther: wxf
 * @Date: 2023/2/16 11:15:02
 * @Description: QuickSortTest1   快排
 * @Version 1.0.0
 */
object QuickSortTest {

  def main(args: Array[String]): Unit = {
    val random: Random = new Random()
    //    val arr: Array[Int] = (1 to 500).map(_ => random.nextInt(1000)).toArray
    val arr: Array[Int] = Array(0, 30, 50, 70, 60, 10, 20)
    println(arr.mkString(","))
    quickSort(arr, 0, arr.length - 1)
    println(arr.mkString(","))

  }

  def partition(arr: Array[Int], start: Int, stop: Int): Int = {
    // 设置基准值
    val mid: Int = arr(start)
    var l: Int = start
    var r: Int = stop
    // 当 左指针 小于 右指针时 循环 排序
    while (l < r) {
      // 左指针对应的值 与 基准值 进行比较；如果比基准值小 继续查找
      while (l < stop && arr(l) <= mid) {
        l += 1
      }
      // 右指针对应的值 与 基准值 进行比较；如果比基准值大 继续查找
      while (r > start && arr(r) > mid) {
        r -= 1
      }
      if (l < r) { // 查找到 比基准值大，或者小的值；并且左指针没有超过右指针。那么进行替换
        swap(arr, l, r)
      }
    }
    // 到达这代表 左指针超过了右指针；呢么基准值与右指针对应的值进行交换(因为左边存储的都是小于基准值的数据；右指针找的就是小于基准值的数据)
    swap(arr, start, r)
    // 将右指针最后的索引返回。左边存储的都是小于他的数据，右边存储的都是大于他的数据
    r
  }

  private def swap(arr: Array[Int], l: Int, r: Int) = {
    val temp: Int = arr(l)
    arr(l) = arr(r)
    arr(r) = temp
  }

  def quickSort(arr: Array[Int], start: Int, stop: Int): Unit = {
    if (start >= stop) return
    val mid: Int = partition(arr, start, stop) // 获取基准值
    // 递归调用
    quickSort(arr, start, mid - 1)
    quickSort(arr, mid + 1, stop)
  }


}
