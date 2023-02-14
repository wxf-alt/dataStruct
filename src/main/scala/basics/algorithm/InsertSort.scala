package basics.algorithm

/**
 * @Auther: wxf
 * @Date: 2023/2/14 19:42:03
 * @Description: InsertSort   插入排序 实现
 * @Version 1.0.0
 */
object InsertSort {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(30, 20, 10, 60, 50, 70)
    println(arr.mkString(","))
    sort(arr)
    println(arr.mkString(","))
  }

  import scala.util.control.Breaks._

  def sort(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length - 1) {
      breakable {
        for (j <- (i + 1) until(0, -1)) {
          // 后面小于前面时候，在进行交换
          if (arr(j) < arr(j - 1)) {
            swap(arr, j, j - 1)
          } else break
        }
      }
    }
  }

}
