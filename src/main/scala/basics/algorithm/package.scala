package basics

package object algorithm {

  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val temp: Int = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

}
