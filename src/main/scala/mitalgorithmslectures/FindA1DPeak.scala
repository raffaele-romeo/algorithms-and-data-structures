package mitalgorithmslectures

import scala.annotation.tailrec

object FindA1DPeak {

  //Given an integer array nums, find a peak element, and return its index.
  def find1DPick(nums: Array[Int]): Int = {
    @tailrec
    def loop(nums: Array[Int], low: Int, high: Int, n: Int): Int = {
      val mid = low + (high - low) / 2

      if ((mid == 0 || nums(mid - 1) <= nums(mid)) && (mid == n - 1 || nums(mid + 1) <= nums(mid)))
        mid
      else if(mid > 0 && nums(mid - 1) > nums(mid))
        loop(nums, low, mid - 1, n)
      else
        loop(nums, mid + 1, high, n)
    }

    loop(nums, 0, nums.length - 1, nums.length)
  }

  def main(args: Array[String]): Unit = {
    println(find1DPick(Array(1, 3, 20, 4, 1, 0)))

  }

}
