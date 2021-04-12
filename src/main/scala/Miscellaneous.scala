import scala.annotation.tailrec

object Miscellaneous {
  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    ar.foldLeft(Map[Int, Int]()) {
      (map, next) => map.+(next -> (map.getOrElse(next, 0) + 1))
    }.mapValues(_ /2).values.sum
  }

  def superDigit(n: String, k: Int): Int = {
    def getDigitSum(n: String): String = n.toStream.map(_.asDigit).sum.toString

    @tailrec
    def superDigitIntern(n: String): Int ={
      n.length match {
        case 1 => n.toInt
        case _ => superDigitIntern(getDigitSum(n))
      }
    }
    superDigitIntern(getDigitSum(n) * k)
  }


  def jumpingOnClouds(c: Array[Int]): Int = {
    @tailrec
    def inner(t: List[Int], nJumps: Int): Int = {
      t.tail match {
        case Nil => nJumps
        case _ :: Nil => nJumps + 1
        case b :: _ :: tail => if (b == 1) inner(tail, nJumps + 1) else inner(tail, nJumps + 2)
      }
    }

    inner(c.toList, 0)
  }

  def countingValleys(steps: Int, path: String): Int = {
    case class Info(upDown: Int = 0, nValleys: Int = 0, nMountains: Int = 0)
    path.toCharArray.foldLeft(Info()){
      case (info, step)  =>
        if (step == 'U') {
          val upDown = info.upDown + 1
          info.copy(upDown = upDown, nValleys = if (upDown == 0) info.nValleys + 1 else info.nValleys)
        } else {
          val upDown = info.upDown - 1
          info.copy(upDown = upDown, nMountains = if (upDown == 0) info.nMountains + 1 else info.nMountains)
        }
    }.nValleys
  }


  def arrayManipulation(n: Int, m: Int, queries: Array[Array[Int]]): Long = {
    val a = Array.ofDim[Long](n + 2)

    for {
      i <- 0 until m
    } {
      a(queries(i)(0)) = a(queries(i)(0)) + queries(i)(2)
      a(queries(i)(1) + 1) = a(queries(i)(1) + 1) - queries(i)(2)
    }

    0
  }

  case class FirstAndSecondMax[A](firstMax: A, secondMax: A)

  def getFirstAndSecondMax[A](list: List[A])(implicit A: Ordering[A]): Option[FirstAndSecondMax[A]] = {
    @tailrec
    def utilMethod(list: List[A], tmp: FirstAndSecondMax[A]): FirstAndSecondMax[A] = {
      list match {
        case Nil => tmp
        case ::(head, tl) =>
          if (A.gt(head, tmp.firstMax))
            utilMethod(tl, FirstAndSecondMax(head, tmp.firstMax))
          else if (A.gt(head, tmp.secondMax))
            utilMethod(tl, FirstAndSecondMax(tmp.firstMax, head))
          else utilMethod(tl, tmp)
      }
    }

    list match {
      case Nil => throw new RuntimeException("First and second maximum of empty list")
      case head :: second :: tail =>
        if (A.gt(head, second)) Some(utilMethod(tail, FirstAndSecondMax(head, second)))
        else Some(utilMethod(tail, FirstAndSecondMax(second, head)))
      case _ => throw new RuntimeException("First and second maximum of one element list")
    }
  }

}
