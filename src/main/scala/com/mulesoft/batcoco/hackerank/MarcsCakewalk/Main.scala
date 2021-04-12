package com.mulesoft.batcoco.hackerank.MarcsCakewalk

import scala.annotation.tailrec

object Solution {

  def fibonacci(n: Double): Double = {
    if (n < 3) {
      n + 1
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def fibonacci2(x: Int): BigInt = {

    @tailrec def fibHelper(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt =
      x match {
        case 0 => prev
        case 1 => next
        case _ => fibHelper(x - 1, next, next + prev)
      }
    fibHelper(x)
  }

  def main(args: Array[String]) {
    print(fibonacci2(1000))
  }

}
