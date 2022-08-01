import Chapter6.Exercise6_1.nonNegativeInt

import scala.annotation.tailrec

object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = seed + 1;
      val nextRNG = SimpleRNG(newSeed)
      (((seed >> 16).toInt, nextRNG))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val rInt: Rand[Int] = _.nextInt

  def rUnit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (aval, rnext) = s(rng)
      (f(aval), rnext)
    }
  }

  object Exercise6_1 {
    @tailrec
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (next, r) = rng.nextInt
      if (next == Integer.MIN_VALUE) nonNegativeInt(r)
      else (Math.abs(next), r)
    }
  }

  object Exercise6_2 {
    @tailrec
    def double(rng: RNG): (Double, RNG) = {
      val (next, r) = Exercise6_1.nonNegativeInt(rng)
      if (next == Integer.MAX_VALUE) double(r)
      else (next.toDouble / Integer.MAX_VALUE, r)
    }
  }

  object Exercise6_4 {
    def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
      if (n == 0) (List(), rng)
      else {
        val (nn, r) = rng.nextInt
        val (t, rr) = ints(n - 1)(r)
        (nn :: t, rr)
      }
    }
  }

  object Exercise6_4_1 {
    def double(rng: RNG): Rand[Double] = {
      map(Exercise6_1.nonNegativeInt)(t => t.toDouble / Integer.MAX_VALUE)
    }
  }

  object Exercise6_6 {
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      (source: RNG) => {
        val (va, nra) = ra(source)
        val (vb, nrb) = rb(nra)
        val vc = f(va, vb)
        (vc, nrb)
      }
    }
  }

  object Exercise6_7 {
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      (rng: RNG) =>
        fs match
          case Nil => (Nil, rng)
          case a :: rest => {
            val (nextVal, nextRNG) = a(rng)
            val (tail, nextNext) = sequence(rest)(nextRNG)
            (nextVal :: tail, nextNext)
          }
    }

    def ints(n: Int)(rng: RNG): Rand[List[Int]] = {
      val source: List[Rand[Int]] = List.fill(n)(rInt)
      sequence(source)
    }

  }

  object Exercise6_8 {
    def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = {
      (source: RNG) => {
        val (na, nr) = ra(source)
        f(na)(nr)
      }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(rInt) { (i: Int) =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) rUnit(mod)
        else nonNegativeLessThan(n)
      }
    //
    //
    //      (source: RNG) => {
    //      val (i, nr) = nonNegativeInt(source)
    //      val mod = i%n
    //      if(i + (n-1) - mod >= 0) (i, nr)
    //      else {
    //        flatMap()
    //      }
    //    }
  }

  def main(args: Array[String]): Unit = {
    import Exercise6_8._
    val i = SimpleRNG(System.nanoTime())
    println(nonNegativeLessThan(256981)(i)._1)

  }

  object Chapter6_5 {
    case class State[S, +A](run: S => (A, S)) {
      def map[B](f: A => B): State[S, B] = {
        val g: S => (B, S) = (source: S) => {
          val (aa, ss) = run(source)
          (f(aa), ss)
        }
        State(g)
      }

      def flatMap[B](f: A => State[S, B]): State[S, B] = {
        val g: S => (B, S) = (source: S) => {
          val (aa, ss) = run(source)
          val (aaa, sss) = f(aa).run(ss)
          (aaa, sss)
        }
        State(g)
      }

    }

    object State {
      def unit[S, A](a: A): State[S, A] = {
        State((s) => (a, s))
      }


    }

  }

  object Exercise6_11 {
    import Chapter6_5._

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def simulateOne(input: Input, machine: Machine ): Machine = {
      (input, machine) match {
        case (_, Machine(locked, 0, coins)) => Machine(locked, 0, coins)
        case (Coin, Machine(true, candies, coins)) if (candies > 0) => Machine(false, candies, coins+1)
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies-1, coins)
        case (_, m) => m
      }
    }

    def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
      val run: Machine => ((Int, Int), Machine) = (initial: Machine) => {
        inputs match {
          case Nil => ((initial.coins, initial.candies), initial)
          case input :: rest => {
            val m = simulateOne(input, initial)
            simulate(rest).run(m)
          }
        }
      }
      State(run)
    }



  }

}
