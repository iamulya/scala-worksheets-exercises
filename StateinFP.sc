trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
   def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

//State monad
case class MyState[S, +A](run: S => (A, S)) {

  def unit[S, A](a: A) : MyState[S, A] = MyState(s => (a,s))
  def flatMap[B](f: A => MyState[S, B]) : MyState[S, B] = MyState(s => {
    val (a,s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B) : MyState[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: MyState[S,B])(f: (A,B) => C) : MyState[S, C] = flatMap(a => sb.map(b => f(a,b)))

  def sequence[S,A](sas: List[MyState[S,A]]) : MyState[S, List[A]] = {
    def go(s: S, stateActions: List[MyState[S,A]], acc: List[A]) : (List[A], S) = {
      stateActions match {
        case Nil => (acc, s)
        case h :: t => h.run(s) match {
          case (a, s1) => go(s1, t, a :: acc)
        }
      }
    }
    MyState(s => go(s, sas, List()))
  }

  def sequenceViaFoldRight[S,A](sas: List[MyState[S, A]]): MyState[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((a, acc) => a.map2(acc)(_ :: _))
  /*def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B) : B = as match {
  case Nil => z
  case ::(x, xs) => f(x, foldRight(xs, z)(f))
  }
  f(a:MyState[S, A], foldRight(xs, unit[S, List[A]](List()))(f))*/

  def modify[S](f: S => S): MyState[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: MyState[S, S] = MyState(s => (s, s))

  def set[S](s: S): MyState[S, Unit] = MyState(_ => ((), s))

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): MyState[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}



