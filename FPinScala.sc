import scala.annotation.tailrec

/* Warm up */
def fib(n: Int) : Int = {
  @tailrec
  def fibloop(acc1 : Int, acc2 : Int, n: Int) : Int = {
    if (n == 1) acc1
    else if (n == 2) acc2
    else fibloop(acc2, acc1 + acc2, n - 1)
  }
  fibloop(0, 1, n)
}

fib(13)

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
  @tailrec
  def loop(n: Int) : Boolean = {
    if (n > as.length - 2) return true
    if (!ordered(as(n), as(n+1))) false
    else loop(n+1)
  }
  loop(0)
}

isSorted[Int](Array(3, 5, 4), (a, b) => b > a)

def partial1[A, B, C](a: A, f: (A,B) => C) : B => C = (b) => f(a, b)

// curry uses partial application
def curry[A, B, C](f: (A, B) => C) : A => (B => C) = (a) => partial1(a, f)

// => is right-associative, so A => B => C eq A => (B => C)
def uncurry[A, B, C](f: A => B => C) : (A, B) => C = (a, b) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B) : A => C = (a) => f(g(a))

/* Functional data structures */
sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList{
  def sum(ints: MyList[Int]) : Int = ints match {
    case MyNil => 0
    case MyCons(head, tail) => head + sum(tail)
  }

  def product(ds: MyList[Double]) : Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(d, ds) => d * product(ds)
  }

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: MyList[A]) : MyList[A] = drop(xs, 1)

  def setHead[A](x: A, xs: MyList[A]) : MyList[A] = xs match {
    case MyNil => MyNil
    case MyCons(h, t) => MyCons(x, t)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case MyNil => a2
    case MyCons(h, t) => MyCons(h, append(t, a2))
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) if t != MyNil => MyCons(h, init(t))
    case _ => MyNil
  }
}

val x = MyList(1,2,3,4,5) match {
  case MyCons(x, MyCons(2, MyCons(4, _))) => x
  case MyNil => 42
  case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) =>x+y
  case MyCons(h, t) => h + MyList.sum(t)
  case _ => 101
}

val myList = MyList(1, 2, 3, 4)
val brokenList = MyList.drop(myList, 5)
val tailsyo = MyList.tail(myList)
val inityo = MyList.init(myList)
val dropyo = MyList.dropWhile[Int](myList, (_ => true))

