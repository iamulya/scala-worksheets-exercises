//example of non-strict/lazy function in almost every language: && and ||

case object MyEmpty extends MyStream[Nothing]
case class MyCons[A](head: () => A, tail: () => MyStream[A]) extends MyStream[A]

trait MyStream[+A] {
  import MyStream._

  def toList: List[A] = this match {
    case MyCons(h,t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int) : MyStream[A] = this match {
    case MyEmpty => myempty
    case MyCons(h, t) if n > 0 => mycons(h(), t().take(n - 1))
    case MyCons(h, t) if n == 0 => myempty
  }
}


object MyStream {
  def myempty[A]: MyStream[A] = MyEmpty

  def mycons[A](h: => A, t: => MyStream[A]) : MyStream[A] = {
    lazy val head = h
    lazy val tail = t
    MyCons(() => head, () => tail)
  }

  def apply[A](as: A*) : MyStream[A] = if(as.isEmpty) myempty else mycons(as.head, apply(as.tail: _*))
}

def toList[A](str: MyStream[A]): List[A] = str match {
  case MyCons(h,t) => h() :: t().toList
  case _ => List()
}

def take[A](str: MyStream[A])(n: Int) : MyStream[A] = str match {
  case MyEmpty => MyStream.myempty
  case MyCons(h, t) if n > 0 => MyStream.mycons(h(), take(t())(n - 1))
  case MyCons(_, _) if n == 0 => MyStream.myempty
}

def drop[A](str: MyStream[A])(n: Int) : MyStream[A] = str match {
  case MyEmpty => MyStream.myempty
  case MyCons(h, t) if n > 0 => drop(t())(n - 1)
  case MyCons(h, t) if n == 0 => MyStream.mycons(h(), t())
}

def takeWhile[A](str: MyStream[A])(p: A => Boolean): MyStream[A] =  str match {
  case MyCons(h, t) if p(h()) => MyStream.mycons(h(), takeWhile(t())(p))
  case _ => MyStream.myempty
}

def foldRight[B, A](str: MyStream[A])(z: => B)(f: (A, => B) => B): B =
  str match {
    case MyCons(h,t) => f(h(), foldRight(t())(z)(f))
    case _ => z
  }

def exists[A](str: MyStream[A])(p: A => Boolean): Boolean = foldRight(str)(false)((a, b) => p(a) || b)

def forAll[A](str: MyStream[A])(p: A => Boolean): Boolean = str match {
  case MyCons(h,t) => p(h()) && forAll(t())(p)
  case _ => true
}

def takeWhileFoldRight[A](str: MyStream[A])(p: A => Boolean): MyStream[A] =
  foldRight(str)(MyStream.myempty[A])((a,b) => if(p(a)) MyStream.mycons(a,b) else MyStream.myempty[A])

def headOption[A](str: MyStream[A]): Option[A] = foldRight(str)(None: Option[A])((a,_) => Some(a))

def map[A,B](str: MyStream[A])(f: A => B) : MyStream[B] = str match {
  case MyCons(h, t) => MyStream.mycons(f(h()), map(t())(f))
  case _ => MyStream.myempty
}

def filter[A](str: MyStream[A])(p: A => Boolean): MyStream[A] = str match {
  case MyCons(h, t) if p(h()) => MyStream.mycons(h(), filter(t())(p))
  case MyCons(h, t) if !p(h()) => filter(t())(p)
  case _ => MyStream.myempty
}

def append[A, B>:A](str: MyStream[A])(s: => MyStream[B]): MyStream[B] = foldRight(str)(s)((a,b) => MyStream.mycons(a,b))

def flatmap[A,B](str: MyStream[A])(f: A => MyStream[B]) : MyStream[B] = str match {
  case MyCons(h, t) => append(f(h()))(flatmap(t())(f))
  case _ => MyStream.myempty
}

/*data                 codata
inductive            coinductive
finite objects       infinite objects
structural recursion guarded corecursion
structural induction guarded coinduction*/

val str1 = take(MyStream(1,2,3))(4).toList
val str2 = drop(MyStream(1,2,3))(2).toList
val str3 = takeWhile(MyStream(1,2,3))(a => a < 3).toList
val str4 = forAll(MyStream(1,2,3))(a => a <= 3)
val str5 = takeWhileFoldRight(MyStream(1,2,3))(a => a < 3).toList
val str6 = headOption(MyStream.myempty)


