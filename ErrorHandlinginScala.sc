/* Why avoid exceptions */
//1. exceptions make the code context dependant and thus its no more referentially transparent
//2. Exceptions are not typesafe

//we need to keep the biggest benefit of exceptions: consolidate and centralize error-handling logic
/*A common idiom is to do option.getOrElse(throw new Exception("FAIL")) to convert
the None case of an Option back to an exception. The general rule of thumb is
that we use exceptions only if no reasonable program would ever catch the exception;
if for some callers the exception might be a recoverable error, we use Option (or
Either, discussed later) to give them flexibility*/



case class MySome[+A](v: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this.map(f) getOrElse MyNone

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = Some(this).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] = flatMap(a => if (f(a)) MySome(a) else MyNone)

}

object MyOption {
  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //Between map, lift, sequence, traverse, map2, map3, and so on, you should never have
  //to modify any existing functions to work with optional values.

  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = a flatMap(val1 => b map(val2 => f(val1, val2)))

  import scala.collection.immutable.List
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = a match {
    case Nil => MySome(Nil)
    case ::(x, xs) => x flatMap (val1 => sequence(xs) map (val1 :: _))//map2(x, sequence(xs))(_ :: _)
  }

  def sequenceWithFold[A](a: List[MyOption[A]]): MyOption[List[A]] = a.foldRight[MyOption[List[A]]](MySome(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => MySome(Nil)
    case ::(x, xs) => f(x) flatMap (val1 => traverse(xs)(f) map (val1 :: _))//map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def sequenceViaTraverse[A](a: List[MyOption[A]]): MyOption[List[A]] =
    traverse(a)(x => x)
}


case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap (a => b map (b1 => f(a, b1)))
}

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

