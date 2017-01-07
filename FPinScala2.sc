import scala.collection.immutable.List

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B) : B = as match {
  case Nil => z
  case ::(x, xs) => f(x, foldRight(xs, z)(f))
}

def sum(as: List[Int]) = foldRight(as, 0)(_ + _)
def product(as: List[Int]) = foldRight(as, 1)(_ * _)

val smit = sum(List(1,2,4,6))
val prit = product(List(1,2,6,3))

val newFR = foldRight(List(1,2,3), Nil:List[Int])(::(_,_))

def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => 1 + b)
val len = length(List(1,3,6,3))

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case ::(x, xs) => foldLeft(xs, f(z, x))(f)
}

def sumLeft(as: List[Int]) = foldLeft(as, 0)(_ + _)
def productLeft(as: List[Int]) = foldLeft(as, 1)(_ * _)
def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => 1 + b)

val smity = sumLeft(List(1,2,4,6))
val prity = productLeft(List(1,2,6,3))
val leny = lengthLeft(List(1,3,6,3))

def reverse(as: List[Int]) = foldLeft(as, Nil:List[Int])((b: List[Int], a: Int) => ::(a, b))
val rev = reverse(List(1,2,3))
val dick = ::(3, ::(2, ::(1, Nil)))

def append[A](a1: List[A], a2: List[A]): List[A] =
  a1 match {
    case Nil => a2
    case ::(h,t) => ::(h, append(t, a2))
  }

def appendRight[A](a1: List[A], a2: List[A]): List[A] =
foldRight(a1, a2)(::(_,_))

def mappend(ass: List[List[Int]]) : List[Int] = ass match {
  case Nil => Nil
  case ::(xs, xss) => append(xs, mappend(xss))
}

val t = mappend(List(List(1,2), List(3,4)))
val newappend = appendRight(List(1,2), List(3,4))

def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A, B) => B) : B = as match {
  case Nil => z
  case ::(x, xs) => foldLeft(xs, f(x, z))((b: B, a: A) => f(a, b))
}

def foldLeftViaRight[A,B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
  case Nil => z
  case ::(x, xs) => foldRight(xs, z)((a: A, b: B) => f(b, a))
}

def sumRightLeft(as: List[Int]) = foldRightViaLeft(as, 0)(_ + _)
val smitLeft = sumRightLeft(List(1,2,4,6))

def sumLeftRight(as: List[Int]) = foldLeftViaRight(as, 0)(_ + _)
val smitRight = sumLeftRight(List(1,2,4,6))

def map[A, B](as: List[A])(f: A => B) : List[B] = as match {
  case Nil => Nil
  case ::(x, xs) => ::(f(x), map(xs)(f))
}

val added = map(List(1,2,3))(x => x + 1)

def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Nil => Nil
  case ::(x, xs) if f(x) => ::(x, filter(xs)(f))
  case ::(x,xs) if !f(x) => filter(xs)(f)
}

val filtered = filter(List(1,2,3))(x => x == 1)

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
  case Nil => Nil
  case ::(x, xs) => append(f(x), flatMap(xs)(f))
}

val flatmapped = flatMap(List(1,2,3))(i => List(i,i))

def filterWithFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
flatMap(as)((a) => if(f(a)) List(a) else List())

val fil = filterWithFlatmap(List(1,2,3))(x => x == 1)

def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C) : List[C] =
  (as, bs) match {
    case(Nil, Nil) => Nil
    case (Nil, ::(_, _)) => Nil
    case (::(_, _), Nil) => Nil
    case (::(a, as), ::(b, bs)) => ::(f(a,b), zipWith(as, bs)(f))
  }

val zipped = zipWith(List(1,2,3), List(4,5,6))((x,y) => x + y)

def hasPrefix[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  case(Nil, Nil) => true
  case (Nil, ::(_, _)) => false
  case (::(_, _), Nil) => true
  case (::(a, as), ::(b, bs)) if a == b => hasPrefix(as, bs)
  case (::(a, _), ::(b, _)) if a != b => false
}

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  case(Nil, Nil) => false
  case (Nil, ::(_, _)) => false
  case (::(_, _), Nil) => false
  case (::(a, _), ::(b, _)) if a == b => hasPrefix(sup, sub)
  case (::(a, as), ::(b, _)) if a != b => hasSubsequence(as, sub)
}

val isIt = hasSubsequence(List(1,2,3,4), List(2,3))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
  case Leaf(a) => l(a)
  case Branch(t1, t2) => b(fold(t1)(l)(b), fold(t2)(l)(b))
}

def size(t: Tree[Int]) : Int = t match{
  case Leaf(_) => 1
  case Branch(t1, t2) => 1 + size(t1) + size(t2)
}

def sizeWithFold(t: Tree[Int]) : Int  = fold(t)(_ => 1)((a,b) => 1 + a + b)
def maxWithFold(t: Tree[Int]) : Int = fold(t)(a => a)((a, b) => scala.math.max(a,b))
def depthWithFold(t: Tree[Int]) : Int  = fold(t)(_ => 0)((a,b) => 1 + a + b)
def mapWithFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
  fold[A, Tree[B]](t)(a => Leaf(f(a)))((t1, t2) => Branch(t1, t2))

val tree1 = (Branch(Leaf(3), Leaf(4)))
val tsize = size(tree1)
val tsizeWithFold = sizeWithFold(tree1)
val max = maxWithFold(tree1)
val depth = depthWithFold(tree1)
val gemappd = mapWithFold(tree1)((x) => x*2)