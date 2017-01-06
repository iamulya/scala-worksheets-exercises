import scala.annotation.tailrec

//Functional programming entails referential transparency which entails more testability, composability, modularity and
//parallelism. Its seperation of concern at its highest niveau.
/*case classes*/

abstract class Notification

case class Email(body: String, title: String) extends Notification

case class Sms(message: String) extends Notification

val em = Email("wassup", "yo")

println(em)

/*Extractor objects*/
object Twice {
  def apply(x: Int): Int = x*2
  def unapply(y: Int): Option[Int] = if (y % 2 == 0) Some(y/2) else None
}

object Even {
  def unapply(x: Int): Boolean = (x % 2 == 0)
}

val t = Twice(21)
println(t)
t match { case Even() => println("its even")}

/*classes and traits*/
class Point(var x: Int, var y: Int) {
  override def toString: String = "x : " + x + " , " + y
}

abstract class AbsIterator {
  type T
  def hasNext: Boolean
  def next: T
}

//Note about traits:
//A trait is a class meant to be added to other classes as mixins
//The differentiating quality of traits is that the actual supertype of a trait is
//dependant on the type to which the trait has been added in a mixin composition.
//It is not known statically at the time of the definition of the trait

//Example: Assume a trait D defines some aspect of an instance x of type C (i.e. D is a base class
//of C). Then the actual supertype of D in x is the compound type consisting of all
// the base classes in L (C) that succeed D. The actual supertype gives the context for
// resolving a super reference in a trait.
trait RichIterator extends AbsIterator {
  def forEach(f: T => Unit) : Unit = while(hasNext) f(next)
}

class StringIterator(s: String) extends AbsIterator {
  type T = Char
  private var i = 0
  def next: Char = {val ch = s.charAt(i); i += 1; ch}

  def hasNext: Boolean = s.length > i
}

class Iter extends StringIterator("damn!") with RichIterator
val iter = new Iter
iter forEach println

/*Functions are first classes objects*/
class Decorator(left: String, right: String) {
  def layout[A](x: A) = left + x.toString + right
}

def applicar(f: Int => String, v: Int) : String = f(v)
val format = new Decorator("(", ")")
applicar(format.layout, 9)

/*Nested functions*/
def filter(xs: List[Int], threshold: Int): List[Int] = {
  def process(ys: List[Int]) : List[Int] =
    if(ys.isEmpty) ys
    else if (ys.head < threshold) ys.head :: process(ys.tail)
    else process(ys.tail)
  process(xs)
}

println(filter(List(1,4,8,9,10,17), 9))

/*Companion objects and classes*/
class IntPair(var x: Int, var y: Int)

object IntPair {
  import math.Ordering

  implicit def ipOrdering : Ordering[IntPair] = Ordering.by(pair => (pair.x, pair.y))
}

class foo {
  import foo._
  private def z = pp
  private def boo = "bar"

  def printIt = println(z)
}

object foo {
  private def pp = "zaz"
}

val meh = new foo
meh.printIt

/*def and val*/
def even: Int => Boolean = _ % 2 == 0
val damn = even eq even

val even2: Int => Boolean = _ % 2 == 0
val queva = even2 eq even2

val test: () => Int = {
  val r = util.Random.nextInt
  () => r
}

test()
test()

val testDef: () => Int = {
  def r = util.Random.nextInt
  () => r
}

testDef()
// Int = -1049057402
testDef()

/*Sequence comprehensions*/
def showEven(from: Int, to: Int)  = {
  for(i <- from until to if i % 2 == 0) yield i
}

println(showEven(1, 90))

//fancy show
for(i <- Iterator.range(0, 20);
    j <- Iterator.range(i, 20) if i + j == 30) println(s"$i, $j")

/* Generic Classes */
class Stack[T] {
  var elems: List[T] = Nil
  def push(x: T) { elems = x :: elems }
  def top : T = elems.head
  def pop {elems = elems.tail}
}

/* Type parameter/Variance annotations*/
//Completely different from Java: Annotation added when the abstraction is defined, as opposed to when its used

//+ means A can only be used in covariant positions(return type of a function)
//- means A can only be used in contravariant positions(parameters of a function)

/** Lower type bounds **/
//While upper type bounds limit a type to a subtype of another type,
//      lower type bounds declare a type to be a supertype of another type.
class ImmutableStack[+A] {
  def push[B >: A](elem: B) : ImmutableStack[B] = new ImmutableStack[B] {
    override def top : B = elem
    override def pop : ImmutableStack[B] = ImmutableStack.this
    override def toString = elem.toString + ImmutableStack.this.toString
  }

  def top : A = sys.error("No element available")
  def pop : ImmutableStack[A] = sys.error("No element available")
  override def toString = ""
}

var s: ImmutableStack[Any] = new ImmutableStack().push("hello");
s = s.push(new Object())
s = s.push(7)
println(s)

/* Upper type bounds */
abstract class Animal {
  def whoami = "i am an animal"
}

abstract class Pet extends Animal

class Cat extends Pet {
  override def whoami: String = "Cat"
}
class Dog extends Pet {
  override def whoami: String = "Dog"
}

class Lion extends Animal {
  override def whoami: String = "Lion"
}

class Cage[P <: Pet](p: Pet) {
  def whozDaPet = p.whoami
}

var dogCage = new Cage[Dog](new Dog)
println(dogCage.whozDaPet)
var catCage = new Cage[Cat](new Cat)
//Cannot put Lion in a cage as Lion is not a Pet..Bad Lion //
//var lionCage = new Cage[Lion](new Lion)

/* Inner Classes */
//In Scala, the inner classes are bound to the outer object, unlike Java where its bound to the outer class
class Graph {
  class Node {
    var connectedNodes : List[Node] = Nil
    def connectTo(node: Node): Unit = {
      if(connectedNodes.find(node.equals).isEmpty) {
        connectedNodes = node :: connectedNodes
      }
    }
  }

  var nodes : List[Node] = Nil
  def createNode : Node = {
    val node = new Node
    nodes = node :: nodes
    node
  }
}

val g: Graph = new Graph
val n1: g.Node = g.createNode
val n2: g.Node = g.createNode
n1.connectTo(n2)      // legal
val h: Graph = new Graph
val n3: h.Node = h.createNode
//n1.connectTo(n3)      // illegal!, because n3 has a type h.Node, whereas n1 has g.Node
// In Scala, the type where Node is bound to the outer class Graph is represented as Graph#Node

class JavaLikeGraph {
  class Node {
    var connectedNodes: List[JavaLikeGraph#Node] = Nil
    def connectTo(node: JavaLikeGraph#Node) {
      if (connectedNodes.find(node.equals).isEmpty) {
        connectedNodes = node :: connectedNodes
      }
    }
  }
  var nodes: List[Node] = Nil
  def newNode: Node = {
    val res = new Node
    nodes = res :: nodes
    res
  }
}

/* Abstract types */
// Types as members of objects
// Like value definitions, we can override type definitions in subclasses.
trait Buffer {
  type T
  val element: T
}

abstract class SeqBuffer extends Buffer {
  type U
  type T <: Seq[U]
  def length = element.length
}

//traits or classes with abstract type members are often used in conjunction with anonymous class instantiations
abstract class IntSeqBuffer extends SeqBuffer {
  type U = Int
}

def createIntSeqBufferWith(elem1: Int, elem2: Int) : IntSeqBuffer = new IntSeqBuffer {
  override type T = List[U]
  override val element = List(elem1, elem2)
}

val buf = createIntSeqBufferWith(7, 8)
println("length = " + buf.length)
println("content = " + buf.element)

//it is often possible to reappropriate the abstract type members as type parameters of classes and of course vice versa
abstract class TypedBuffer[+T] {
  val element : T
}

abstract class TypedSeqBuffer[U, +T <: Seq[U]] extends TypedBuffer[T] {
  def length = element.length
}

def createTypedSeqBufferWith(elem1: Int, elem2: Int) : TypedSeqBuffer[Int, Seq[Int]] = new TypedSeqBuffer[Int, List[Int]] {
  val element = List(elem1, elem2)
}

val typedBuf = createTypedSeqBufferWith(7, 8)
println("length = " + typedBuf.length)
println("content = " + typedBuf.element)

/* Compound Types */
trait Cloneable extends java.lang.Cloneable {
  override def clone(): Cloneable = {
    super.clone().asInstanceOf[Cloneable]
  }
}
trait Resetable {
  def reset: Unit
}

def cloneAndReset(obj: Cloneable with Resetable): Cloneable = {
  val cloned = obj.clone()
  obj.reset
  cloned
}

/* Explicity typed Self References */
// A Reaction/Solution to good ol' "what does 'this' refer to"

abstract class AbstractGraph {
  type Node <: NodeInterface
  type Edge

  trait NodeInterface {
    def connectTo(node: Node) : Edge
  }

  def nodes: List[Node]
  def edges: List[Edge]
  def addNode: Node
}

abstract class DirectedGraph extends AbstractGraph {
  type Edge <: EdgeImpl

  class EdgeImpl(from: Node, to: Node){
    def origin = from
    def dest = to
  }

  class NodeImpl extends NodeInterface {
    self: Node => //this makes 'this' references point to Node rather than NodeImpl
    override def connectTo(node: Node): Edge = {
      val edge = newEdge(this, node)
      edges = edge :: edges
      edge
    }
  }

  protected def newNode: Node
  protected def newEdge(impl: Node, node: Node) : Edge
  var nodes: List[Node] = Nil
  var edges: List[Edge] = Nil
  def addNode = {
    val node = newNode
    nodes = node :: nodes
    node
  }
}

//lets go concrete!!
class ConcreteGraph extends DirectedGraph {
  override type Edge = EdgeImpl

  override protected def newNode: Node = new NodeImpl

  override protected def newEdge(from: Node, to: Node): Edge = new EdgeImpl(from, to)

  override type Node = NodeImpl
}

val graph: AbstractGraph = new ConcreteGraph
val node1 = graph.addNode
val node2 = graph.addNode
val node3 = graph.addNode
node1.connectTo(node2)
node2.connectTo(node3)
node1.connectTo(node3)

/* Implicit parameters */

//Using Category theory!!
abstract class Semigroup[A] {
  def join(x: A, y: A) : A
}

abstract class Monoid[A] extends Semigroup[A] {
  def zero: A
}

implicit object StringMonoid extends Monoid[String] {
  override def zero: String = ""

  override def join(x: String, y: String): String = x concat y
}

implicit object IntMonoid extends Monoid[Int] {
  override def zero: Int = 0

  override def join(x: Int, y: Int): Int = x + y
}

def sum[A](xs: List[A])(implicit m: Monoid[A]): A = {
  if (xs.isEmpty) m.zero
  else m.join(xs.head, sum(xs.tail))
}

println(sum(List(1, 2, 3)))          // uses IntMonoid implicitly
println(sum(List("a", "b", "c")))    // uses StringMonoid implicitly

/* Readability */
// Any method which takes a single parameter can be used as an infix operator in Scala.

case class MyBool(x: Boolean) {
  def and(that: MyBool): MyBool = if (x) that else this
  def or(that: MyBool): MyBool = if (x) this else that
  def negate: MyBool = MyBool(!x)
}

def not(x: MyBool) = x.negate
def xor(x: MyBool, y: MyBool) = (x or y) and not(x and y)

/* Call by name examples / Automatic type-dependent closure construction */
// the following doesnt work in the worksheet, but works upon including it in a Scala object's main
/* class LoopUnlessCond(body: => Unit) {
  def unless(cond: => Boolean) {
    body
    if (!cond) unless(cond)
  }
}
def loopz(body: => Unit): LoopUnlessCond = new LoopUnlessCond(body)

var i = 10
loopz {
  println("i = " + i)
  i -= 1
} unless (i == 0) */

