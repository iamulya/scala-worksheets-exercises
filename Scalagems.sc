/* if else ternary operator */
implicit class Question[T](predicate: => Boolean) {
  def ?(left: => T) = predicate -> left
}
implicit class Colon[R](right: => R) {
  def ::[L <% R](pair: (Boolean, L)): R = if (pair._1) pair._2 else right
}
val x = (5 % 2 == 0) ? 5 :: 4.5