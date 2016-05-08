/**
  * Created by hendrik.belitz on 03.05.2016.
  */
trait Stack[+A] {
  def push[B>:A](x:B) = NonEmptyStack(x, this)
  def pop:Stack[A]
  def top:A
  def isEmpty:Boolean
}

object EmptyStack extends Stack[Nothing] {
  override def pop: Stack[Nothing] = sys.error("Nope")
  override def top: Nothing = sys.error("Nope")
  override def isEmpty: Boolean = true
}

case class NonEmptyStack[+A](x:A,rest:Stack[A]) extends Stack[A] {
  override def pop: Stack[A] = rest
  override def top: A = x
  override def isEmpty: Boolean = true
}
