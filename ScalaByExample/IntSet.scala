/**
  * Created by hendrik.belitz on 02.05.2016.
  */
trait IntSet {
  def incl(x:Int):IntSet
  def excl(x:Int):IntSet
  def incl(s:IntSet):IntSet
  def contains(x:Int):Boolean
  def union(s:IntSet):IntSet
  def intersection(s:IntSet):IntSet
  def isEmpty:Boolean
}

object EmptySet extends IntSet {
  def incl(x:Int):IntSet = new NonEmptySet(x,EmptySet,EmptySet)
  def excl(x:Int):IntSet = this
  def incl(s:IntSet):IntSet = s
  def contains(x:Int):Boolean = false
  def union(s:IntSet):IntSet = s
  def intersection(s:IntSet):IntSet = this
  def isEmpty:Boolean = true
  override def toString = "";
}

case class NonEmptySet(value:Int,left:IntSet,right:IntSet) extends IntSet {
  def isEmpty:Boolean = false

  def incl(x:Int):IntSet = {
    if ( x < value ) new NonEmptySet( value, left.incl(x), right )
    else if (x > value) new NonEmptySet( value, left, right.incl(x) )
    else this
  }

  def excl(x:Int):IntSet =  {
    if ( x == value ) left.incl(right)
    else new NonEmptySet(value,left.excl(x),right.excl(x))
  }

  def incl(s:IntSet):IntSet = {
    s match {
      case EmptySet => this
      case n: NonEmptySet => this.incl(n.value).incl(n.left).incl(n.right)
    }
  }

  def contains(x:Int):Boolean = {
    if ( x < value ) left.contains(x)
    else if ( x > value ) right.contains(x)
    else true
  }

  def union(s:IntSet):IntSet = s match {
    case EmptySet => this
    case n : NonEmptySet => this.incl(n)
  }

  def intersection(s:IntSet):IntSet = s match {
    case EmptySet => EmptySet
    case n : NonEmptySet => {
      val result = left.intersection(s).incl( right.intersection(s) )
      if ( s.contains(value)) result.incl(value)
      else result
    }
  }
  override def toString = left.toString + " "+value+" " + right.toString;
}
