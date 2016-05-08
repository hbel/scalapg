/**
  * Created by hendrik.belitz on 03.05.2016.
  */
trait Integer{
  def successor: Integer
  def +(that: Integer): Integer = that match {
    case ZeroInt => this
    case n : Positive => this.successor + n.predecessor
    case n : Negative => this.predecessor + n.successor
  }
  def -(that: Integer): Integer = that match {
    case ZeroInt => this
    case n : Positive => this.predecessor - n.predecessor
    case n : Negative => this.successor - n.successor
  }
  def predecessor: Integer
  def negate: Integer
}

object ZeroInt extends Integer {
  override def negate: Integer = ZeroInt
  override def successor: Integer = Positive(ZeroInt)
  override def +(that: Integer): Integer = that
  override def predecessor: Integer = Negative(ZeroInt)
  override def toString = "0"
}

case class Positive(pred:Integer) extends Integer {
  override def successor: Integer = Positive(this)
  override def negate: Integer = ZeroInt-this
  override def predecessor: Integer = pred
  override def toString = {
    def iter(x:Integer,acc:Int):Int = x match {
      case ZeroInt => acc
      case n : Integer => iter(n.predecessor,acc+1)
    }
    iter(this,0).toString
  }
}

case class Negative(succ:Integer) extends Integer {
  override def successor: Integer = succ
  override def negate: Integer = ZeroInt-this
  override def predecessor: Integer = Negative(this)
  override def toString = {
    def iter(x:Integer,acc:Int):Int = x match {
      case ZeroInt => acc
      case n : Integer => iter(n.successor,acc-1)
    }
    iter(this,0).toString
  }
}