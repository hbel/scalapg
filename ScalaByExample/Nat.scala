/**
  * Created by hendrik.belitz on 02.05.2016.
  */
trait Nat {
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def predecessor: Nat = sys.error("Zero has no predecessor")
  def successor: Nat = OtherNat(Zero)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = that match {
    case Zero => Zero
    case n:Nat => sys.error("Cannot subtract from zero")
  }
  override def toString = "0"
}

case class OtherNat(pred:Nat) extends Nat {
  def predecessor: Nat = pred
  def successor: Nat = OtherNat(this)
  def + (that: Nat): Nat = that match {
    case Zero => this
    case n : Nat => this.successor + n.predecessor
  }
  def - (that: Nat): Nat = that match {
    case Zero => this
    case n : Nat => this.predecessor - n.predecessor
  }
  override def toString = {
    def iter(x:Nat,acc:Int):Int = x match {
      case Zero => acc
      case n : Nat => iter(n.predecessor,acc+1)
    }
    iter(this,0).toString
  }
}
