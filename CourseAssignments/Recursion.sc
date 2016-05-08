def pascal(c:Int,r:Int) : Int = {
  if ( c>r || c < 0 || r < 0 ) sys.error("Column must be no larger than row and row must be at least zero.")
  if ( r == 0 || r == 1 ) 1
  else if ( c == 0 || c == r ) 1
  else pascal(c-1,r-1) + pascal(c,r-1)
}

pascal(2,3)
pascal(2,4)
pascal(3,4)
pascal(3,5)

def balance(chars: String): Boolean = {
  def iter(c:List[Char],count:Int):Int = c match {
    case Nil => count
    case x :: xs => {
        x match {
          case '(' => iter(xs,count+1)
          case ')' => if ( count == 0 ) -1 else iter(xs,count-1)
          case _ => iter(xs,count)
        }
      }
  }
  iter(chars.toList,0) == 0
}

balance("((")
balance("()")
balance("(())")
balance(")(")
balance("(if (zero? x) max (/ 1 x))")
balance("I told him (that it’s not (yet) done). (But he wasn’t listening)")
balance(":-)")
balance("())(")

def countChange(money:Int,coins:List[Int]):Int = {
  def iter(m:Int,i:Int,c:List[Int]):Int = {
    if ( m - i * c.head < 0 ) 0
    else {
      c match {
        case Nil => 0
        case x :: Nil => if (m % x != 0) 0 else 1
        case x :: xs => {
          if (m - i * x == 0) 1
          else iter(m, i + 1, c) + iter(m - i * x, 0, xs)
        }
      }
    }
  }
  if ( coins == Nil || money < 1 ) 0
  else iter(money,0,coins.sorted)
}

countChange(0,List(1,2))
countChange(10,List())
countChange(1,List(1,2))
countChange(2,List(1,2))
countChange(4,List(1,2))
countChange(6,List(1,2))
countChange(8,List(1,2))
countChange(10,List(1,2,3))
countChange(20,List(1,2,5,10))