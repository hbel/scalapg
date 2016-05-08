def msort[A<%Ordered[A]](l:List[A]):List[A] = {
  def merge(a:List[A],b:List[A]): List[A] = {
    if ( a.isEmpty ) b
    else if ( b.isEmpty ) a
    else if ( a.head < b.head ) a.head :: merge(a.tail,b)
    else b.head :: merge(a,b.tail)
  }
  l match {
    case x :: Nil => x :: Nil
    case xs: List[A] => {
      val len = xs.length / 2
      if (len==0) xs
      else merge(msort(xs.take(len)), msort(xs.drop(len)))
    }
  }
}

def bfind(l:List[Int],x:Int):Boolean = {
  val pivot = l.length/2
  //println(pivot,l(pivot),l)
  if ( l(pivot) == x ) true
  else {
    if ( l.length <= 1 ) false
    else if ( l(pivot) > x ) bfind(l.take(pivot),x)
    else bfind(l.drop(pivot),x)
  }
}

def squareList(xs:List[Int]):List[Int] = xs match {
  case List() => Nil
  case y :: ys => y*y :: squareList(ys)
}

def squareList2(xs:List[Int]):List[Int] = xs map( x => x*x )

def posElems(xs:List[Int]):List[Int] = xs match {
  case List() => Nil
  case y :: ys => if ( y > 0 ) y :: posElems(ys) else posElems(ys)
}

def forAll(xs:List[Int],f:Int => Boolean):Boolean = xs.filter(f).length == xs.length
def exists(xs:List[Int],f:Int => Boolean):Boolean = xs.filter(f) != Nil

def rl2(op:(Int,Int)=>Int)(xs:List[Int]):Int = xs match {
  case y :: Nil => y
  case y :: ys => op(y, rl(op)(ys))
  case _ => sys.error("")
}

def rl(op:(Int,Int)=>Int)(xs:List[Int]):Int = xs match {
  case Nil => sys.error("Nope")
  case x :: xs => fl(x)(op)(xs)
}

def sum = rl((x:Int,y:Int)=>x+y)_
def product(xs:List[Int]):Int = rl {(x,y)=>x*y}(1::xs)

def fl(acc:Int)(op:(Int,Int)=>Int)(xs:List[Int]) : Int= xs match {
  case Nil => acc
  case x :: xs => fl(op(acc,x))(op)(xs)
}

val sl = msort(List(1,2,3,4))
val s2 = squareList(sl)
val s3 = squareList2(sl)
val s4 = posElems(sl)
val s51 = forAll(s4,_>0)
val s5 = forAll(sl,_>0)
val s6 = exists(sl,_<0)
val s61 = exists(s4,_<0)
val s7 = rl{(x,y)=>x+y}(sl)
val s8 = sum(sl)
val s9 = product(sl)
val s10 = sl.foldLeft(1)((x,y)=>x*y)
val s11 = (1 /: sl){(x,y)=>x*y}
val len = (0 /: sl){(x,y)=>x+1}
val map = (sl :\ List[Int]()){(x,y)=>2*x :: y}

val persons = List(("Hendrik",40),("Laura",17),("Franzi",36))
val names = for(p<-persons if p._2 > 20 ) yield p._1
names.foreach( x => println(x))
val dings = for{ i <- List.range(1,10)
     j <- List.range(0,i)
     if i+j > 15 } yield (i,j)