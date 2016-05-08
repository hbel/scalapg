type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): Set = x => x == elem
def union(s: Set, t: Set): Set = x => s(x) || t(x)
def intersect(s: Set, t: Set): Set = x => s(x) && t(x)
def diff(s: Set, t: Set): Set = x => s(x) && !t(x)
def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

var one = singletonSet(1)
var two = singletonSet(2)
var three = singletonSet(3)
contains(one,1)
contains(one,2)
contains( union(one,two),1)
contains( union(one,two),2)
contains( intersect(one,two),1)
contains( intersect(one,two),2)
contains( diff(one,two),1)
contains( diff(one,one),1)
contains(filter(union(one,two), _ > 1 ),1)
contains(filter(union(one,two), _ > 1 ),2)

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a>1000) true
    else if (contains(s,a) && !p(a)) false
    else iter(a+1)
  }
  iter(-1000)
}

val mySet = union(union(one,two),three)
contains(mySet,1)
contains(mySet,2)
contains(mySet,3)
forall(mySet, _>0)
forall(mySet, _%2==0)

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))
exists(mySet, _>0)
exists(mySet, _%2==0)
exists(mySet, _==4)

def map(s:Set,f:Int=>Int):Option[Set] = {
  def iter(a: Int, xs :Option[Set]): Option[Set] = {
    if (a>1000) xs
    else if (contains(s,a)) xs match {
      case Some(set) => iter (a+1,Some(union(set,singletonSet(f(a)))))
      case None => iter (a+1,Some(singletonSet(f(a))))
    }
    else iter(a+1,xs)
  }
  iter(-1000,None)
}

val newSet = map(mySet,_*2).get
contains(newSet,2)
contains(newSet,4)
contains(newSet,6)
contains(newSet,1)