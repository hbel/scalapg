//val arr = Array(5,32986,239761,29011,3,-3,0,326)
//Sorting.quickSort(arr)
//Numerics.sqrt(11.0)
//Numerics.factorial(5)

def sum(a:Int,b:Int):Int = if ( a == b ) 0 else a+sum(a+1,b)

def sum(f:Int=>Int,a:Int,b:Int):Int = if ( a == b ) 0 else f(a)+sum(f,a+1,b)
def sum2(f:Int=>Int)(a:Int,b:Int):Int = sum(f,a,b)
def sum3(f:Int=>Int,a:Int,b:Int):Int = {
  def iter(f:Int=>Int,a:Int,b:Int,acc:Int):Int =
    if ( a == b ) acc else iter(f,a+1,b,acc+f(a))
  iter(f,a,b,0)
}
def sum4(f:Int=>Int)(a:Int,b:Int):Int = sum3(f,a,b)

def general(g:(Int,Int)=>Int,one:Int)(f:Int=>Int)(a:Int,b:Int):Int = {
  def iter(f:Int=>Int,a:Int,b:Int,acc:Int):Int =
    if ( a == b ) acc else iter(f,a+1,b,g(acc,f(a)))
  iter(f,a,b,one)
}

def gsum = general(_+_,0)_
def gproduct = general(_*_,1)_

def gsumid = gsum(x=>x)
def factorial(a:Int) = gproduct(x=>x)(1,a+1)
gsumid(1,6)
factorial(4)




