def range(start:Int,end:Int):Stream[Int] = {
  if ( start == end ) Stream.Empty
  else Stream.cons( start, range(start+1,end))
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

time { val x = for (x <- range(1,10000000) if x < 10 ) yield x }
time { val y = for (x <- List.range(1,10000000) if x < 10 ) yield x }