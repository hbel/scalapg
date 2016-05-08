/**
  * Created by hendrik.belitz on 02.05.2016.
  */
object Sorting {
  def quickSort(xs:Array[Int]):Array[Int] = {
    if ( xs.length < 1 ) xs
    else {
      val pivot = xs(xs.length/2)
      Array.concat(
        quickSort( xs filter (pivot > _)),
        xs filter (pivot == _),
        quickSort( xs filter (pivot < _)))
    }
  }
}
