import scala.math.min
import scala.io.Source

object arrays {

  /*
     getIntsAsString
   For example: If the Array contains 1, 2, 3, 4, 5
     printInts("my array ", Array(1, 2, 3, 4, 5), " : ") gives
     myarray 1:2:3:4:5
  */

  def arraySize(a: Array[Int]): Int = {
    a.length
  }

  def getIntsAsString(label: String, delimiter: String, a: Array[Int]): String = {
     var x = a.mkString(delimiter)
    (label + x) 
      
  }

  // Read the contents of filename into a.
  // You should only read as many lines as the array can hold (a.length)
  // Each line should be converted to Int (if possible) or 0 otherwise.

  def readFileIntoArray(filename: String, a: Array[Int]) {
var x = 0
      for (line <- Source.fromFile(filename).getLines())
{  if (arraySize(a)!= x) { 
    a(x) = line.toInt
    x+=1}}
      
  }

  //Minimum chunk
  ///  Return the minimum value in a.
  ///  Example: If a contains {5, 7, 4, 9}, return 4. 
  ///  Assume a contains at least one value.

  def minimum(a: Array[Int]): Int = {
    require(a.length > 0) 
    val x = a.min
    x; 
  }
  //CountEven chunk
  ///  Return the number of even values in a.
  ///  Example: If a contains {-4, 7, 6, 12, 9}, return 3. 
  def countEven(a: Array[Int]): Int = {
    
      val x = a.count(_%2 == 0)
      x;   
  }

  //CountEven chunk
  ///  Return the number of even values in a.
  ///  Example: If a contains {-4, 7, 6, 12, 9}, return 3. 

  def countOdd(a: Array[Int]): Int = {
    val x = a.count(_%2 != 0)
    x;
  }

  //PairwiseAdd chunk
  ///  Add corresponding elements of a and b and place them in sum.
  ///  Assume all arrays have the same Length.
  ///  Example: If a contains {2, 4, 6} and b contains {7, -1, 8}
  ///  then at the end sum should contain {9, 3, 14}. 

  def pairwiseAdd(a: Array[Int], b: Array[Int], c: Array[Int])= {
   
      for (i <- 0 until (min(a.length, b.length)))
      c(i) = (a(i) + b(i))
        c
      
      
  }
  //NewPairwiseAdd chunk
  ///  Return a new array whose elements are the sums of the
  ///  corresponding elements of a and b.
  ///  Assume a and b have the same Length.
  ///  Example: If a contains {2, 4, 6} and b contains {3, -1, 5}
  ///  then return an array containing {5, 3, 11}. 
  def newPairwiseAdd(a: Array[Int], b: Array[Int]): Array[Int] = {
    val addSize = min(a.length, b.length)
    var newArray = Array.fill(addSize)(0)

   for (i <- 0 until (min(a.length, b.length)))
      newArray(i) = (a(i) + b(i))
    newArray 
   }
  //IsAscending chunk
  ///  Return true if the numbers are sorted in increasing order,
  ///  so that in each pair of consecutive entries,
  ///  the second is always at least as large as the first.
  ///  Return false otherwise.  Assume an array with fewer than
  ///  two elements is ascending.
  ///  Examples: If a contains {2, 5, 5, 8}, return true;
  ///  if a contains {2, 5, 3, 8}, return false. 
  def isAscending(a: Array[Int]):Boolean = {
     var increasing = true
     for (i <- 0 to (a.length-2))
          if (a(i) > a(i+1))
            increasing = false
     increasing
  } 

  /*
     getAscendingRun(a, position) returns the position where a 
     run (of ascending values) ends. If a run ends at the end of
     the array, the array's length is returned. This function is 
     designed to be called over and over until there are no more 
     runs.

    example:

    If you ahve an array of data:
    val data = Array(2, 5, 8, 3, 9, 9, 8)

    getAscendingRun(data, 0) returns 3 (since 3 < 8)
      run is 2, 5, 8
    getAscendingRun(data, 3) returns 6 (since 8 < 9)
      run is 3, 9, 9
    getAscendingRun(data, 6) returns 7 (since 8 is the last item in the list)
      run is 8

  */

 def getAscendingRun(a: Array[Int], position: Int): Int = {
    require(position < a.length)
     // USE A WHILE LOOP
   var answer = a.length
     var iffound = "not found"
     for (i <- position to (a.length-2)){
         while (iffound != "found"){
          if (a(i) > a(i+1)){
           answer = (i + 1)
              iffound = "found"}}}
          
    answer
  }

  /*
    This should use teh getAscendingRun() function to produce a string
    of runs. The runs should be separated by commas with a vertical bar
    between each run. In the above:

    2, 5, 8 | 3, 9, 9 | 8
  */

  def getRunsAsString(a: Array[Int]): String = {
    ""
  }
  // end PrintRuns chunk   
}
