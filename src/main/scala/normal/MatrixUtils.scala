package normal


object MatrixUtils {
  def multiply(lMx: Matrix, rMx: Matrix)(implicit threading: ThreadStrategy): Matrix = {
    assert(lMx.colSize == rMx.rowSize)

    def initBufor: Array[Array[Int]] = {
      val buffer = new Array[Array[Int]](lMx.rowSize)
      (0 until buffer.length).foreach { i =>
        buffer(i) = new Array[Int](rMx.colSize)
      }
      buffer
    }

    def computeValue(i: Int, j: Int): Int = {
      val row = lMx.row(i)
      val col: Array[Int] = rMx.col(j)
      row.zip(col).map { case (r,c) => r*c }.sum
    }

    // TEMP BUFOR
    val tempBufor = initBufor

    val computations = for {
      i <- 0 until lMx.rowSize
      j <- 0 until rMx.colSize
    } yield {
      threading.execute { () => { tempBufor(i)(j) = computeValue(i,j) }}
    }

    computations.foreach(_())

    Matrix(tempBufor)
  }
}
