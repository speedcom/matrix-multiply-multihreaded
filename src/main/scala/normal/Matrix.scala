package normal


case class Matrix(private val rep: Array[Array[Int]]) {

  def row(i: Int): Array[Int] = rep(i)
  def col(j: Int): Array[Int] = rep.foldLeft(Array.empty[Int])((buf, row) => buf :+ row(j))

  def rowSize = rep.size
  def colSize = rep(0).size

  override def toString: String = "Macierz" + rep.foldLeft("") {
    (msg, row) => msg + row.mkString("\n|", " | ", "|")
  }
}