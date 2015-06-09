package normal


object App extends App {

//  implicit val ts: ThreadStrategy = OneThreadStrategy
  implicit val ts: ThreadStrategy = ThreadPoolStrategy

  val lMx = Matrix(Array(Array(1,2,3), Array(4,5,6)))
  val rMx = Matrix(Array(Array(1), Array(1), Array(1)))

  val resultMatrix =  MatrixUtils.multiply(lMx, rMx)
  println(s"result matrix of multiply: $resultMatrix")
}
