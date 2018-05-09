package gradefinder

object Main extends App {

  if (args.length != 4) {
    println("Usage: gradefinder <grades.csv> <boundaries.csv> <noise> <samples> ")
    System.exit(1)
  }

  val Array(gradesPath, boundariesPath, noiseStr, samplesStr) = args
  val noise = noiseStr.toDouble
  val samples = samplesStr.toInt

  def readCSV(path: String): List[List[String]] = {
    import com.github.tototoshi.csv.CSVReader
    import java.nio.file._
    val reader = CSVReader.open(Paths.get(path).toFile)
    val rows = reader.all()
    reader.close()
    rows
  }

  val (header :: data) = readCSV(gradesPath)
  val gradebook = data.map(_.map(_.toInt))

  val boundaries = readCSV(boundariesPath).tail.map {
    case List(min, max, letter) => Boundary(min.toInt, max.toInt, letter)
  }

  val model = new Model(header, gradebook,noise, boundaries, samples)
  model.infer()
}