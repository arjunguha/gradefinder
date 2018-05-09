package gradefinder

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.library.atomic.continuous
import com.cra.figaro.algorithm.sampling.{MetropolisHastingsAnnealer, ProposalScheme, Schedule}
import com.cra.figaro.experimental.normalproposals.NormalProposer

case class Boundary(min: Int, max: Int, next: String) {
  require(min < max)
  require(min > 0)
  require(max < 100)
}

object Helpers {

  def norm(arr: Array[Double]): Unit = {
    val sum = arr.sum
    for (i <- 0.until(arr.length)) {
      arr(i) = arr(i) / sum
    }
  }

  def makeWeights(noise: Double, len: Int): Element[Array[Double]] = {

    def reweight(perWeightNoise: Array[Double]): Array[Double] = {
      val result = Array.fill(len)(1 / len.toDouble)
      for (i <- 0.until(len)) {
        result(i) = result(i) + noise * perWeightNoise(i)
      }
      norm(result)
      result
    }
    val perWeightNoise = continuous.Dirichlet(Array.fill(len)(1/len.toDouble): _*)
    Apply(perWeightNoise, reweight)
  }
}

class Model(assignments: Seq[String],
            gradebook: Seq[Seq[Int]],
            weightNoise: Double,
            boundaries: Seq[Boundary]) {

  val weights = Helpers.makeWeights(weightNoise, assignments.length)

  val bounds: Seq[Element[Int]] =
    boundaries.map(b => discrete.Uniform(b.min.to(b.max).by(1): _*))


  def calcGrade(entry: Seq[Int]): Element[Double] = {
    Apply(weights, (weightsV: Array[Double]) =>
      weightsV.zip(entry).map({ case (w,v) => w * v }).sum)
  }


  def isStable(bound: Int, grade: Double): Boolean = {
    grade >= bound || (grade < bound && grade + 1 < bound)
  }


  for (Seq(lo, hi) <- bounds.sliding(2)) {
    // There must be a 2% gap between each letter boundary
    Apply(lo, hi, (x: Int, y: Int) => x + 2 < y)
      .observe(true)
  }

  for (entry <- gradebook) {
    val grade = calcGrade(entry)
    for (bound <- bounds) {
      Apply(bound, grade, isStable).setConstraint(b => if (b) 1 else 0.000001)
    }
  }

  def evaluate(algorithm: MetropolisHastingsAnnealer): Unit = {
    val bestWeights = algorithm.mostLikelyValue(weights)
      .map(x => math.round(x * 1000) / 1000.0)
    val bestBounds = bounds.map(b => algorithm.mostLikelyValue(b))

    val totals = gradebook.map(entry => entry.zip(bestWeights)
      .map({ case (x,y) => x * y}).sum)

    println("-- Grade cutoffs ---")

    val letters = "F" :: boundaries.map(_.next).toList
    val ranges = (0 +: bestBounds.toList :+ 100).sliding(2).toList

    for ((List(lo, hi), letter) <- ranges.zip(letters)) {
      val n = totals.filter(x => x >= lo && x < hi).length
      println(s"$letter [$lo, $hi) -- $n students")
    }

    println("--- Weights ---")
    for ((best, assignment) <- bestWeights.zip(assignments)) {
      println(s"$assignment: $best")
    }

    val badness = totals.map(total => bestBounds.map(bound => isStable(bound, total))).flatten.filter(x => !x).length
    println("-- Solution quality ---")
    println(s"Number of students whose letter grades will change if they receive 1 more point: $badness")
  }

  def infer() = {
    val algorithm = MetropolisHastingsAnnealer(
      scheme = ProposalScheme.default,
      annealSchedule = Schedule.default(1.0))
    algorithm.start()

    while (true) {
      Thread.sleep(1000 * 15)
      println("*** Solution with  " + algorithm.getSampleCount + " samples. ***")
      evaluate(algorithm)
    }

  }

}



