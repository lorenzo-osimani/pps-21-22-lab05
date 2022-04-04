package u05lab.ex2

import java.util.stream.Collectors

trait ConferenceReviewing:

  enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(article: Int, question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def acceptedArticles: Set[Int]

  def sortedAcceptedArticles: List[(Int, Double)]

  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

class ConferenceReviewingImpl extends ConferenceReviewing:

  import scala.collection.mutable.ListBuffer
  import Question.*

  val reviews = ListBuffer[(Int, Map[Question, Int])]()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then throw IllegalArgumentException()
    reviews += (article -> scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val scores = Map((RELEVANCE -> relevance), (SIGNIFICANCE -> significance), (CONFIDENCE -> confidence), (FINAL -> fin))
    loadReview(article, scores)

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(_._1 == article).map(_._2.get(question).get).sorted.toList

  override def averageFinalScore(article: Int): Double =
    val usefulReviews = reviews.filter(_._1 == article)
    usefulReviews.map(_._2.get(FINAL).get).sum / usefulReviews.length

  private def accepted(article: Int): Boolean =
    averageFinalScore(article) > 5 && reviews.filter(_._1 == article).flatMap(_._2).filter(x => x._1 == RELEVANCE && x._2 > 8).nonEmpty

  override def acceptedArticles: Set[Int] =
    reviews.map(_._1).distinct.filter(accepted(_)).toSet

  override def sortedAcceptedArticles: List[(Int, Double)] =
    this.acceptedArticles.map(r => (r, this.averageFinalScore(r)))
      .toList.sorted((x, y) => x._2.compareTo(y._2))

  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    reviews.map(_._1).distinct.map(r => (r, this.averageFinalScore(r))).toMap