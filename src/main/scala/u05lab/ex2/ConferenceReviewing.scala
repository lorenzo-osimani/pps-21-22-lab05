package u05lab.ex2

import java.util.stream.Collectors

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:

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

  import Question.*

  private var reviews = Map[Int, List[Map[Question, Int]]]()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then throw IllegalArgumentException()
    reviews = reviews + (article -> (scores :: reviews.getOrElse(article, List())))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Map((RELEVANCE -> relevance), (SIGNIFICANCE -> significance), (CONFIDENCE -> confidence), (FINAL -> fin)))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews(article).map(_(question)).sorted

  override def averageFinalScore(article: Int): Double =
    mean(reviews(article).map(_(FINAL)))

  private def accepted(article: Int): Boolean =
    averageFinalScore(article) >= 5 && reviews(article).filter(_(RELEVANCE) >= 8).nonEmpty

  override def acceptedArticles: Set[Int] =
    reviews.keySet.filter(accepted(_))

  override def sortedAcceptedArticles: List[(Int, Double)] =
    acceptedArticles.map(r => (r, this.averageFinalScore(r)))
      .toList.sorted((x, y) => x._2.compareTo(y._2))

  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    reviews.keySet.map(article => (article, mean(reviews(article).map(r => r(CONFIDENCE)*r(FINAL)/10.0)))).toMap

  private def mean(list: List[Double]): Double = list.sum / list.size