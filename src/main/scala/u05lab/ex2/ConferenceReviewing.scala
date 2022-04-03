package u05lab.ex2

trait ConferenceReviewing:

  def loadReview(article: Int, scores: Map[Int, Int]): Unit

  def orderedScores(article: Int, relevance: Int): Unit
