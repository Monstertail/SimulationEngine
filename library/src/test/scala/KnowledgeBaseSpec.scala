import library.EpistemicLogic.Sentence._
import library.EpistemicLogic.Solver._
import library.EpistemicLogic.KnowledgeBase

import org.scalatest.FlatSpec

class KnowledgeBaseSpec extends FlatSpec {

  val p1: EpistemicSentence = P("Sun rises from the East")
  val p2: EpistemicSentence = P("Earth is flat")
  val p3: EpistemicSentence = NotE(p2)

  "No constraint" should "return a knowledgeBase without consistency guarantee" in {
    val kb: KnowledgeBase = new KnowledgeBase()
    kb.learn(Set(p1, p3))
    kb.learn(Set(NotE(p1), p2))

    assert(kb.know(NotE(p1)))
    assert(kb.know(p1))
    assert(kb.know(p2))
//    assert(kb.know(AndE(p1, NotE(p1))))
  }

  "Default constraint" should "return a consistent knowledgeBase" in {
    val kb: KnowledgeBase = new KnowledgeBase()
    kb.default()

    kb.learn(Set(p1, p3))
    kb.learn(Set(NotE(p1), p2))

    assert(!kb.know(NotE(p1)))
    assert(!kb.know(p2))
  }

  "knowledgeAboutAnother" should "return the known propositions about another agent" in {
    val e1: EpistemicSentence = Ka(1, P("Time 5"))
    val e2: EpistemicSentence = Ka(1, P("Time 6"))

    val kb: KnowledgeBase = new KnowledgeBase()
    kb.default()
    kb.learn(Set(e1, e2))

    assert(kb.knowledgeAboutAnother(1).size == 2)
  }

  "Add new constraint" should "apply the new constraints over the knowledge base" in {

    val e1: EpistemicSentence = Ka(1, P("Time 5 unit"))
    val e2: EpistemicSentence = Ka(1, P("Time 6 unit"))
    val e3: EpistemicSentence = Ka(5, P("Time 5 unit"))
    val e4: EpistemicSentence = Ka(5, P("Time 7 unit"))

    val kb: KnowledgeBase = new KnowledgeBase()
    kb.default()

    def getTime(s: String): Int = {
      val prefix: String = "Time "
      val posfix: String = " unit"
      s.slice(s.indexOf(prefix) + prefix.length, s.indexOf(posfix)).toInt
    }

    kb.addConstraints(x => {
      x match {
        case Ka(i, p) => kb.knowledgeAboutAnother(i).toList match {
          case Nil => true
          case y :: l =>
            getTime(y.toString) < getTime(x.toString)
        }
        case _ => true
      }
    })

    assert(kb.constraints.length == 2)

    kb.learn(Set(e2))
    kb.learn(Set(e1))   // should not modify the state of the knowledge

    kb.learn(Set(e3, e4))

    assert(!kb.know(e1))
    assert(kb.know(e3))
    assert(kb.know(e4))
  }
}
