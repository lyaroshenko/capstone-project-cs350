package lambda

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TermSpec extends AnyFlatSpec with Matchers:

  "freeVars" should "return the variable itself" in:
    Term.freeVars(Var("x")) shouldEqual Set("x")

  it should "exclude bound variable" in:
    Term.freeVars(Abs("x", Var("x"))) shouldEqual Set()

  it should "include free variable inside abstraction" in:
    Term.freeVars(Abs("x", Var("y"))) shouldEqual Set("y")

  it should "union free vars of both sides in application" in:
    Term.freeVars(App(Var("x"), Var("y"))) shouldEqual Set("x", "y")

  it should "handle shadowed variable" in:
    Term.freeVars(Abs("x", Abs("x", Var("x")))) shouldEqual Set()

  "substitute" should "rule 1: replace matching variable" in:
    Term.substitute(Var("x"), "x", Var("y")) shouldEqual Var("y")

  it should "rule 2: leave non-matching variable" in:
    Term.substitute(Var("a"), "x", Var("y")) shouldEqual Var("a")

  it should "rule 3: recurse into application" in:
    Term.substitute(App(Var("x"), Var("x")), "x", Var("y")) shouldEqual App(Var("y"), Var("y"))

  it should "rule 4: skip abstraction that rebinds x" in:
    val term = Abs("x", Var("x"))
    Term.substitute(term, "x", Var("y")) shouldEqual term

  it should "rule 5: skip abstraction when x not free in body" in:
    val term = Abs("y", Var("y"))
    Term.substitute(term, "x", Var("y")) shouldEqual term

  it should "rule 6: substitute inside abstraction when safe" in:
    Term.substitute(Abs("z", Var("x")), "x", Var("y")) shouldEqual Abs("z", Var("y"))

  it should "rule 7: alpha-convert to avoid capture" in:
    // [y/x](λy.x) — naive result would be λy.y (y captured).
    // Correct: rename bound y to fresh z, giving λz.y
    val result = Term.substitute(Abs("y", Var("x")), "x", Var("y"))
    result match
      case Abs(param, body) =>
        param should not equal "y"
        body shouldEqual Var("y")
      case _ => fail("expected Abs")

  "betaReduce" should "apply identity function" in:
    Term.betaReduce(App(Abs("x", Var("x")), Var("y"))) shouldEqual Var("y")

  it should "apply const function" in:
    Term.betaReduce(App(Abs("x", Abs("y", Var("x"))), Var("a"))) shouldEqual Abs("y", Var("a"))

  it should "return unchanged if not a redex" in:
    val t = App(Var("f"), Var("x"))
    Term.betaReduce(t) shouldEqual t
