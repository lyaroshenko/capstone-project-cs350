package lambda

import lambda.Term.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TermSpec extends AnyFlatSpec with Matchers:

  // freeVars
  "freeVars" should "return the variable itself" in {
    freeVars(Var("x")) shouldEqual Set("x")
  }

  it should "exclude bound variable" in {
    freeVars(Abs("x", Var("x"))) shouldEqual Set()
  }

  it should "include free variable inside abstraction" in {
    freeVars(Abs("x", Var("y"))) shouldEqual Set("y")
  }

  it should "union free vars of both sides in application" in {
    freeVars(App(Var("x"), Var("y"))) shouldEqual Set("x", "y")
  }

  it should "handle shadowed variable" in {
    // λx. λx. x — inner x shadows outer, no free vars
    freeVars(Abs("x", Abs("x", Var("x")))) shouldEqual Set()
  }

  // substitute
  "substitute" should "rule 1: replace matching variable" in {
    substitute(Var("x"), "x", Var("y")) shouldEqual Var("y")
  }

  it should "rule 2: leave non-matching variable" in {
    substitute(Var("a"), "x", Var("y")) shouldEqual Var("a")
  }

  it should "rule 3: recurse into application" in {
    substitute(App(Var("x"), Var("x")), "x", Var("y")) shouldEqual App(Var("y"), Var("y"))
  }

  it should "rule 4: skip abstraction that rebinds x" in {
    // [N/x](λx.x) = λx.x
    val term = Abs("x", Var("x"))
    substitute(term, "x", Var("y")) shouldEqual term
  }

  it should "rule 5: skip abstraction when x not free in body" in {
    // [N/x](λy.y) = λy.y — x doesn't appear in body
    val term = Abs("y", Var("y"))
    substitute(term, "x", Var("y")) shouldEqual term
  }

  it should "rule 6: substitute inside abstraction when safe" in {
    // [y/x](λz.x) = λz.y — z not free in y, safe
    substitute(Abs("z", Var("x")), "x", Var("y")) shouldEqual Abs("z", Var("y"))
  }

  it should "rule 7: alpha-convert to avoid capture" in {
    // [y/x](λy.x) — naive sub would give λy.y (capture!)
    // must rename y to fresh var z: λz.y
    val result = substitute(Abs("y", Var("x")), "x", Var("y"))
    result match
      case Abs(param, body) =>
        param should not equal "y" // renamed
        body shouldEqual Var("y")  // substitution happened
      case _ => fail("expected Abs")
  }

  // betaReduce
  "betaReduce" should "apply identity function" in {
    // (λx.x) y → y
    betaReduce(App(Abs("x", Var("x")), Var("y"))) shouldEqual Var("y")
  }

  it should "apply const function" in {
    // (λx.λy.x) a → λy.a
    betaReduce(App(Abs("x", Abs("y", Var("x"))), Var("a"))) shouldEqual Abs("y", Var("a"))
  }

  it should "return unchanged if not a redex" in {
    val t = App(Var("f"), Var("x"))
    betaReduce(t) shouldEqual t
  }
