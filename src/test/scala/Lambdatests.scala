package lambda

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// ─────────────────────────────────────────────
// Term helpers
// ─────────────────────────────────────────────
class TermFreeVarsSpec extends AnyFlatSpec with Matchers:

  "freeVars" should "return the variable name for a bare Var" in:
    Term.freeVars(Var("x")) shouldBe Set("x")

  it should "return an empty set for a closed abstraction" in:
    // λx.x  – x is bound
    Term.freeVars(Abs("x", Var("x"))) shouldBe empty

  it should "return the free variable in an abstraction whose body mentions another var" in:
    // λx.y  – y is free
    Term.freeVars(Abs("x", Var("y"))) shouldBe Set("y")

  it should "union free vars from both sides of an App" in:
    // (x y)
    Term.freeVars(App(Var("x"), Var("y"))) shouldBe Set("x", "y")

  it should "not include bound variables in nested abstractions" in:
    // λx.(λy.x)  – both x and y are bound
    Term.freeVars(Abs("x", Abs("y", Var("x")))) shouldBe empty

  it should "correctly handle the identity applied to a free var" in:
    // (λx.x) y  – only y is free
    Term.freeVars(App(Abs("x", Var("x")), Var("y"))) shouldBe Set("y")

  it should "handle multiple free variables" in:
    // λx.(y z)  – y and z are free
    Term.freeVars(Abs("x", App(Var("y"), Var("z")))) shouldBe Set("y", "z")


class TermFreshVarSpec extends AnyFlatSpec with Matchers:

  "freshVar" should "return z when the avoid set is empty" in:
    Term.freshVar(Set.empty) shouldBe "z"

  it should "return z0 when z is taken" in:
    Term.freshVar(Set("z")) shouldBe "z0"

  it should "return z1 when z and z0 are taken" in:
    Term.freshVar(Set("z", "z0")) shouldBe "z1"

  it should "skip an arbitrary number of taken names" in:
    val taken = Set("z") ++ (0 to 9).map(i => s"z$i").toSet
    Term.freshVar(taken) shouldBe "z10"

  it should "not return a name in the avoid set" in:
    val avoid = Set("z", "z0", "z1", "z2")
    avoid should not contain Term.freshVar(avoid)


class TermSubstituteSpec extends AnyFlatSpec with Matchers:

  // shorthand
  private def sub(t: Term, x: String, n: Term): Term = Term.substitute(t, x, n)

  "substitute" should "replace a matching Var" in:
    sub(Var("x"), "x", Var("y")) shouldBe Var("y")

  it should "leave a non-matching Var unchanged" in:
    sub(Var("z"), "x", Var("y")) shouldBe Var("z")

  it should "substitute inside App" in:
    sub(App(Var("x"), Var("x")), "x", Var("a")) shouldBe App(Var("a"), Var("a"))

  it should "not substitute inside an Abs that rebinds the variable" in:
    // substitute x->a in λx.x  → λx.x  (x is rebound)
    sub(Abs("x", Var("x")), "x", Var("a")) shouldBe Abs("x", Var("x"))

  it should "substitute inside an Abs whose param differs and x is free" in:
    // substitute x->a in λy.x  → λy.a
    sub(Abs("y", Var("x")), "x", Var("a")) shouldBe Abs("y", Var("a"))

  it should "leave an Abs unchanged when x is not free in its body" in:
    // substitute x->a in λy.y  → λy.y  (x not free in body)
    sub(Abs("y", Var("y")), "x", Var("a")) shouldBe Abs("y", Var("y"))

  it should "alpha-rename to avoid capture" in:
    // substitute x -> y in λy.x  – naive result would be λy.y (wrong!)
    // correct result uses a fresh variable for y
    val result = sub(Abs("y", Var("x")), "x", Var("y"))
    // The bound name must not be y (would capture the free y)
    result match
      case Abs(param, body) =>
        param should not be "y"
        Term.freeVars(result) shouldBe Set("y") // y still appears free inside
      case _ => fail("Expected an Abs")

  it should "handle nested substitution without variable capture" in:
    // λy.(λz.x) with substitute x -> y
    val term   = Abs("y", Abs("z", Var("x")))
    val result = sub(term, "x", Var("y"))
    Term.freeVars(result) shouldBe Set("y")


class TermBetaReduceSpec extends AnyFlatSpec with Matchers:

  "betaReduce" should "reduce a redex correctly" in:
    // (λx.x) y  →  y
    val app = App(Abs("x", Var("x")), Var("y"))
    Term.betaReduce(app) shouldBe Var("y")

  it should "return the App unchanged when there is no redex" in:
    // (x y)  – x is not an abstraction
    val app = App(Var("x"), Var("y"))
    Term.betaReduce(app) shouldBe app

  it should "substitute correctly in a non-trivial body" in:
    // (λx. x x) y  →  y y
    val app = App(Abs("x", App(Var("x"), Var("x"))), Var("y"))
    Term.betaReduce(app) shouldBe App(Var("y"), Var("y"))

  it should "handle constant functions" in:
    // (λx.y) z  →  y
    val app = App(Abs("x", Var("y")), Var("z"))
    Term.betaReduce(app) shouldBe Var("y")


// ─────────────────────────────────────────────
// Evaluator – single-step
// ─────────────────────────────────────────────
class NormalOrderStepSpec extends AnyFlatSpec with Matchers:

  private val step = Evaluator.normalOrderStep

  "normalOrderStep" should "reduce the outermost redex first" in:
    // (λx.x) y  →  y
    step(App(Abs("x", Var("x")), Var("y"))) shouldBe Some(Var("y"))

  it should "step inside the function before the argument when no outer redex" in:
    // ((λx.x) y) z
    val t = App(App(Abs("x", Var("x")), Var("y")), Var("z"))
    step(t) shouldBe Some(App(Var("y"), Var("z")))

  it should "step under an abstraction" in:
    // λx.((λy.y) x)  →  λx.x
    val t = Abs("x", App(Abs("y", Var("y")), Var("x")))
    step(t) shouldBe Some(Abs("x", Var("x")))

  it should "return None for a Var" in:
    step(Var("x")) shouldBe None

  it should "return None for a normal form abstraction" in:
    step(Abs("x", Var("x"))) shouldBe None

  it should "step the argument when the function is already a value and there's no outer redex" in:
    // x ((λy.y) z)  – function is a free var, not an Abs, so no outer redex
    val t = App(Var("x"), App(Abs("y", Var("y")), Var("z")))
    step(t) shouldBe Some(App(Var("x"), Var("z")))


class ApplicativeOrderStepSpec extends AnyFlatSpec with Matchers:

  private val step = Evaluator.applicativeOrderStep

  "applicativeOrderStep" should "reduce the argument before the redex" in:
    // (λx.x) ((λy.y) z)  — argument must be reduced first
    val t = App(Abs("x", Var("x")), App(Abs("y", Var("y")), Var("z")))
    step(t) shouldBe Some(App(Abs("x", Var("x")), Var("z")))

  it should "reduce the outer redex once the argument is in normal form" in:
    // (λx.x) z  →  z
    step(App(Abs("x", Var("x")), Var("z"))) shouldBe Some(Var("z"))

  it should "step under abstractions" in:
    // λx.((λy.y) x)  →  λx.x
    val t = Abs("x", App(Abs("y", Var("y")), Var("x")))
    step(t) shouldBe Some(Abs("x", Var("x")))

  it should "return None for a Var" in:
    step(Var("x")) shouldBe None

  it should "return None for a normal-form abstraction" in:
    step(Abs("x", Var("x"))) shouldBe None

  it should "not reduce when func is not an Abs and neither side steps" in:
    step(App(Var("x"), Var("y"))) shouldBe None


class CallByValueStepSpec extends AnyFlatSpec with Matchers:

  private val step = Evaluator.callByValueStep

  "callByValueStep" should "reduce the argument first" in:
    // (λx.x) ((λy.y) z)
    val t = App(Abs("x", Var("x")), App(Abs("y", Var("y")), Var("z")))
    step(t) shouldBe Some(App(Abs("x", Var("x")), Var("z")))

  it should "apply a beta reduction only when the argument is a value" in:
    // (λx.x) z  – z is a Var, which isValue → reduce
    step(App(Abs("x", Var("x")), Var("z"))) shouldBe Some(Var("z"))

  it should "apply a beta reduction when the argument is an Abs" in:
    // (λx.x) (λy.y)  – λy.y is a value → reduce
    val t = App(Abs("x", Var("x")), Abs("y", Var("y")))
    step(t) shouldBe Some(Abs("y", Var("y")))

  it should "not step under abstractions" in:
    // λx.((λy.y) x)  — CBV does not reduce under binders
    val t = Abs("x", App(Abs("y", Var("y")), Var("x")))
    step(t) shouldBe None

  it should "return None for a Var" in:
    step(Var("x")) shouldBe None

  it should "not reduce when func is not an Abs" in:
    step(App(Var("x"), Var("y"))) shouldBe None


class IsValueSpec extends AnyFlatSpec with Matchers:

  "isValue" should "consider Abs a value" in:
    Evaluator.isValue(Abs("x", Var("x"))) shouldBe true

  it should "consider Var a value" in:
    Evaluator.isValue(Var("x")) shouldBe true

  it should "not consider an App a value" in:
    Evaluator.isValue(App(Var("x"), Var("y"))) shouldBe false

  it should "not consider a redex a value" in:
    Evaluator.isValue(App(Abs("x", Var("x")), Var("y"))) shouldBe false


// ─────────────────────────────────────────────
// Evaluator – full evaluation
// ─────────────────────────────────────────────
class EvaluatorEvaluateSpec extends AnyFlatSpec with Matchers:

  // Church booleans
  private val tru  = Abs("t", Abs("f", Var("t")))          // λt.λf.t
  private val fls  = Abs("t", Abs("f", Var("f")))          // λt.λf.f
  private val id   = Abs("x", Var("x"))                    // λx.x
  private val konst = Abs("x", Abs("y", Var("x")))         // λx.λy.x  (= true)

  // (λx.x) y  – single step to normal form
  private val simpleRedex = App(id, Var("y"))

  "evaluate with NormalOrder" should "reach Done for a simple redex" in:
    Evaluator.evaluate(simpleRedex, Strategy.NormalOrder) match
      case EvalResult.Done(t, steps) =>
        t      shouldBe Var("y")
        steps  shouldBe 1
      case other => fail(s"Expected Done, got $other")

  it should "return Done immediately for a normal form" in:
    Evaluator.evaluate(Var("x"), Strategy.NormalOrder) match
      case EvalResult.Done(t, steps) =>
        t     shouldBe Var("x")
        steps shouldBe 0
      case other => fail(s"Expected Done(Var(x),0), got $other")

  it should "evaluate the Church identity applied to true" in:
    // (λx.x) tru  →  tru
    val t = App(id, tru)
    Evaluator.evaluate(t, Strategy.NormalOrder) match
      case EvalResult.Done(result, _) => result shouldBe tru
      case other                      => fail(s"Expected Done, got $other")

  it should "signal StepLimitReached for an omega combinator" in:
    // Ω = (λx.x x)(λx.x x)  – diverges
    val omega = App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x"))))
    val limit = 50
    Evaluator.evaluate(omega, Strategy.NormalOrder, limit) match
      case EvalResult.StepLimitReached(_, steps) => steps shouldBe limit
      case other                                  => fail(s"Expected StepLimitReached, got $other")

  "evaluate with ApplicativeOrder" should "reach Done for a simple redex" in:
    Evaluator.evaluate(simpleRedex, Strategy.ApplicativeOrder) match
      case EvalResult.Done(t, steps) =>
        t     shouldBe Var("y")
        steps shouldBe 1
      case other => fail(s"Expected Done, got $other")

  it should "evaluate a multi-step expression" in:
    // (λx.λy.x) a b  →  a
    val t = App(App(konst, Var("a")), Var("b"))
    Evaluator.evaluate(t, Strategy.ApplicativeOrder) match
      case EvalResult.Done(result, _) => result shouldBe Var("a")
      case other                      => fail(s"Expected Done, got $other")

  "evaluate with CallByValue" should "reach Done for a simple redex" in:
    Evaluator.evaluate(simpleRedex, Strategy.CallByValue) match
      case EvalResult.Done(t, steps) =>
        t     shouldBe Var("y")
        steps shouldBe 1
      case other => fail(s"Expected Done, got $other")

  it should "not reduce under a lambda" in:
    // λx.((λy.y) x)  – CBV leaves this unreduced (no outer redex with value arg)
    val t = Abs("x", App(id, Var("x")))
    Evaluator.evaluate(t, Strategy.CallByValue) match
      case EvalResult.Done(result, steps) =>
        result shouldBe t   // unchanged
        steps  shouldBe 0
      case other => fail(s"Expected Done, got $other")

  it should "evaluate applying identity to an abstraction" in:
    // (λx.x)(λy.y)  →  λy.y
    val t = App(id, id)
    Evaluator.evaluate(t, Strategy.CallByValue) match
      case EvalResult.Done(result, _) => result shouldBe id
      case other                      => fail(s"Expected Done, got $other")

  "EvalResult" should "carry the correct step count on Done" in:
    // Three reductions: ((λx.x)(λx.x)) y  →  (λx.x) y  →  y
    val t = App(App(id, id), Var("y"))
    Evaluator.evaluate(t, Strategy.NormalOrder) match
      case EvalResult.Done(result, steps) =>
        result shouldBe Var("y")
        steps  shouldBe 2
      case other => fail(s"Expected Done, got $other")