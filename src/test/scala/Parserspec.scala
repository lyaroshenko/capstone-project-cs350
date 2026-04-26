package lambda

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers:

  "Parser.parse" should "parse a single variable" in:
    Parser.parse("x") shouldBe Var("x")

  it should "parse a multi-character identifier" in:
    Parser.parse("foo") shouldBe Var("foo")

  it should "parse an identifier with digits and underscores" in:
    Parser.parse("x1_a") shouldBe Var("x1_a")

  it should "parse a simple abstraction" in:
    Parser.parse("\\x. x") shouldBe Abs("x", Var("x"))

  it should "parse a two-parameter abstraction as nested Abs" in:
    Parser.parse("\\x y. x") shouldBe Abs("x", Abs("y", Var("x")))

  it should "parse a three-parameter abstraction" in:
    Parser.parse("\\x y z. z") shouldBe Abs("x", Abs("y", Abs("z", Var("z"))))

  it should "parse an abstraction whose body is itself an abstraction" in:
    Parser.parse("\\x. \\y. x") shouldBe Abs("x", Abs("y", Var("x")))

  it should "treat multi-param sugar and nested lambdas identically" in:
    Parser.parse("\\x y. x") shouldBe Parser.parse("\\x. \\y. x")

  it should "parse application of two variables" in:
    Parser.parse("f x") shouldBe App(Var("f"), Var("x"))

  it should "parse application left-associatively" in:
    Parser.parse("f g h") shouldBe App(App(Var("f"), Var("g")), Var("h"))

  it should "parse three-term left-associative application" in:
    Parser.parse("a b c d") shouldBe App(App(App(Var("a"), Var("b")), Var("c")), Var("d"))

  it should "parse a parenthesised variable" in:
    Parser.parse("(x)") shouldBe Var("x")

  it should "parse application of identity to a variable" in:
    Parser.parse("(\\x. x) y") shouldBe App(Abs("x", Var("x")), Var("y"))

  it should "use parentheses to override left-associativity" in:
    Parser.parse("f (g h)") shouldBe App(Var("f"), App(Var("g"), Var("h")))

  it should "parse a parenthesised abstraction applied to another abstraction" in:
    Parser.parse("(\\x. x) (\\y. y)") shouldBe App(Abs("x", Var("x")), Abs("y", Var("y")))

  it should "parse the Church numeral 2" in:
    Parser.parse("\\f x. f (f x)") shouldBe
      Abs("f", Abs("x", App(Var("f"), App(Var("f"), Var("x")))))

  it should "parse the K combinator" in:
    Parser.parse("\\x y. x") shouldBe Abs("x", Abs("y", Var("x")))

  it should "parse the S combinator" in:
    Parser.parse("\\x y z. x z (y z)") shouldBe
      Abs("x", Abs("y", Abs("z", App(App(Var("x"), Var("z")), App(Var("y"), Var("z"))))))

  it should "parse the Omega combinator" in:
    val selfApp = Abs("x", App(Var("x"), Var("x")))
    Parser.parse("(\\x. x x) (\\x. x x)") shouldBe App(selfApp, selfApp)

  it should "ignore extra whitespace everywhere" in:
    Parser.parse("  \\x  .  x  ") shouldBe Abs("x", Var("x"))

  it should "ignore newlines and tabs" in:
    Parser.parse("\\x.\n  x\t") shouldBe Abs("x", Var("x"))

  it should "let the abstraction body consume the rest of the term" in:
    // \x. x y must parse as Abs("x", App(x, y)), not App(Abs("x", x), y)
    Parser.parse("\\x. x y") shouldBe Abs("x", App(Var("x"), Var("y")))

  it should "throw ParseError for an empty string" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("")

  it should "throw ParseError for a backslash with no parameters" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("\\. x")

  it should "throw ParseError for a backslash with no dot" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("\\x x")

  it should "throw ParseError for an unclosed parenthesis" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("(x")

  it should "throw ParseError for an unexpected closing parenthesis" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("x)")

  it should "throw ParseError for an unrecognised character" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("x + y")

  it should "throw ParseError for trailing tokens after a complete term" in:
    an[Parser.ParseError] should be thrownBy Parser.parse("(x y) )")
