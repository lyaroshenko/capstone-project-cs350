package lambda

sealed trait Term
case class Var(name: String)              extends Term
case class Abs(param: String, body: Term) extends Term
case class App(func: Term, arg: Term)     extends Term

object Term:

  def freeVars(t: Term): Set[String] = t match
    case Var(name)        => Set(name)
    case Abs(param, body) => freeVars(body) - param
    case App(func, arg)   => freeVars(func) ++ freeVars(arg)

  def freshVar(avoid: Set[String]): String =
    val candidates = Iterator.single("z") ++ Iterator.from(0).map(i => s"z$i")
    candidates.dropWhile(avoid.contains).next()

  def substitute(term: Term, x: String, n: Term): Term = term match
    case Var(name) =>
      if name == x then n else term
    case App(func, arg) =>
      App(substitute(func, x, n), substitute(arg, x, n))
    case Abs(param, body) =>
      if param == x then term
      else if !freeVars(body).contains(x) then term
      else if !freeVars(n).contains(param) then Abs(param, substitute(body, x, n))
      else
        val z = freshVar(freeVars(n) ++ freeVars(body))
        Abs(z, substitute(substitute(body, param, Var(z)), x, n))

  def betaReduce(app: App): Term = app match
    case App(Abs(param, body), arg) => substitute(body, param, arg)
    case _                          => app
