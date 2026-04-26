package lambda

enum EvalResult:
  case Done(term: Term, steps: Int)
  case StepLimitReached(term: Term, steps: Int)

object Evaluator:
  def normalOrderStep(t: Term): Option[Term] = t match
    case App(Abs(param, body), arg) =>
      Some(Term.substitute(body, param, arg))
    case App(func, arg) =>
      normalOrderStep(func).map(App(_, arg))
        .orElse(normalOrderStep(arg).map(App(func, _)))
    case Abs(param, body) =>
      normalOrderStep(body).map(Abs(param, _))
    case Var(_) => None

  def applicativeOrderStep(t: Term): Option[Term] = t match
    case App(func, arg) =>
      applicativeOrderStep(arg).map(App(func, _))
        .orElse(applicativeOrderStep(func).map(App(_, arg)))
        .orElse(func match
          case Abs(param, body) => Some(Term.substitute(body, param, arg))
          case _ => None)
    case Abs(param, body) =>
      applicativeOrderStep(body).map(Abs(param, _))
    case Var(_) => None

  def callByValueStep(t: Term): Option[Term] = t match
    case App(func, arg) =>
      callByValueStep(arg).map(App(func, _))
        .orElse(callByValueStep(func).map(App(_, arg)))
        .orElse(func match
          case Abs(param, body) if isValue(arg) =>
            Some(Term.substitute(body, param, arg))
          case _ => None)
    case _ => None

  def isValue(t: Term): Boolean = t match
    case Abs(_, _) => true
    case Var(_)    => true
    case _         => false

  def evaluate(
                term: Term,
                strategy: Strategy,
                maxSteps: Int = 1000
              ): EvalResult =
    val step: Term => Option[Term] = strategy match
      case Strategy.NormalOrder      => normalOrderStep
      case Strategy.ApplicativeOrder => applicativeOrderStep
      case Strategy.CallByValue      => callByValueStep

    @annotation.tailrec
    def loop(current: Term, steps: Int): EvalResult =
      if steps >= maxSteps then EvalResult.StepLimitReached(current, steps)
      else step(current) match
        case None       => EvalResult.Done(current, steps)
        case Some(next) => loop(next, steps + 1)

    loop(term, 0)