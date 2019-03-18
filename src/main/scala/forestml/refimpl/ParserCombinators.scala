package forestml.refimpl

/** Applicative parser combinators for building predictive top-down parsers. */
trait ParserCombinators {

  /** Type of the tokens */
  type TT

  /** Subtype of the tokens that are irrelevant for making the next decision. */
  type W <: TT

  type Ws = List[Token[W]]

  /** Error recovery heuristic for resynchronization */
  type H

  /** Result returned by a `Parser`.
    *
    * There are three cases:
    *
    * $ - the parsing step was successful,
    * $ - the parsing step failed, but it resynchronized already
    * $ - the parse failed and requires to discard more stack
    *     frames in order to resynchronize.
    */
  sealed trait ParseRes[+A]

  /** Request to discard `resyncLevel` stack frames in order to resynchronize.
    *
    * The case `Resynchronize(0)` is automatically converted
    * into a `ParseFailure` by the wrapper returned by
    * the `sync` method.
    */
  case class Resynchronize(resyncLevel: Int) extends ParseRes[Nothing]

  /** Means that the parser failed to parse, but managed to resynchronize.
    *
    * Discarding further stack frames is not needed.
    */
  case object ParseFailure extends ParseRes[Nothing]

  /** Successful parse that, given the whitespace prefix,
    * produces a value of type `A`.
    */
  case class ParseSuccess[+A](result: A) extends ParseRes[A]


  /** Resynchronizing applicative parser combinator.
    *
    * Given input (tokens), accumulator of error messages, and
    * a resynchronization context, 
    * computes new states of input and error
    * accumulator, and returns a [ParseRes].
    */
  sealed trait PC[+A] { outer =>

    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[A])

    def zip[B](other: PC[B]): PC[(A, B)] = {
      new PC[(A, B)] {
        def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
          input: ParserInput[TT, W],
          errors: ErrAcc,
          ctx: SynchronizationContext
        ): (ParserInput[TT, W], ErrAcc, ParseRes[(A, B)]) = {
          val (i1, e1, r1) = outer(input, errors, ctx)
          r1 match {
            case ParseSuccess(a) => {
              val (i2, e2, r2) = other(i1, e1, ctx)
              val r = r2 match {
                case ParseSuccess(b) => ParseSuccess((a, b))
                case ParseFailure => ParseFailure
                case s @ Resynchronize(_) => s
              }
              (i2, e2, r)
            }
            case ParseFailure => {
              val (i2, e2, r2) = other(i1, e1, ctx)
              val r = r2 match {
                case ParseSuccess(b) => ParseFailure
                case ParseFailure => ParseFailure
                case s @ Resynchronize(_) => s
              }
              (i2, e2, r)
            }
            case x @ Resynchronize(i) => (i1, e1, x)
          }
        }
      }
    }

    def map[B](f: A => B): PC[B] = new PC[B] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[B]) = {
        val (i1, e1, r1) = outer(input, errors, ctx)
        val r = r1 match {
          case ParseSuccess(a) => ParseSuccess(f(a))
          case ParseFailure => ParseFailure
          case s @ Resynchronize(_) => s
        }
        (i1, e1, r)
      }
    }

    /** Behaves like usual `flatMap` under the assumption that `this` 
      * parser combinator does not fail.
      *
      * Exits with a fatal error if `this` fails after all.
      *
      * Useful only for single-token languages, whose presense in the
      * input has already been verified by `peek`.
      */
    def flatMapInfallible[B](f: A => PC[B]): PC[B] = new PC[B] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[B]) = {
        val (i1, e1, r1) = outer(input, errors, ctx)
        r1 match {
          case ParseSuccess(a) => {
            val pcb = f(a)
            pcb(i1, e1, ctx)
          }
          case _ => throw new Error(
            "The `flatMapInfallible` has failed. " + 
            "The logic that guaranteed that it must succeed was flawed. " +
            "This should never happen, " +
            "it's a bug in the reference implementation. ID-762759052."
          )
        }
      }
    }

    def syncIf(
      isGoodResynchronizationPoint: (Token[TT], H) => Boolean
    ): PC[A] = new PC[A] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) = {
        val extendedCtx = ctx.push(isGoodResynchronizationPoint)
        val (i1, e1, r1) = outer(input, errors, extendedCtx)
        val r = r1 match {
          case Resynchronize(i) => {
            if (i == 0) ParseFailure
            else Resynchronize(i - 1)
          }
          case ParseFailure => ParseFailure
          case ParseSuccess(a) => ParseSuccess(a)
        }
        (i1, e1, r)
      }
    }

    def syncOnSet(following: Set[TT]): PC[A] = this.syncIf {
      case (tok, _) => following contains tok.tokenType
    }

    /** If an element is parsed successfully, performes an additional check
      * on the parsed element (possibly using additional hints from the
      * resynchronization context), and if the condition is satisfied,
      * emits a warning (still returns `ParseSuccess`, does not fail).
      */
    def warnIf
      (condition: (A, SynchronizationContext) => Boolean)
      (ifTrue: (A, SynchronizationContext) => ParseWarning)
    : PC[A] = new PC[A] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) = {
        val (i1, e1, r1) = outer(input, errors, ctx)
        val e2 = r1 match {
          case ParseSuccess(a) if (condition(a, ctx)) => {
            errors.append(ifTrue(a, ctx))
          }
          case _ => errors
        }
        (i1, e2, r1)
      }
    }

    def ensuring(condition: A => Boolean, errMsg: String): PC[A] = new PC[A] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) = {
        val (i1, e1, r1) = outer(input, errors, ctx)
        val (e2, r2) = r1 match {
          case ParseSuccess(a) => if (condition(a)) {
            (e1, r1)
          } else {
            (
              e1.append(i1.error(
                "PARSE-ERROR-SEMANTIC-CHECK-VIOLATED",
                errMsg
              )),
              ParseFailure
            )
          }
          case _ => (e1, r1)
        }
        (i1, e2, r2)
      }
    }
  }

  case class SynchronizationContext(
    errorRecoveryHeuristic: H,
    stack: List[(Token[TT], H) => Boolean]
  ) {
    def push(predicate: (Token[TT], H) => Boolean): SynchronizationContext = {
      SynchronizationContext(errorRecoveryHeuristic, predicate :: stack)
    }

    lazy val predicateStack: List[Token[TT] => Boolean] = {
      stack.map(thb => ((t: Token[TT]) => thb(t, errorRecoveryHeuristic)))
    }
  }

  object SynchronizationContext {
    def empty(heuristic: H): SynchronizationContext =
      SynchronizationContext(heuristic, Nil)
  }

  def delay[A](p: => PC[A]) = new PC[A] {
    private lazy val instantiated = p
    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) =
      instantiated(input, errors, ctx)
  }

  def pure[A](a: A): PC[A] = new PC[A] {
    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) =
      (input, errors, ParseSuccess(a))
  }

  def unit: PC[Unit] = pure( () )

  def eat(t: TT): PC[(List[Token[W]], Token[TT])] = {
    new PC[(List[Token[W]], Token[TT])] {
      def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
        input: ParserInput[TT, W],
        errors: ErrAcc,
        ctx: SynchronizationContext
      ): (ParserInput[TT, W], ErrAcc, ParseRes[(Ws, Token[TT])]) = {
        input.peekType(0) match {
          case Some(x) => if (x == t) {
            val r = ParseSuccess((
              input.stashedWhitespace,
              input.peek(0).get
            ))
            (input.advance(1), errors, r)
          } else {
            // panic mode!
            // 1) Emit error.
            // 2) run forward until either event occurs:
            //    i) A character from the synchronization set is found,
            //       then exit with a `Resynchronize` command
            //       (corresponds to useful character insertion, junk deletion)
            //    ii) `c` is found eventually, consume it and with Failure
            //       (corresponds to junk character deletion)
            //    iii) Eof - emit `Failure`, nothing more to do.
            val newErrors = errors.append(
              input.error(
                "FORML-ERR-SYN-EAT-UNEXP-TOK",
                "Expected " + t + " but found " + x
              )
            )
            val (newParserInput, res) = panic(input, ctx)
            (newParserInput, newErrors, res)
          }
          case None => {
            val newErrors = errors.append(
              input.error(
                "FORML-ERR-SYN-EAT-EOF",
                "Expected " + t + " but hit end of input"
              )
            )
            (input, newErrors, ParseFailure)
          }
        }
      }

      def panic(input: ParserInput[TT, W], ctx: SynchronizationContext)
      : (ParserInput[TT, W], ParseRes[Nothing]) = {
        @annotation.tailrec
        def recHelper(offset: Int): (Int, ParseRes[Nothing]) = {
          input.peek(offset) match {
            case None => (offset, ParseFailure)
            case Some(x) => if (x.tokenType == t) {
              (offset + 1, ParseFailure)
            } else {
              (ctx.predicateStack zip Stream.from(0)).find {
                // next `case` is decons-only, no branch
                case (predicate, level) => predicate(x)
              }.map(_._2) match {
                case None => recHelper(offset + 1)
                case Some(level) => (offset, Resynchronize(level))
              }
            }
          }
        }
        val (offset, result) = recHelper(0)
        (input.advance(offset), result)
      }
    }
  }

  def onlyIfStartsWith[A](firstSet: Set[TT], p: PC[A])
  : PC[Option[A]] = new PC[Option[A]] {
    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[Option[A]]) = {
      val typ = input.peekType(0)
      if (typ.fold(false)(firstSet.contains)) {
        (p.map{a => (Some(a): Option[A])})(input, errors, ctx)
      } else {
        (input, errors, ParseSuccess(None))
      }
    }
  }

  def repeatWhileStartsWith[A](firstSet: Set[TT], p: PC[A])
  : PC[List[A]] = new PC[List[A]] {
    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[List[A]]) = {
      var listBuilder = collection.mutable.ListBuffer.empty[A]
      var currentInput = input
      var currentErrors = errors
      var allSuccessful = true

      while (true) {
        val typ = currentInput.peekType(0)
        if (typ.fold(false)(firstSet.contains)) {
          val (i, e, a) = p(currentInput, currentErrors, ctx)
          a match {
            case ParseFailure => {
              // failed, but managed to resynchronize, continue the loop
              allSuccessful = false
            }
            case r @ Resynchronize(_) => {
              // failed, and also didn't manage to resynchronize -> escalate
              return (i, e, r)
            }
            case ParseSuccess(elem) => {
              listBuilder += elem
            }
          }
          currentInput = i
          currentErrors = e
        } else {
          return (if (allSuccessful) {
            (currentInput, currentErrors, ParseSuccess(listBuilder.toList))
          } else {
            (currentInput, currentErrors, ParseFailure)
          })
        }
      }
      throw new Error("unreachable after `while(true) { ... }`")
    }
  }

  def branch[A](branches: List[(Set[TT], PC[A])]): PC[A] = new PC[A] {

    private val branchMap: Map[TT, PC[A]] = (for {
      (fs, p) <- branches
      t <- fs
    } yield (t, p))(collection.breakOut)

    require(
      branchMap.size == branches.map(_._1.size).sum,
      "Intersecting FIRST sets in predictive `branch`!"
    )

    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[A]) = {
      input.peekType(0) match {
        case None => {
          (
            input,
            errors.append(input.error(
              "FORML-ERR-SYN-BRANCH-EOF",
              "Unexpected end of input. Expected one of: " +
              branchMap.values.mkString("{", ",", "}")
            )),
            ParseFailure
          )
        }
        case Some(t) => {
          branchMap.get(t) match {
            case None => {
              val e2 = errors.append(input.error(
                "FORML-ERR-SYN-BRANCH-UNEXP-TOK",
                "Unexpected token type: " + t + ". " +
                "Expected one of: " +
                branchMap.values.mkString("{", ",", "}")
              ))
              panic(input, e2, ctx)
            }
            case Some(p) => p(input, errors, ctx)
          }
        }
      }
    }

    def panic[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[Nothing]) = {

      // Runs forward until one of the following three
      // cases occur:
      // - End of input is reached.
      // - A token from the FIRST set of one of the branches is found.
      // - A token from the resynchronization context is found
      @annotation.tailrec
      def recHelper(offset: Int)
      : (ParserInput[TT, W], ErrAcc, ParseRes[Nothing]) = {
        input.peek(offset) match {
          // EOF
          case None => (input.advance(offset), errors, ParseFailure)
          // There is a next symbol, not EOF
          case Some(x) => branchMap.get(x.tokenType) match {
            // Symbol from FIRST set of one of the branches: delegate
            case Some(p) => {
              val i1 = input.advance(offset)
              val (i2, e2, res) = p(i1, errors, ctx)
              res match {
                case ParseFailure => (i2, e2, ParseFailure)
                case r @ Resynchronize(level) => (i2, e2, r)
                case ParseSuccess(_) => (i2, e2, ParseFailure)
              }
            }
            // Symbol not from any FIRST set
            case None => {
              (ctx.predicateStack zip Stream.from(0)).find {
                case (predicate, level) => predicate(x)
              }.map(_._2) match {
                // Symbol found in resynchronization context
                case Some(level) =>
                  (input.advance(offset), errors, Resynchronize(level))
                // Neither in FIRST, nor in `ctx`
                case None => recHelper(offset + 1)
              }
            }
          }
        }
      }

      recHelper(0)
    }
  }

  val eof: PC[Ws] = new PC[Ws] {
    def apply[ErrAcc <: Accumulator[ParseError, ErrAcc]](
      input: ParserInput[TT, W],
      errors: ErrAcc,
      ctx: SynchronizationContext
    ): (ParserInput[TT, W], ErrAcc, ParseRes[Ws]) = {
      input.peekType(0) match {
        case None =>
          (input, errors, ParseSuccess(input.stashedWhitespace))
        case Some(_) => {
          val e = input.error(
            "PARSE-ERROR-NOT-EOF",
            "Expected EOF here, but found more tokens."
          )
          (input, errors.append(e), ParseFailure)
        }
      }
    }
  }
}
