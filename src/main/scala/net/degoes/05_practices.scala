package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = !(!self && !that)

    def ^^(that: EmailFilter): EmailFilter = (self || that) && !(self && that)

    def unary_!(): EmailFilter = EmailFilter.Not(self)
  }
  object EmailFilter {
    final case object Always                                    extends EmailFilter
    final case class Not(target: EmailFilter)                   extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class SenderEquals(target: Address)              extends EmailFilter
    final case class RecipientEquals(target: Address)           extends EmailFilter
    final case class BodyContains(phrase: String)               extends EmailFilter
    final case class SubjectContains(phrase: String)            extends EmailFilter

    val always: EmailFilter = Always

    val never: EmailFilter = !always

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter =
      !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter =
      senders.foldLeft(never)((acc, address) => acc || senderIs(address))

    def recipientIn(recipients: Set[Address]): EmailFilter =
      recipients.foldLeft(never)((acc, address) => acc || recipientIs(address))

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait Turtle { self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  object executable {
    final case class Turtle2(exec: Turtle => Unit) { self =>
      def ++(that: Turtle2): Turtle2 =
        Turtle2 { turtle =>
          self.exec(turtle)
          that.exec(turtle)
        }
    }
    object Turtle2 {
      def turnLeft(deg: Int): Turtle2  = Turtle2(_.turnLeft(deg))
      def turnRight(deg: Int): Turtle2 = Turtle2(_.turnRight(deg))
      val goForward: Turtle2           = Turtle2(_.goForward())
      val goBackward: Turtle2          = Turtle2(_.goBackward())
      val draw: Turtle2                = Turtle2(_.draw())

    }
  }

  object declarative {
    sealed trait Turtle2 { self =>
      def andThen(that: Turtle2): Turtle2 = And(self, that)
    }
    final case object ToForward                         extends Turtle2
    final case object TurnLeft                          extends Turtle2
    final case object Draw                              extends Turtle2
    final case class And(left: Turtle2, right: Turtle2) extends Turtle2

    def idle: Turtle2 = goForward andThen goBackward

    def goForward(distance: Int): Turtle2 =
      (1 to distance).foldLeft(idle)((acc, _) => acc andThen goForward)
    val goForward: Turtle2 = ToForward

    def goBackward(distance: Int): Turtle2 =
      (1 to distance).foldLeft(idle)((acc, _) => acc andThen goBackward)
    val goBackward: Turtle2 = turnLeft(180) andThen goForward andThen turnLeft(180)

    def turnRight(deg: Int): Turtle2 = turnLeft(360 - deg)
    def turnLeft(deg: Int): Turtle2  = (1 to deg).foldLeft(idle)((acc, _) => acc andThen turnLeft)
    val turnLeft: Turtle2            = TurnLeft

    def exec(turtle: Turtle, turtle2: Turtle2): Unit = {
      def loop(turtle2: Turtle2): Turtle2 = turtle2 match {
        case ToForward => ???
        case TurnLeft  => ???
        case Draw      => ???
        case And(left, right) => {
          loop(left)
          loop(right)
        }
      }
    }
  }

}
