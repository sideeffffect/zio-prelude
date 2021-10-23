package zio.prelude
package experimental

import zio.prelude.newtypes.{AndF, OrF}

trait Involution[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def complement(a: A): A

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]

}

object Involution {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = Involution[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Involution[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    involution: Involution.Aux[A, Join, Meet]
  ): Involution.Aux[A, Join, Meet] =
    involution
}

trait InvolutionSyntax {

  /**
   * Provides infix syntax for the complement of the value.
   */
  implicit class InvolutionOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit joinMeet: Involution.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: Involution.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: Involution.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: Involution.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * A symbolic alias for `complement`.
     */
    def unary_!(implicit complement: Involution.Aux[A, Associative, Associative]): A =
      complement.complement(l)

    /**
     * The complement of the value.
     */
    def complement(implicit complement: Involution.Aux[A, Associative, Associative]): A =
      complement.complement(l)

  }

}
