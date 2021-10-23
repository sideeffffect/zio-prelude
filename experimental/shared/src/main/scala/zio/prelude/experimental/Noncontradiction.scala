package zio.prelude
package experimental

import zio.prelude.newtypes.{AndF, OrF}

trait Noncontradiction[A] {

  type Join[x] <: Identity[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def complement(a: A): A

  def bottom: A = Join.identity

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]

}

object Noncontradiction {

  type Aux[A, +join[x] <: Identity[x], +meet[x] <: Associative[x]] = Noncontradiction[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A, Join[x] <: Identity[x], Meet[x] <: Associative[x]](implicit
    noncontradiction: Noncontradiction.Aux[A, Join, Meet]
  ): Noncontradiction.Aux[A, Join, Meet] =
    noncontradiction
}

trait NoncontradictionSyntax {

  /**
   * Provides infix syntax for the complement of the value.
   */
  implicit class NoncontradictionOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit joinMeet: Noncontradiction.Aux[A, Identity, Associative]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: Noncontradiction.Aux[A, Identity, Associative]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: Noncontradiction.Aux[A, Identity, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: Noncontradiction.Aux[A, Identity, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * A symbolic alias for `complement`.
     */
    def unary_!(implicit complement: Noncontradiction.Aux[A, Identity, Associative]): A =
      complement.complement(l)

    /**
     * The complement of the value.
     */
    def complement(implicit complement: Noncontradiction.Aux[A, Identity, Associative]): A =
      complement.complement(l)

  }

}
