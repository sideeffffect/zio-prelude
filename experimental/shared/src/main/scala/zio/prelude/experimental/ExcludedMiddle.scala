package zio.prelude
package experimental

import zio.prelude.newtypes.{AndF, OrF}

trait ExcludedMiddle[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Identity[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def complement(a: A): A

  def top: A = Meet.identity

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]

}

object ExcludedMiddle {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Identity[x]] = ExcludedMiddle[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Identity[x]](implicit
    excludedMiddle: ExcludedMiddle.Aux[A, Join, Meet]
  ): ExcludedMiddle.Aux[A, Join, Meet] =
    excludedMiddle
}

trait ExcludedMiddleSyntax {

  /**
   * Provides infix syntax for the complement of the value.
   */
  implicit class ExcludedMiddleOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit joinMeet: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      joinMeet.meet(l, r)

    /**
     * A symbolic alias for `complement`.
     */
    def unary_!(implicit complement: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      complement.complement(l)

    /**
     * The complement of the value.
     */
    def complement(implicit complement: ExcludedMiddle.Aux[A, Associative, Identity]): A =
      complement.complement(l)

  }

}
