package zio.prelude
package experimental

import zio.prelude.newtypes.{AndF, OrF}

trait DistributiveJoinMeet[A] {

  type Join[x] <: Associative[x]

  type Meet[x] <: Associative[x]

  def join(l: => A, r: => A): A =
    Join.combine(OrF(l), OrF(r))

  def meet(l: => A, r: => A): A =
    Meet.combine(AndF(l), AndF(r))

  def Join: Join[OrF[A]]

  def Meet: Meet[AndF[A]]

}

object DistributiveJoinMeet {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Associative[x]] = DistributiveJoinMeet[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * Summons an implicit `DistributiveJoinMeet[A]`.
   */
  def apply[A, Join[x] <: Associative[x], Meet[x] <: Associative[x]](implicit
    distributiveJoinMeet: DistributiveJoinMeet.Aux[A, Join, Meet]
  ): DistributiveJoinMeet.Aux[A, Join, Meet] =
    distributiveJoinMeet
}

trait DistributiveJoinMeetSyntax {

  /**
   * Provides infix syntax for joining or meeting two values.
   */
  implicit class DistributiveJoinMeetOps[A](private val l: A) {

    /**
     * A symbolic alias for `join`.
     */
    def vvv(r: => A)(implicit joinMeet: DistributiveJoinMeet.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * Join two values.
     */
    def join(r: => A)(implicit joinMeet: DistributiveJoinMeet.Aux[A, Associative, Associative]): A =
      joinMeet.join(l, r)

    /**
     * A symbolic alias for `meet`.
     */
    def ^^^(r: => A)(implicit joinMeet: DistributiveJoinMeet.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)

    /**
     * Meet two values.
     */
    def meet(r: => A)(implicit joinMeet: DistributiveJoinMeet.Aux[A, Associative, Associative]): A =
      joinMeet.meet(l, r)
  }

}
