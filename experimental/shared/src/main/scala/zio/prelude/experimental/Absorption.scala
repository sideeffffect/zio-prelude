package zio.prelude
package experimental

trait Absorption[A] {
  def or(l: A, r: A): A
  def and(l: A, r: A): A
}

object Absorption {

  /**
   * Summons an implicit `Absorption[A]`.
   */
  def apply[A](implicit absorption: Absorption[A]): Absorption[A] = absorption

  implicit lazy val BoolInstance: DistributiveAbsorption[Boolean]
    with ExcludedMiddle[Boolean]
    with Involution[Boolean]
    with Noncontradiction[Boolean] =
    new DistributiveAbsorption[Boolean]
      with ExcludedMiddle[Boolean]
      with Involution[Boolean]
      with Noncontradiction[Boolean] {
      override def complement(a: Boolean): Boolean      = !a
      override val bottom: Boolean                      = false
      override val top: Boolean                         = true
      override def or(l: Boolean, r: Boolean): Boolean  = l || r
      override def and(l: Boolean, r: Boolean): Boolean = l && r
    }

  implicit def SetInstance[A]: DistributiveAbsorption[Set[A]] =
    new DistributiveAbsorption[Set[A]] {
      override def or(l: Set[A], r: Set[A]): Set[A]  = l | r
      override def and(l: Set[A], r: Set[A]): Set[A] = l & r
    }
}

trait AbsorptionSyntax {

  /**
   * Provides infix syntax for joining or meeting two values.
   */
  implicit class AbsorptionOps[A](private val l: A) {

    /**
     * A symbolic alias for `or`.
     */
    def vvv(r: A)(implicit absorption: Absorption[A]): A =
      absorption.or(l, r)

    /**
     * Or two values.
     */
    def or(r: A)(implicit absorption: Absorption[A]): A =
      absorption.or(l, r)

    /**
     * A symbolic alias for `and`.
     */
    def ^^^(r: A)(implicit absorption: Absorption[A]): A =
      absorption.and(l, r)

    /**
     * And two values.
     */
    def and(r: A)(implicit absorption: Absorption[A]): A =
      absorption.and(l, r)
  }

}
