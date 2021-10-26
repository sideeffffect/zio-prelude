package zio.prelude
package experimental

trait Noncontradiction[A] extends Complement[A] {
  def bottom: A
}

object Noncontradiction {

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A](implicit noncontradiction: Noncontradiction[A]): Noncontradiction[A] =
    noncontradiction
}
