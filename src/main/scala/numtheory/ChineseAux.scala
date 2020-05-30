package numtheory

import Num.Implicits._

/***
 * Case Class to assist the Chinese Remainder method in NumberTheory class
 *
 * <p>
 *   Format: x ≡ `number` mod `moduli`
 * </p>
 *
 * @param number A number
 * @param modulus A moduli
 * @param op Implicit type class
 * @tparam A Type class
 */
case class ChineseAux[A](number: A, modulus: A)(implicit op: Num[A]){ }

