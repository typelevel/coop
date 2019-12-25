/*
 * Copyright 2019 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package coop

import cats.{Applicative, Eval, Traverse}
import cats.implicits._

sealed trait ThreadF[+A] extends Product with Serializable

object ThreadF {

  implicit val traverse: Traverse[ThreadF] = new Traverse[ThreadF] {

    def foldLeft[A, B](fa: ThreadF[A], b: B)(f: (B, A) => B): B = fa match {
      case Fork(left, right) => f(f(b, left), right)
      case Cede(result) => f(b, result)
      case Done => b
    }

    def foldRight[A, B](fa: ThreadF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Fork(left, right) => f(left, f(right, lb))
      case Cede(result) => f(result, lb)
      case Done => lb
    }

    def traverse[G[_]: Applicative, A, B](fa: ThreadF[A])(f: A => G[B]): G[ThreadF[B]] = fa match {
      case Fork(left, right) =>
        (f(left), f(right)).mapN(Fork(_, _): ThreadF[B])

      case Cede(result) =>
        f(result).map(Cede(_): ThreadF[B])

      case Done =>
        (Done: ThreadF[B]).pure[G]
    }
  }

  final case class Fork[A](left: A, right: A) extends ThreadF[A]
  final case class Cede[A](result: A) extends ThreadF[A]
  case object Done extends ThreadF[Nothing]
}
