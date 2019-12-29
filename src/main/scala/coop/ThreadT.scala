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

import cats.{Applicative, Functor, Monad}
import cats.free.FreeT
import cats.implicits._

import scala.collection.immutable.Queue

object ThreadT {

  def liftF[M[_]: Functor, A](ma: M[A]): ThreadT[M, A] =
    FreeT.liftT[ThreadF, M, A](ma)

  def fork[M[_]: Applicative, A](left: A, right: A): ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](ThreadF.Fork(left, right))

  def cede[M[_]: Applicative]: ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](ThreadF.Cede(()))

  def done[M[_]: Applicative, A]: ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](ThreadF.Done)

  def start[M[_]: Applicative, A](child: ThreadT[M, A]): ThreadT[M, Unit] =
    fork[M, Boolean](false, true).ifM(child >> done[M, Unit], ().pure[ThreadT[M, ?]])

  def roundRobin[M[_]: Monad, A](main: ThreadT[M, A]): M[Unit] = {
    // we maintain a separate head just to avoid queue prepending
    def loop(head: Option[ThreadT[M, _]], work: Queue[ThreadT[M, _]]): M[Unit] =
      head.map(h => (h, work)).orElse(work.dequeueOption) match {
        case Some((head, tail)) =>
          import ThreadF._

          head.resume flatMap {
            case Left(Fork(left, right)) =>
              loop(Some(left), tail.enqueue(right))

            case Left(Cede(results)) =>
              loop(None, tail.enqueue(results))

            case Left(Done) | Right(_) =>
              loop(None, tail)
          }

        case None =>
          Monad[M].unit
      }

    loop(Some(main), Queue.empty)
  }
}
