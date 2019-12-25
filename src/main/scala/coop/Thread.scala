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

object Thread {

  def liftF[M[_]: Functor, A](ma: M[A]): Thread[M, A] =
    FreeT.liftT[ThreadF, M, A](ma)

  def fork[M[_]: Applicative, A](left: A, right: A): Thread[M, A] =
    FreeT.liftF[ThreadF, M, A](ThreadF.Fork(left, right))

  def cede[M[_]: Applicative, A](results: A): Thread[M, A] =
    FreeT.liftF[ThreadF, M, A](ThreadF.Cede(results))

  def done[M[_]: Applicative, A]: Thread[M, A] =
    FreeT.liftF[ThreadF, M, A](ThreadF.Done)

  def start[M[_]: Applicative, A](child: Thread[M, A]): Thread[M, Unit] =
    fork[M, Boolean](false, true).ifM(child.void, ().pure[Thread[M, ?]])

  def roundRobin[M[_]: Monad, A](main: Thread[M, A]): M[Unit] = {
    def loop(work: Queue[Thread[M, _]]): M[Unit] =
      work.dequeueOption match {
        case Some((head, tail)) =>
          import ThreadF._

          head.resume flatMap {
            case Left(Fork(left, right)) =>
              loop(tail.enqueue(left).enqueue(right))   // treat forking as a yield

            case Left(Cede(results)) =>
              loop(tail.enqueue(results))

            case Left(Done) | Right(_) =>
              loop(tail)
          }

        case None =>
          Monad[M].unit
      }

    loop(Queue(main))
  }
}
