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
  import ThreadF._

  def liftF[M[_]: Functor, A](ma: M[A]): ThreadT[M, A] =
    FreeT.liftT[ThreadF, M, A](ma)

  def fork[M[_]: Applicative, A](left: A, right: A): ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](Fork(left, right))

  def cede[M[_]: Applicative]: ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Cede(()))

  def done[M[_]: Applicative, A]: ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](Done)

  def monitor[M[_]: Applicative]: ThreadT[M, MonitorId] =
    FreeT.liftF[ThreadF, M, MonitorId](Monitor(m => m))

  def await[M[_]: Applicative](id: MonitorId): ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Await(id, ()))

  def notify[M[_]: Applicative](id: MonitorId): ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Notify(id, ()))

  def start[M[_]: Applicative, A](child: ThreadT[M, A]): ThreadT[M, Unit] =
    fork[M, Boolean](false, true).ifM(child >> done[M, Unit], ().pure[ThreadT[M, ?]])

  def roundRobin[M[_]: Monad, A](main: ThreadT[M, A]): M[Boolean] = {
    // we maintain a separate head just to avoid queue prepending
    def loop(
        head: Option[ThreadT[M, _]],
        work: Queue[ThreadT[M, _]],
        locks: Map[MonitorId, Queue[ThreadT[M, _]]])
        : M[Boolean] =
      head.map(h => (h, work)).orElse(work.dequeueOption) match {
        case Some((head, tail)) =>
          head.resume flatMap {
            case Left(Fork(left, right)) =>
              loop(Some(left), tail.enqueue(right), locks)

            case Left(Cede(results)) =>
              loop(None, tail.enqueue(results), locks)

            case Left(Done) | Right(_) =>
              loop(None, tail, locks)

            case Left(Monitor(f)) =>
              val id = new MonitorId()
              loop(Some(f(id)), tail, locks + (id -> Queue.empty))

            case Left(Await(id, results)) =>
              loop(None, tail, locks.updated(id, locks(id).enqueue(results)))

            case Left(Notify(id, results)) =>
              // enqueueAll was added in 2.13
              val tail2 = locks(id).foldLeft(tail)(_.enqueue(_))
              loop(None, tail2.enqueue(results), locks.updated(id, Queue.empty))
          }

        // if we have outstanding awaits but no active fibers, then we're deadlocked
        case None =>
          locks.forall(_._2.isEmpty).pure[M]
      }

    loop(Some(main), Queue.empty, Map.empty)
  }
}
