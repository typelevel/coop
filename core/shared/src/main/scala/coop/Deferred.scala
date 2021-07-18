/*
 * Copyright 2020 Typelevel
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

import cats.{Applicative, Monad}
import cats.free.FreeT
import cats.syntax.all._

import ThreadF._

final class Deferred[A] private[coop] (private[coop] val monitorId: MonitorId) { self =>
  def tryGet[F[_]: Applicative]: ThreadT[F, Option[A]] =
    FreeT.liftF(TryGetDeferred(this, identity[Option[A]]))

  def get[F[_]: Monad]: ThreadT[F, A] =
    tryGet[F].flatMap {
      case Some(a) => Applicative[ThreadT[F, *]].pure(a)
      case None => ThreadT.await(monitorId) >> get[F]
    }

  def complete[F[_]: Monad](a: A): ThreadT[F, Unit] =
    FreeT.liftF(CompleteDeferred(this, a, () => ()): ThreadF[Unit]) >> ThreadT.notify[F](monitorId)

  def apply[F[_]: Monad]: DeferredPartiallyApplied[F] =
    new DeferredPartiallyApplied[F]

  class DeferredPartiallyApplied[F[_]: Monad] {
    def tryGet: ThreadT[F, Option[A]] = self.tryGet
    def get: ThreadT[F, A] = self.get
    def complete(a: A): ThreadT[F, Unit] = self.complete(a)
  }
}

object Deferred {
  def apply[F[_]: Applicative, A]: ThreadT[F, Deferred[A]] =
    ThreadT.monitor[F].flatMap(id => FreeT.liftF(MkDeferred(id, identity[Deferred[A]])))
}
