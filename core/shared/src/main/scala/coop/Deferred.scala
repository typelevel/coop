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

import cats.Monad
import cats.syntax.all._

import ThreadF.MonitorId

final class Deferred[A] private[coop] (private[coop] val monitorId: MonitorId) { self =>
  def tryGet[F[_]: ApplicativeThread]: F[Option[A]] =
    ApplicativeThread[F].deferredTryGet(this)

  def get[F[_]: Monad: ApplicativeThread]: F[A] =
    tryGet[F].flatMap {
      case Some(a) => Monad[F].pure(a)
      case None => ApplicativeThread[F].await(monitorId) >> get[F]
    }

  def complete[F[_]: Monad: ApplicativeThread](a: A): F[Unit] =
    ApplicativeThread[F].deferredComplete(this, a) >> ApplicativeThread[F].notify(monitorId)

  def apply[F[_]: Monad: ApplicativeThread]: DeferredPartiallyApplied[F] =
    new DeferredPartiallyApplied[F]

  class DeferredPartiallyApplied[F[_]: Monad: ApplicativeThread] {
    def tryGet: F[Option[A]] = self.tryGet
    def get: F[A] = self.get
    def complete(a: A): F[Unit] = self.complete(a)
  }
}

object Deferred {
  def apply[F[_]: ApplicativeThread, A]: F[Deferred[A]] =
    ApplicativeThread[F].deferred
}
