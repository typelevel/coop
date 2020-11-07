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

import cats.Functor

import ThreadF.MonitorId

final class Ref[A] private[coop] (private[coop] val monitorId: MonitorId) { self =>
  def get[F[_]: ApplicativeThread]: F[A] =
    modify(a => (a, a))

  def set[F[_]: ApplicativeThread](a: A): F[Unit] =
    modify(_ => (a, ()))
  
  def modify[F[_]: ApplicativeThread, B](f: A => (A, B)): F[B] =
    ApplicativeThread[F].refModify[A, B](this, f)

  def getAndSet[F[_]: ApplicativeThread](a: A): F[A] =
    modify(oldA => (a, oldA))

  def getAndUpdate[F[_]: ApplicativeThread](f: A => A): F[A] =
    modify(a => (f(a), a))

  def updateAndGet[F[_]: ApplicativeThread](f: A => A): F[A] =
    modify { a =>
      val newA = f(a)
      (newA, newA)
    }

  def apply[F[_]: ApplicativeThread]: RefPartiallyApplied[F] =
    new RefPartiallyApplied[F]

  class RefPartiallyApplied[F[_]: ApplicativeThread] {
    val get: F[A] = self.get
    def set(a: A): F[Unit] = self.set(a)
    def modify[B](f: A => (A, B)): F[B] = self.modify(f)
    def getAndSet(a: A): F[A] = self.getAndSet(a)
    def getAndUpdate(f: A => A): F[A] = self.getAndUpdate(f)
    def updateAndGet(f: A => A): F[A] = self.updateAndGet(f)
  }
}

object Ref {
  def of[F[_]: Functor: ApplicativeThread, A](a: A): F[Ref[A]] =
    ApplicativeThread[F].refOf(a)
}
