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

import cats.Applicative
import cats.free.FreeT

import ThreadF._

final class Ref[A] private[coop] (private[coop] val monitorId: MonitorId) { self =>
  def get[F[_]: Applicative]: ThreadT[F, A] =
    modify(a => (a, a))

  def set[F[_]: Applicative](a: A): ThreadT[F, Unit] =
    modify(_ => (a, ()))
  
  def modify[F[_]: Applicative, B](f: A => (A, B)): ThreadT[F, B] =
    FreeT.liftF(ModifyRef(this, f, identity[B]))

  def getAndSet[F[_]: Applicative](a: A): ThreadT[F, A] =
    modify(oldA => (a, oldA))

  def getAndUpdate[F[_]: Applicative](f: A => A): ThreadT[F, A] =
    modify(a => (f(a), a))

  def updateAndGet[F[_]: Applicative](f: A => A): ThreadT[F, A] =
    modify { a =>
      val newA = f(a)
      (newA, newA)
    }

  def apply[F[_]: Applicative]: RefPartiallyApplied[F] =
    new RefPartiallyApplied[F]

  class RefPartiallyApplied[F[_]: Applicative] {
    val get: ThreadT[F, A] = self.get
    def set(a: A): ThreadT[F, Unit] = self.set(a)
    def modify[B](f: A => (A, B)): ThreadT[F, B] = self.modify(f)
    def getAndSet(a: A): ThreadT[F, A] = self.getAndSet(a)
    def getAndUpdate(f: A => A): ThreadT[F, A] = self.getAndUpdate(f)
    def updateAndGet(f: A => A): ThreadT[F, A] = self.updateAndGet(f)
  }
}

object Ref {
  def of[F[_]: Applicative, A](a: A): ThreadT[F, Ref[A]] =
    ThreadT.monitor[F].flatMap(id => FreeT.liftF(MkRef(a, id, identity[Ref[A]])))
}
