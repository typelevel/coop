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

import cats.{Functor, Monad}
import cats.data.Kleisli
import cats.implicits._
import cats.mtl.Ask

import ThreadF.MonitorId

final class MVar[A] private (monitor: MonitorId) { outer =>

  private[this] val Key = this.asInstanceOf[MVar[Any]]

  def tryRead[F[_]: Functor: MVar.Ask]: F[Option[A]] = getU[F]

  def read[F[_]: Monad: ApplicativeThread: MVar.Ask]: F[A] =
    tryRead[F] flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].await(monitor) >> read[F]
    }

  def tryPut[F[_]: Monad: ApplicativeThread: MVar.Ask](a: A): F[Boolean] =
    getU[F] flatMap {
      case Some(_) =>
        false.pure[F]

      case None =>
        setU[F](a).as(true)
    }

  def put[F[_]: Monad: ApplicativeThread: MVar.Ask](a: A): F[Unit] =
    tryPut[F](a).ifM(().pure[F], ApplicativeThread[F].await(monitor) >> put[F](a))

  def tryTake[F[_]: Monad: ApplicativeThread: MVar.Ask]: F[Option[A]] =
    getU[F] flatMap {
      case Some(a) =>
        removeU[F].as(Some(a): Option[A])

      case None =>
        (None: Option[A]).pure[F]
    }

  def take[F[_]: Monad: ApplicativeThread: MVar.Ask]: F[A] =
    tryTake[F] flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].await(monitor) >> take[F]
    }

  def swap[F[_]: Monad: ApplicativeThread: MVar.Ask](a: A): F[A] =
    getU[F] flatMap {
      case Some(oldA) =>
        setU[F](a).as(oldA)

      case None =>
        ApplicativeThread[F].await(monitor) >> swap[F](a)
    }

  def apply[F[_]: Monad: ApplicativeThread: MVar.Ask]: MVarPartiallyApplied[F] =
    new MVarPartiallyApplied[F]

  class MVarPartiallyApplied[F[_]: Monad: ApplicativeThread: MVar.Ask] {

    val tryRead: F[Option[A]] = outer.tryRead[F]

    val read: F[A] = outer.read[F]

    def tryPut(a: A): F[Boolean] = outer.tryPut[F](a)

    def put(a: A): F[Unit] = outer.put[F](a)

    val tryTake: F[Option[A]] = outer.tryTake[F]

    val take: F[A] = outer.take[F]

    def swap(a: A): F[A] = outer.swap[F](a)
  }

  private[this] def getU[F[_]: Functor: MVar.Ask]: F[Option[A]] =
    Ask[F, MVar.Universe].ask.map(_().get(Key).map(_.asInstanceOf[A]))

  private[this] def setU[F[_]: Monad: MVar.Ask: ApplicativeThread](a: A): F[Unit] =
    Ask[F, MVar.Universe].ask.map(_() += (Key -> a.asInstanceOf[Any])) >>
      ApplicativeThread[F].notify(monitor)

  private[this] def removeU[F[_]: Monad: MVar.Ask: ApplicativeThread]: F[Unit] =
    Ask[F, MVar.Universe].ask.map(_() -= Key) >>
      ApplicativeThread[F].notify(monitor)
}

object MVar {
  // we use a kleisli of a ref of a map here rather than StateT to avoid issues with zeros in F
  // the Any(s) are required due to the existentiality of the A types
  type Universe = UnsafeRef[Map[MVar[Any], Any]]
  type Ask[F[_]] = cats.mtl.Ask[F, Universe]

  def empty[F[_]: Functor: ApplicativeThread, A]: F[MVar[A]] =
    ApplicativeThread[F].monitor.map(new MVar[A](_))    // not actually pure due to object identity, but whatevs

  def apply[F[_]: Monad: ApplicativeThread: Ask, A](a: A): F[MVar[A]] =
    empty[F, A].flatMap(mv => mv.put[F](a).as(mv))

  def resolve[F[_], A](mvt: Kleisli[F, Universe, A]): F[A] =
    mvt.run(new UnsafeRef(Map[MVar[Any], Any]()))
}
