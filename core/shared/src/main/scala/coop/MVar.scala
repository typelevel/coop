/*
 * Copyright 2021 Typelevel
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

import cats.{Applicative, FlatMap, Monad}
import cats.data.StateT
import cats.syntax.all._

import ThreadF.MonitorId

final class MVar[A] private (monitor: MonitorId) { outer =>

  import MVar.Action

  private[this] val Key = this.asInstanceOf[MVar[Any]]

  def tryRead[F[_]: Applicative]: Action[F, Option[A]] = getU[F]

  def read[F[_]: Monad: ApplicativeThread]: Action[F, A] =
    tryRead[F] flatMap {
      case Some(a) => StateT.pure(a)
      case None => StateT.liftF[F, Map[MVar[Any], Any], Unit](ApplicativeThread[F].await(monitor)) >> read[F]
    }

  def tryPut[F[_]: Monad: ApplicativeThread](a: A): Action[F, Boolean] =
    getU[F] flatMap {
      case Some(_) =>
        StateT.pure(false)

      case None =>
        setU[F](a).as(true)
    }

  def put[F[_]: Monad: ApplicativeThread](a: A): Action[F, Unit] =
    tryPut[F](a).ifM(
      StateT.pure(()),
      StateT.liftF[F, Map[MVar[Any], Any], Unit](ApplicativeThread[F].await(monitor)) >> put[F](a)
    )

  def tryTake[F[_]: Monad: ApplicativeThread]: Action[F, Option[A]] =
    getU[F] flatMap {
      case Some(a) =>
        removeU[F].as(Some(a): Option[A])

      case None =>
        StateT.pure(None)
    }

  def take[F[_]: Monad: ApplicativeThread]: Action[F, A] =
    tryTake[F] flatMap {
      case Some(a) => StateT.pure(a)
      case None => StateT.liftF[F, Map[MVar[Any], Any], Unit](ApplicativeThread[F].await(monitor)) >> take[F]
    }

  def swap[F[_]: Monad: ApplicativeThread](a: A): Action[F, A] =
    getU[F] flatMap {
      case Some(oldA) =>
        setU[F](a).as(oldA)

      case None =>
        StateT.liftF[F, Map[MVar[Any], Any], Unit](ApplicativeThread[F].await(monitor)) >> swap[F](a)
    }

  def apply[F[_]: Monad: ApplicativeThread]: MVarPartiallyApplied[F] =
    new MVarPartiallyApplied[F]

  class MVarPartiallyApplied[F[_]: Monad: ApplicativeThread] {

    val tryRead: Action[F, Option[A]] = outer.tryRead[F]

    val read: Action[F, A] = outer.read[F]

    def tryPut(a: A): Action[F, Boolean] = outer.tryPut[F](a)

    def put(a: A): Action[F, Unit] = outer.put[F](a)

    val tryTake: Action[F, Option[A]] = outer.tryTake[F]

    val take: Action[F, A] = outer.take[F]

    def swap(a: A): Action[F, A] = outer.swap[F](a)
  }

  private[this] def getU[F[_]: Applicative]: Action[F, Option[A]] =
    StateT.get[F, Map[MVar[Any], Any]].map(_.get(Key).map(_.asInstanceOf[A]))

  private[this] def setU[F[_]: Monad: ApplicativeThread](a: A): Action[F, Unit] =
    StateT.modify[F, Map[MVar[Any], Any]](_ + (Key -> a)) >> StateT.liftF(ApplicativeThread[F].notify(monitor))

  private[this] def removeU[F[_]: Monad: ApplicativeThread]: Action[F, Unit] =
    StateT.modify[F, Map[MVar[Any], Any]](_ - Key) >> StateT.liftF(ApplicativeThread[F].notify(monitor))
}

object MVar {
  type Action[F[_], A] = StateT[F, Map[MVar[Any], Any], A]

  def empty[F[_]: Applicative: ApplicativeThread, A]: Action[F, MVar[A]] =
    StateT.liftF(ApplicativeThread[F].monitor.map(new MVar[A](_))) // not actually pure due to object identity, but whatevs

  def apply[F[_]: Monad: ApplicativeThread, A](a: A): Action[F, MVar[A]] =
    empty[F, A].flatMap(mv => mv.put[F](a).as(mv))

  def resolve[F[_]: FlatMap, A](mvt: Action[F, A]): F[A] =
    mvt.runA(Map[MVar[Any], Any]())
}
