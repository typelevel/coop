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
import cats.data.Kleisli
import cats.implicits._
import cats.mtl.ApplicativeAsk

final class MVar[A] private () {

  private[this] val Key = this.asInstanceOf[MVar[Any]]

  def tryRead[F[_]: Functor: MVar.Ask]: F[Option[A]] = getU[F]

  def read[F[_]: Monad: ApplicativeThread: MVar.Ask]: F[A] =
    tryRead[F] flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].cede_ >> read[F]
    }

  def tryPut[F[_]: Monad: MVar.Ask](a: A): F[Boolean] =
    getU[F] flatMap {
      case Some(_) =>
        false.pure[F]

      case None =>
        setU[F](a).as(true)
    }

  def put[F[_]: Monad: ApplicativeThread: MVar.Ask](a: A): F[Unit] =
    tryPut[F](a) flatMap { p =>
      if (p)
        ().pure[F]
      else
        ApplicativeThread[F].cede_ >> put[F](a)
    }

  def tryTake[F[_]: Monad: MVar.Ask]: F[Option[A]] =
    getU[F] flatMap {
      case Some(a) =>
        removeU[F].as(Some(a): Option[A])

      case None =>
        (None: Option[A]).pure[F]
    }

  def take[F[_]: Monad: ApplicativeThread: MVar.Ask]: F[A] =
    tryTake[F] flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].cede_ >> take[F]
    }

  def swap[F[_]: Monad: ApplicativeThread: MVar.Ask](a: A): F[A] =
    getU[F] flatMap {
      case Some(oldA) =>
        setU[F](a).as(oldA)

      case None =>
        ApplicativeThread[F].cede_ >> swap[F](a)
    }

  private[this] def getU[F[_]: Functor: MVar.Ask]: F[Option[A]] =
    ApplicativeAsk[F, MVar.Universe].ask.map(_().get(Key).map(_.asInstanceOf[A]))

  private[this] def setU[F[_]: Functor: MVar.Ask](a: A): F[Unit] =
    ApplicativeAsk[F, MVar.Universe].ask.map(_() += (Key -> a.asInstanceOf[Any]))

  private[this] def removeU[F[_]: Functor: MVar.Ask]: F[Unit] =
    ApplicativeAsk[F, MVar.Universe].ask.map(_() - Key)
}

object MVar {
  // we use a kleisli of a ref of a map here rather than StateT to avoid issues with zeros in F
  // the Any(s) are required due to the existentiality of the A types
  type Universe = UnsafeRef[Map[MVar[Any], Any]]
  type Ask[F[_]] = ApplicativeAsk[F, Universe]

  def empty[F[_]: Applicative, A]: F[MVar[A]] =
    new MVar[A].pure[F]   // not actually pure due to object identity, but whatevs

  def apply[F[_]: Monad: ApplicativeThread: Ask, A](a: A): F[MVar[A]] =
    empty[F, A].flatMap(mv => mv.put[F](a).as(mv))

  def resolve[F[_], A](mvt: Kleisli[F, Universe, A]): F[A] =
    mvt.run(new UnsafeRef(Map[MVar[Any], Any]()))
}
