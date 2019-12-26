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

import cats.Monad
import cats.data.Kleisli
import cats.implicits._
import cats.mtl.ApplicativeAsk

final class MVar[F[_]: Monad: ApplicativeThread: ApplicativeAsk[?[_], MVar.Universe], A] private () {
  import MVar._

  private[this] val Key = this.asInstanceOf[MVar[Any, Any]]

  private[this] val getU: F[Option[A]] =
    ApplicativeAsk[F, Universe].ask.map(_().get(Key).map(_.asInstanceOf[A]))

  private[this] def setU(a: A): F[Unit] =
    ApplicativeAsk[F, Universe].ask.map(_() + (Key -> a.asInstanceOf[Any]))

  private[this] val removeU: F[Unit] =
    ApplicativeAsk[F, Universe].ask.map(_() - Key)

  val tryRead: F[Option[A]] = getU

  lazy val read: F[A] =
    tryRead flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].cede_ >> read
    }

  def tryPut(a: A): F[Boolean] =
    getU flatMap {
      case Some(_) =>
        false.pure[F]

      case None =>
        setU(a).as(true)
    }

  def put(a: A): F[Unit] =
    tryPut(a) flatMap { p =>
      if (p)
        ().pure[F]
      else
        ApplicativeThread[F].cede_ >> put(a)
    }

  val tryTake: F[Option[A]] =
    getU flatMap {
      case Some(a) =>
        removeU.as(Some(a): Option[A])

      case None =>
        (None: Option[A]).pure[F]
    }

  lazy val take: F[A] =
    tryTake flatMap {
      case Some(a) => a.pure[F]
      case None => ApplicativeThread[F].cede_ >> take
    }

  def swap(a: A): F[A] =
    getU flatMap {
      case Some(oldA) =>
        setU(a).as(oldA)

      case None =>
        ApplicativeThread[F].cede_ >> swap(a)
    }
}

object MVar {
  // we use a kleisli of a ref of a map here rather than StateT to avoid issues with zeros in F
  // the Any(s) are required due to the existentiality of the A types
  type Universe = UnsafeRef[Map[MVar[Any, Any], Any]]

  def empty[F[_]: Monad: ApplicativeThread: ApplicativeAsk[?[_], MVar.Universe], A]: F[MVar[F, A]] =
    new MVar[F, A].pure[F]

  def apply[F[_]: Monad: ApplicativeThread: ApplicativeAsk[?[_], MVar.Universe], A](a: A): F[MVar[F, A]] =
    empty[F, A].flatMap(mv => mv.put(a).as(mv))

  def resolve[F[_], A](mvt: Kleisli[F, Universe, A]): F[A] =
    mvt.run(new UnsafeRef(Map[MVar[Any, Any], Any]()))
}
