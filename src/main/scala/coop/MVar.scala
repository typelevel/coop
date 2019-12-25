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

import cats.Applicative
import cats.data.Kleisli
import cats.implicits._

final class MVar[M[_]: Applicative, A] private () {
  import MVar._

  private[this] val Key = this.asInstanceOf[MVar[Any, Any]]

  val tryRead: MVarT[M, Option[A]] =
    Kleisli.ask[ThreadT[M, ?], MVarUniverse] map { universe =>
      universe()
        .get(Key)
        .map(_.asInstanceOf[A])
    }

  lazy val read: MVarT[M, A] =
    tryRead flatMap {
      case Some(a) => a.pure[MVarT[M, ?]]
      case None => cede >> read
    }

  def tryPut(a: A): MVarT[M, Boolean] =
    Kleisli.ask[ThreadT[M, ?], MVarUniverse] map { universe =>
      val u = universe()

      u.get(Key) match {
        case Some(_) =>
          false

        case None =>
          universe() = u + (Key -> a.asInstanceOf[Any])
          true
      }
    }

  def put(a: A): MVarT[M, Unit] =
    tryPut(a) flatMap { p =>
      if (p)
        ().pure[MVarT[M, ?]]
      else
        cede >> put(a)
    }

  val tryTake: MVarT[M, Option[A]] =
    Kleisli.ask[ThreadT[M, ?], MVarUniverse] map { universe =>
      val u = universe()
      u.get(Key) match {
        case Some(a) =>
          universe() = u - Key
          Some(a.asInstanceOf[A])

        case None =>
          None
      }
    }

  lazy val take: MVarT[M, A] =
    tryTake flatMap {
      case Some(a) => a.pure[MVarT[M, ?]]
      case None => cede >> take
    }

  def swap(a: A): MVarT[M, A] =
    Kleisli.ask[ThreadT[M, ?], MVarUniverse] flatMap { universe =>
      val u = universe()

      u.get(Key) match {
        case Some(oldA) =>
          universe() = u.updated(Key, a.asInstanceOf[Any])
          oldA.asInstanceOf[A].pure[MVarT[M, ?]]

        case None =>
          cede >> swap(a)
      }
    }

  private[this] val cede = Kleisli.liftF[ThreadT[M, ?], MVarUniverse, Unit](ThreadT.cede(()))
}

object MVar {
  // we use a kleisli of a ref of a map here rather than StateT to avoid issues with zeros in M
  // the Any(s) are required due to the existentiality of the A types
  type MVarUniverse = UnsafeRef[Map[MVar[Any, Any], Any]]
  type MVarT[M[_], A] = Kleisli[ThreadT[M, ?], MVarUniverse, A]

  def empty[M[_]: Applicative, A]: MVarT[M, MVar[M, A]] =
    new MVar[M, A].pure[MVarT[M, ?]]

  def apply[M[_]: Applicative, A](a: A): MVarT[M, MVar[M, A]] =
    empty[M, A].flatMap(mv => mv.put(a).as(mv))

  def resolve[M[_], A](mvt: MVarT[M, A]): ThreadT[M, A] =
    mvt.run(new UnsafeRef(Map[MVar[Any, Any], Any]()))
}
