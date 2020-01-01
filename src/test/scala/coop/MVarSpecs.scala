/*
 * Copyright 2020 Daniel Spiewak
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

import cats.{Eval, Monoid}
import cats.data.{Kleisli, State}
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._

import org.specs2.mutable.Specification

class MVarSpecs extends Specification {
  import FreeTInstances._

  type F[S, A] = Kleisli[ThreadT[State[S, ?], ?], MVar.Universe, A]

  "mvar" should {
    "put and read values" in {
      val eff = for {
        v0 <- MVar.empty[F[Int, ?], Int]
        v = v0[F[Int, ?]]

        _ <- v.put(42)
        i <- v.read

        _ <- MonadState[F[Int, ?], Int].set(i)
      } yield ()

      runToCompletionEmpty(eff) mustEqual 42
    }

    "return None on tryRead for an empty var" in {
      val eff = for {
        v <- MVar.empty[F[Option[Int], ?], Int]
        r <- v.tryRead[F[Option[Int], ?]]
        _ <- MonadState[F[Option[Int], ?], Option[Int]].set(r)
      } yield ()

      runToCompletionEmpty(eff) must beNone
    }

    "return false for tryPut on a full bar" in {
      val eff = for {
        v <- MVar[F[Boolean, ?], Int](42)
        r <- v.tryPut[F[Boolean, ?]](12)
        _ <- MonadState[F[Boolean, ?], Boolean].set(r)
      } yield ()

      runToCompletion(true, eff) must beFalse
    }

    "remove the value on take" in {
      val eff = for {
        v0 <- MVar[F[(Int, Option[Int]), ?], Int](42)
        v = v0[F[(Int, Option[Int]), ?]]

        r1 <- v.take
        r2 <- v.tryRead

        _ <- MonadState[F[(Int, Option[Int]), ?], (Int, Option[Int])].set((r1, r2))
      } yield ()

      runToCompletionEmpty(eff) mustEqual ((42, None))
    }

    "replace the value on swap" in {
      val eff = for {
        v0 <- MVar[F[(Int, Int), ?], Int](42)
        v = v0[F[(Int, Int), ?]]

        r1 <- v.swap(24)
        r2 <- v.read

        _ <- MonadState[F[(Int, Int), ?], (Int, Int)].set((r1, r2))
      } yield ()

      runToCompletionEmpty(eff) mustEqual ((42, 24))
    }

    "resolve a race condition" in {
      val thread = ApplicativeThread[F[(Either[Int, Int], Int), ?]]
      val state = MonadState[F[(Either[Int, Int], Int), ?], (Either[Int, Int], Int)]

      val eff = for {
        v0 <- MVar.empty[F[(Either[Int, Int], Int), ?], Int]
        v = v0[F[(Either[Int, Int], Int), ?]]

        results0 <- MVar.empty[F[(Either[Int, Int], Int), ?], Either[Int, Int]]
        results = results0[F[(Either[Int, Int], Int), ?]]

        _ <- thread start {
          v.tryPut(5).ifM(
            ().pure[F[(Either[Int, Int], Int), ?]],
            results.put(Left(5)))
        }

        _ <- thread start {
          v.tryPut(8).ifM(
            ().pure[F[(Either[Int, Int], Int), ?]],
            results.put(Right(8)))
        }

        r1 <- v.read
        r2 <- results.read

        _ <- state.set((r2, r1))
      } yield ()

      val results = runToCompletionEmpty(eff)

      (results mustEqual ((Left(5), 8))) or (results mustEqual ((Right(8), 5)))
    }

    "detect a deadlock" in {
      type F[A] = Kleisli[ThreadT[Eval, ?], MVar.Universe, A]

      val eff = MVar.empty[F, Unit].flatMap(_.read[F])
      ThreadT.roundRobin(MVar.resolve(eff)).value must beFalse
    }
  }

  def runToCompletionEmpty[S: Monoid](fa: F[S, _]): S =
    runToCompletion(Monoid[S].empty, fa)

  def runToCompletion[S](init: S, fa: F[S, _]): S =
    ThreadT.roundRobin(MVar.resolve(fa)).runS(init).value
}
