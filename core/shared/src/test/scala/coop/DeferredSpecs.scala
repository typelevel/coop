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

import cats.data.State
import cats.kernel.Monoid
import cats.mtl.Stateful
import cats.syntax.all._

import org.specs2.mutable.Specification

class DeferredSpecs extends Specification {
  import FreeTInstances._

  type F[S, A] = ThreadT[State[S, *], A]
  
  "Deferred" should {
    "complete" in {
      val eff = for {
        d <- Deferred[F[Int, *], Int]
        dp = d[F[Int, *]]
        _ <- dp.complete(10)
        v <- dp.get
        _ <- Stateful[F[Int, *], Int].set(v)
      } yield ()

      runToCompletionEmpty(eff) mustEqual 10
    }

    "complete only once" in {
      val eff = for {
        d <- Deferred[F[Int, *], Int]
        dp = d[F[Int, *]]
        _ <- dp.complete(10)
        _ <- dp.complete(20)
        v <- dp.get
        _ <- Stateful[F[Int, *], Int].set(v)
      } yield ()

      runToCompletionEmpty(eff) mustEqual 10
    }

    "tryGet returns None for unset Deferred" in {
      val eff = for {
        d <- Deferred[F[Option[Int], *], Int]
        dp = d[F[Option[Int], *]]
        v <- dp.tryGet
        _ <- Stateful[F[Option[Int], *], Option[Int]].set(v)
      } yield ()

      runToCompletionEmpty(eff) mustEqual None
    }

    "tryGet returns Some for set Deferred" in {
      val eff = for {
        d <- Deferred[F[Option[Int], *], Int]
        dp = d[F[Option[Int], *]]
        _ <- dp.complete(10)
        v <- dp.tryGet
        _ <- Stateful[F[Option[Int], *], Option[Int]].set(v)
      } yield ()

      runToCompletionEmpty(eff) mustEqual Some(10)
    }

    "get blocks until set" in {
      val eff = for {
        state <- Ref.of[F[Int, *], Int](0)
        modifyGate <- Deferred[F[Int, *], Unit]
        readGate <- Deferred[F[Int, *], Unit]
        _ <- ApplicativeThread[F[Int, *]].start(modifyGate.get[F[Int, *]] *> state.updateAndGet[F[Int, *]](_ * 2) *> readGate.complete[F[Int, *]](()))
        _ <- ApplicativeThread[F[Int, *]].start(state.set[F[Int, *]](1) *> modifyGate.complete[F[Int, *]](()))
        _ <- readGate.get[F[Int, *]]
        v <- state.get[F[Int, *]]
        _ <- Stateful[F[Int, *], Int].set(v)
      } yield ()

      runToCompletionEmpty(eff) mustEqual 2
    }
  }

  def runToCompletionEmpty[S: Monoid](fa: F[S, _]): S =
    runToCompletion(Monoid[S].empty, fa)

  def runToCompletion[S](init: S, fa: F[S, _]): S =
    ThreadT.roundRobin(fa).runS(init).value
}
