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
import cats.syntax.all._

import org.specs2.mutable.Specification

class DeferredSpecs extends Specification {
  "Deferred" should {
    "complete" in {
      val eff = for {
        d <- Deferred[State[Int, *], Int]
        dp = d[State[Int, *]]
        _ <- dp.complete(10)
        v <- dp.get
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual 10
    }

    "complete only once" in {
      val eff = for {
        d <- Deferred[State[Int, *], Int]
        dp = d[State[Int, *]]
        _ <- dp.complete(10)
        _ <- dp.complete(20)
        v <- dp.get
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual 10
    }

    "tryGet returns None for unset Deferred" in {
      val eff = for {
        d <- Deferred[State[Option[Int], *], Int]
        dp = d[State[Option[Int], *]]
        v <- dp.tryGet
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual None
    }

    "tryGet returns Some for set Deferred" in {
      val eff = for {
        d <- Deferred[State[Option[Int], *], Int]
        dp = d[State[Option[Int], *]]
        _ <- dp.complete(10)
        v <- dp.tryGet
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual Some(10)
    }

    "get blocks until set" in {
      val eff = for {
        state <- Ref.of[State[Int, *], Int](0)
        modifyGate <- Deferred[State[Int, *], Unit]
        readGate <- Deferred[State[Int, *], Unit]
        _ <- ThreadT.start(modifyGate.get[State[Int, *]] *> state.updateAndGet[State[Int, *]](_ * 2) *> readGate.complete[State[Int, *]](()))
        _ <- ThreadT.start(state.set[State[Int, *]](1) *> modifyGate.complete[State[Int, *]](()))
        _ <- readGate.get[State[Int, *]]
        v <- state.get[State[Int, *]]
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual 2
    }
  }

  def runToCompletionEmpty[S: Monoid](fa: ThreadT[State[S, *], _]): S =
    runToCompletion(Monoid[S].empty, fa)

  def runToCompletion[S](init: S, fa: ThreadT[State[S, *], _]): S =
    ThreadT.roundRobin(fa).runS(init).value
}
