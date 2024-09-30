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

import org.specs2.mutable.Specification

class RefSpecs extends Specification {
  "Ref" should {
    "get and set successfully" in {
      val eff = for {
        ref <- Ref.of[State[(Int, Int), *], Int](5)
        refp = ref[State[(Int, Int), *]]
        v1 <- refp.getAndSet(10)
        v2 <- refp.get
        _ <- ThreadT.liftF(State.set((v1, v2)))
      } yield ()

      runToCompletionEmpty(eff) mustEqual ((5, 10))
    }

    "get and update successfully" in {
      val eff = for {
        ref <- Ref.of[State[(Int, Int), *], Int](5)
        refp = ref[State[(Int, Int), *]]
        v1 <- refp.getAndUpdate(_ * 2)
        v2 <- refp.get
        _ <- ThreadT.liftF(State.set((v1, v2)))
      } yield ()

      runToCompletionEmpty(eff) mustEqual ((5, 10))
    }

    "update and get successfully" in {
      val eff = for {
        ref <- Ref.of[State[(Int, Int), *], Int](5)
        refp = ref[State[(Int, Int), *]]
        v1 <- refp.updateAndGet(_ * 2)
        v2 <- refp.get
        _ <- ThreadT.liftF(State.set((v1, v2)))
      } yield ()

      runToCompletionEmpty(eff) mustEqual ((10, 10))
    }

    "set from a background thread" in {
      val eff = for {
        ref <- Ref.of[State[Int, *], Int](5)
        refp = ref[State[Int, *]]
        _ <- ThreadT.start(refp.set(10))
        _ <- ThreadT.cede[State[Int, *]]
        v <- refp.get
        _ <- ThreadT.liftF(State.set(v))
      } yield ()

      runToCompletionEmpty(eff) mustEqual(10)
    }
  }

  def runToCompletionEmpty[S: Monoid](fa: ThreadT[State[S, *], _]): S =
    runToCompletion(Monoid[S].empty, fa)

  def runToCompletion[S](init: S, fa: ThreadT[State[S, *], _]): S =
    ThreadT.roundRobin(fa).runS(init).value
}
