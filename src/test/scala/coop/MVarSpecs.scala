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

import cats.Eval
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
        v <- MVar.empty[F[Int, ?], Int]
        _ <- v.put[F[Int, ?]](42)
        i <- v.read[F[Int, ?]]
        _ <- MonadState[F[Int, ?], Int].set(i)
      } yield ()

      ThreadT.roundRobin(MVar.resolve(eff)).runS(0).value mustEqual 42
    }
  }
}
