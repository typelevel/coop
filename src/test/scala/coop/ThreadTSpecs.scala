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
import cats.data.WriterT
import cats.implicits._

import org.specs2.mutable.Specification

class ThreadTSpecs extends Specification {

  "cooperative threading" should {
    "interleave writer tells" in {
      type M[A] = WriterT[Eval, List[Int], A]

      def writeRange(from: Int, to: Int): ThreadT[M, Unit] =
        from.until(to).toList traverse_ { i =>
          ThreadT.liftF[M, Unit](WriterT.tell(i :: Nil)) >> ThreadT.cede
        }

      val main = ThreadT.start(writeRange(0, 10)) >>
        ThreadT.start(writeRange(10, 20)) >>
        ThreadT.start(writeRange(20, 30))

      val (results, completed) = ThreadT.roundRobin(main).run.value

      completed must beTrue

      results mustEqual List(
        0, 10, 20,
        1, 11, 21,
        2, 12, 22,
        3, 13, 23,
        4, 14, 24,
        5, 15, 25,
        6, 16, 26,
        7, 17, 27,
        8, 18, 28,
        9, 19, 29)
    }

    "notify all monitors in await order" in {
      import ThreadT._

      type M[A] = WriterT[Eval, List[Int], A]

      val main = for {
        m <- monitor[M]

        _ <- start {
          ThreadT.await[M](m) >> liftF(WriterT.tell(0 :: Nil))
        }

        _ <- start {
          ThreadT.await[M](m) >> liftF(WriterT.tell(1 :: Nil))
        }

        _ <- start {
          ThreadT.await[M](m) >> liftF(WriterT.tell(2 :: Nil))
        }

        _ <- cede[M]
        _ <- ThreadT.notify[M](m)
        _ <- liftF[M, Unit](WriterT.tell(3 :: Nil))
      } yield ()

      val (results, completed) = roundRobin(main).run.value

      completed must beTrue
      results mustEqual 0.to(3).toList
    }

    "detect a trivial deadlock" in {
      val main = for {
        m <- ThreadT.monitor[Eval]
        _ <- ThreadT.start(ThreadT.await[Eval](m))
        _ <- ThreadT.cede[Eval]
      } yield ()

      ThreadT.roundRobin(main).value must beFalse
    }
  }
}
