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

import cats.Monad
import cats.free.FreeT
import cats.mtl.MonadPartialOrder

object FreeTInstances {

  implicit def monadPartialOrderForFreeT[F[_], M[_], S](
      implicit M: Monad[M])
      : MonadPartialOrder[M, FreeT[F, M, *]] =
    new MonadPartialOrder[M, FreeT[F, M, *]] {
      val monadF = M
      val monadG = FreeT.catsFreeMonadForFreeT[F, M]
      def apply[A](fa: M[A]) = FreeT.liftT(fa)
    }
}
