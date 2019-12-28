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

import cats.{~>, Functor, Monad}
import cats.free.FreeT
import cats.mtl.lifting.MonadLayerControl

import scala.util.Either

// TODO where does this belong? cats-mtl?
object FreeTInstances {

  implicit def freeTMonadLayerControl[S[_]: Functor, M[_]: Monad]
      : MonadLayerControl.Aux[FreeT[S, M, ?], M, λ[α => Either[S[FreeT[S, M, α]], α]]] =
    new MonadLayerControl[FreeT[S, M, ?], M] {
      type State[A] = Either[S[FreeT[S, M, A]], A]

      val outerInstance: Monad[FreeT[S, M, ?]] =
        FreeT.catsFreeMonadForFreeT

      val innerInstance: Monad[M] = Monad[M]

      def layerMapK[A](ma: FreeT[S, M, A])(trans: M ~> M): FreeT[S, M, A] =
        ma.mapK(trans)

      def layer[A](inner: M[A]): FreeT[S, M, A] =
        FreeT.liftT(inner)

      def restore[A](state: State[A]): FreeT[S, M, A] =
        state.fold(FreeT.roll(_), FreeT.pure(_))

      def layerControl[A](cps: (FreeT[S, M, ?] ~> λ[α => M[State[α]]]) => M[A]): FreeT[S, M, A] =
        FreeT.liftT(cps(λ[FreeT[S, M, ?] ~> λ[α => M[State[α]]]](_.resume)))

      def zero[A](state: State[A]): Boolean = false
    }
}
