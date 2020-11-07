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

import cats.{Applicative, InjectK, Monad}
import cats.data.{EitherT, Kleisli}
import cats.free.FreeT
import cats.syntax.all._

import ThreadF.MonitorId

trait ApplicativeThread[F[_]] extends Serializable {
  val applicative: Applicative[F]

  def fork[A](left: => A, right: => A): F[A]

  val cede: F[Unit]

  def done[A]: F[A]

  val monitor: F[MonitorId]

  def await(id: MonitorId): F[Unit]

  def notify(id: MonitorId): F[Unit]

  def start[A](child: F[A]): F[Unit]

  def annotate[A](name: String, indent: Boolean = false)(body: F[A]): F[A]

  def refOf[A](a: A): F[Ref[A]]

  def modify[A, B](ref: Ref[A], f: A => (A, B)): F[B]
}

// NB it doesn't really make sense to define this for WriterT or StateT due to the value loss in start/fork
object ApplicativeThread {

  def apply[F[_]](implicit F: ApplicativeThread[F]): ApplicativeThread[F] = F

  implicit def forThreadFCapable[
      F[_]: Applicative,
      S[_]](
      implicit S: InjectK[ThreadF, S])
      : ApplicativeThread[FreeT[S, F, *]] =
    new ApplicativeThread[FreeT[S, F, *]] {

      val applicative = Applicative[FreeT[S, F, *]]

      def fork[A](left: => A, right: => A): FreeT[S, F, A] =
        FreeT.liftF(S(ThreadF.Fork(() => left, () => right)))

      val cede: FreeT[S, F, Unit] =
        FreeT.liftF(S(ThreadF.Cede(() => ())))

      def done[A]: FreeT[S, F, A] =
        FreeT.liftF(S(ThreadF.Done))

      val monitor: FreeT[S, F, MonitorId] =
        FreeT.liftF(S(ThreadF.Monitor(m => m)))

      def await(id: MonitorId): FreeT[S, F, Unit] =
        FreeT.liftF(S(ThreadF.Await(id, () => ())))

      def notify(id: MonitorId): FreeT[S, F, Unit] =
        FreeT.liftF(S(ThreadF.Notify(id, () => ())))

      def start[A](child: FreeT[S, F, A]): FreeT[S, F, Unit] =
        fork(false, true).ifM(child.void >> done[Unit], ().pure[FreeT[S, F, *]])

      def annotate[A](name: String, indent: Boolean)(body: FreeT[S, F, A]): FreeT[S, F, A] =
        FreeT.liftF(S(ThreadF.Annotate(name, () => ()))) *> {
          if (indent)
            FreeT.liftF(S(ThreadF.Indent(() => ()))) *> body <* FreeT.liftF(S(ThreadF.Dedent(() => ())))
          else
            body
        }

      def refOf[A](a: A): FreeT[S, F, Ref[A]] =
        monitor.flatMap(id => FreeT.liftF(S(ThreadF.MkRef(a, id, identity[Ref[A]]))))

      def modify[A, B](ref: Ref[A], f: A => (A, B)): FreeT[S, F, B] =
        FreeT.liftF(S(ThreadF.ModifyRef(ref, f, identity[B])))
    }

  implicit def forKleisli[F[_]: Monad: ApplicativeThread, R]: ApplicativeThread[Kleisli[F, R, *]] =
    new ApplicativeThread[Kleisli[F, R, *]] {
      private val thread = ApplicativeThread[F]

      val applicative = Applicative[Kleisli[F, R, *]]

      def fork[A](left: => A, right: => A): Kleisli[F, R, A] =
        Kleisli.liftF(thread.fork(left, right))

      val cede: Kleisli[F, R, Unit] =
        Kleisli.liftF(thread.cede)

      def done[A]: Kleisli[F, R, A] =
        Kleisli.liftF(thread.done[A])

      val monitor: Kleisli[F, R, MonitorId] =
        Kleisli.liftF(thread.monitor)

      def await(id: MonitorId): Kleisli[F, R, Unit] =
        Kleisli.liftF(thread.await(id))

      def notify(id: MonitorId): Kleisli[F, R, Unit] =
        Kleisli.liftF(thread.notify(id))

      def start[A](child: Kleisli[F, R, A]): Kleisli[F, R, Unit] =
        Kleisli.ask[F, R] flatMapF { r =>
          thread.start(child.run(r))
        }

      def annotate[A](name: String, indent: Boolean)(body: Kleisli[F, R, A]): Kleisli[F, R, A] =
        Kleisli { r => thread.annotate(name, indent)(body.run(r)) }

      def refOf[A](a: A): Kleisli[F, R, Ref[A]] =
        Kleisli.liftF(thread.refOf(a))

      def modify[A, B](ref: Ref[A], f: A => (A, B)): Kleisli[F, R, B] =
        Kleisli.liftF(thread.modify(ref, f))
    }

  implicit def forEitherT[F[_]: Monad: ApplicativeThread, E]: ApplicativeThread[EitherT[F, E, *]] =
    new ApplicativeThread[EitherT[F, E, *]] {
      private val thread = ApplicativeThread[F]

      val applicative = Applicative[EitherT[F, E, *]]

      def fork[A](left: => A, right: => A): EitherT[F, E, A] =
        EitherT.liftF(thread.fork(left, right))

      val cede: EitherT[F, E, Unit] =
        EitherT.liftF(thread.cede)

      def done[A]: EitherT[F, E, A] =
        EitherT.liftF(thread.done[A])

      val monitor: EitherT[F, E, MonitorId] =
        EitherT.liftF(thread.monitor)

      def await(id: MonitorId): EitherT[F, E, Unit] =
        EitherT.liftF(thread.await(id))

      def notify(id: MonitorId): EitherT[F, E, Unit] =
        EitherT.liftF(thread.notify(id))

      def start[A](child: EitherT[F, E, A]): EitherT[F, E, Unit] =
        EitherT.liftF(thread.start(child.value))

      def annotate[A](name: String, indent: Boolean)(body: EitherT[F, E, A]): EitherT[F, E, A] =
        EitherT(thread.annotate(name, indent)(body.value))

      def refOf[A](a: A): EitherT[F, E, Ref[A]] =
        EitherT.liftF(thread.refOf(a))

      def modify[A, B](ref: Ref[A], f: A => (A, B)): EitherT[F,E,B] =
        EitherT.liftF(thread.modify(ref, f))
    }
}
