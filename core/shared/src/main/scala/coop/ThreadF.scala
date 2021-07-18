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

import cats.Functor

sealed trait ThreadF[+A] extends Product with Serializable

object ThreadF {

  implicit val functor: Functor[ThreadF] = new Functor[ThreadF] {
    def map[A, B](fa: ThreadF[A])(f: A => B): ThreadF[B] = fa match {
      case Fork(left, right) => Fork(() => f(left()), () => f(right()))
      case Cede(results) => Cede(() => f(results()))
      case Done => Done

      case Monitor(body) => Monitor(body.andThen(f))
      case Await(id, results) => Await(id, () => f(results()))
      case Notify(id, results) => Notify(id, () => f(results()))

      case Annotate(text, results) => Annotate(text, () => f(results()))
      case Indent(results) => Indent(() => f(results()))
      case Dedent(results) => Dedent(() => f(results()))

      case MkRef(a, id, body) => MkRef(a, id, body.andThen(f))
      case ModifyRef(ref, modF, body) => ModifyRef(ref, modF, body.andThen(f))

      case MkDeferred(id, body) => MkDeferred(id, body.andThen(f))
      case TryGetDeferred(deferred, body) => TryGetDeferred(deferred, body.andThen(f))
      case CompleteDeferred(deferred, a, body) => CompleteDeferred(deferred, a, () => f(body()))
    }
  }

  final case class Fork[A](left: () => A, right: () => A) extends ThreadF[A]
  final case class Cede[A](results: () => A) extends ThreadF[A]
  case object Done extends ThreadF[Nothing]

  final case class Monitor[A](body: MonitorId => A) extends ThreadF[A]
  final case class Await[A](id: MonitorId, results: () => A) extends ThreadF[A]
  final case class Notify[A](id: MonitorId, results: () => A) extends ThreadF[A]

  final case class Annotate[A](text: String, results: () => A) extends ThreadF[A]
  final case class Indent[A](results: () => A) extends ThreadF[A]
  final case class Dedent[A](results: () => A) extends ThreadF[A]

  final case class MkRef[A, B](a: A, id: MonitorId, body: Ref[A] => B) extends ThreadF[B]
  final case class ModifyRef[A, B, C](ref: Ref[A], f: A => (A, B), body: B => C) extends ThreadF[C]

  final case class MkDeferred[A, B](id: MonitorId, body: Deferred[A] => B) extends ThreadF[B]
  final case class TryGetDeferred[A, B](deferred: Deferred[A], body: Option[A] => B) extends ThreadF[B]
  final case class CompleteDeferred[A, B](deferred: Deferred[A], a: A, body: () => B) extends ThreadF[B]
  
  // an opaque fresh id
  final case class MonitorId private[coop] (private[coop] val id: Int)
  private object MonitorId
}
