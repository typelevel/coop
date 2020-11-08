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

import cats.{~>, Applicative, Functor, Monad, Show}
import cats.data.Const
import cats.free.FreeT
import cats.implicits._

import scala.collection.immutable.Queue

object ThreadT {
  import ThreadF._

  def liftF[M[_]: Functor, A](ma: M[A]): ThreadT[M, A] =
    FreeT.liftT[ThreadF, M, A](ma)

  def fork[M[_]: Applicative, A](left: => A, right: => A): ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](Fork(() => left, () => right))

  def cede[M[_]: Applicative]: ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Cede(() => ()))

  def done[M[_]: Applicative, A]: ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, A](Done)

  def monitor[M[_]: Applicative]: ThreadT[M, MonitorId] =
    FreeT.liftF[ThreadF, M, MonitorId](Monitor(m => m))

  def await[M[_]: Applicative](id: MonitorId): ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Await(id, () => ()))

  def notify[M[_]: Applicative](id: MonitorId): ThreadT[M, Unit] =
    FreeT.liftF[ThreadF, M, Unit](Notify(id, () => ()))

  def start[M[_]: Applicative, A](child: ThreadT[M, A]): ThreadT[M, Unit] =
    fork[M, Boolean](false, true).ifM(child >> done[M, Unit], ().pure[ThreadT[M, *]])

  def annotate[M[_]: Applicative, A](name: String, indent: Boolean = false)(body: ThreadT[M, A]): ThreadT[M, A] =
    FreeT.liftF[ThreadF, M, Unit](Annotate(name, () => ())) *> {
      if (indent)
        FreeT.liftF[ThreadF, M, Unit](Indent(() => ())) *> body <* FreeT.liftF[ThreadF, M, Unit](Dedent(() => ()))
      else
        body
    }

  def roundRobin[M[_]: Monad, A](main: ThreadT[M, A]): M[Boolean] = {
    // we maintain a separate head just to avoid queue prepending
    case class LoopState(
        head: Option[() => ThreadT[M, _]],
        work: Queue[() => ThreadT[M, _]],
        monitorCount: Int,
        locks: Map[MonitorId, Queue[() => ThreadT[M, _]]],
        refs: Map[MonitorId, _],
        deferreds: Map[MonitorId, _]
    )

    Monad[M].tailRecM(LoopState(Some(() => main), Queue.empty, 0, Map.empty, Map.empty, Map.empty)) { ls =>
      val LoopState(head, work, count, locks, refs, deferreds) = ls

      head.tupleRight(work).orElse(work.dequeueOption) match {
        case Some((head, tail)) =>
          head().resume map {
            case Left(Fork(left, right)) =>
              Left(LoopState(Some(left), tail.enqueue(right), count, locks, refs, deferreds))

            case Left(Cede(results)) =>
              val tail2 = tail.enqueue(results)
              Left(LoopState(None, tail2, count, locks, refs, deferreds))

            case Left(Done) | Right(_) =>
              Left(LoopState(None, tail, count, locks, refs, deferreds))

            case Left(Monitor(f)) =>
              val id = new MonitorId(count)
              Left(LoopState(Some(() => f(id)), tail, count + 1, locks + (id -> Queue.empty), refs, deferreds))

            case Left(Await(id, results)) =>
              Left(LoopState(None, tail, count, locks.updated(id, locks(id).enqueue(results)), refs, deferreds))

            case Left(Notify(id, results)) =>
              // enqueueAll was added in 2.13
              val tail2 = locks(id).foldLeft(tail)(_.enqueue(_))
              Left(LoopState(None, tail2.enqueue(results), count, locks.updated(id, Queue.empty), refs, deferreds))

            case Left(Annotate(_, results)) =>
              Left(LoopState(Some(results), tail, count, locks, refs, deferreds))

            case Left(Indent(results)) =>
              Left(LoopState(Some(results), tail, count, locks, refs, deferreds))

            case Left(Dedent(results)) =>
              Left(LoopState(Some(results), tail, count, locks, refs, deferreds))

            case Left(mkref: MkRef[a, b]) =>
              val a = mkref.a
              val ref = new Ref[a](mkref.id)
              Left(LoopState(Some(() => mkref.body(ref)), tail, count, locks, refs + (mkref.id -> a), deferreds))

            case Left(modifyRef: ModifyRef[a, b, c]) =>
              val a = refs(modifyRef.ref.monitorId).asInstanceOf[a]
              val (newA, b) = modifyRef.f(a)
              Left(LoopState(Some(() => modifyRef.body(b)), tail, count, locks, refs.updated(modifyRef.ref.monitorId, newA), deferreds))

            case Left(mkDeferred: MkDeferred[a, b]) =>
              val deferred = new Deferred[a](mkDeferred.id)
              Left(LoopState(Some(() => mkDeferred.body(deferred)), tail, count, locks, refs, deferreds))

            case Left(tryGetDeferred: TryGetDeferred[a, b]) =>
              val optA = deferreds.get(tryGetDeferred.deferred.monitorId).map(_.asInstanceOf[a])
              Left(LoopState(Some(() => tryGetDeferred.body(optA)), tail, count, locks, refs, deferreds))

            case Left(completeDeferred: CompleteDeferred[a, b]) =>
              val newA = deferreds.get(completeDeferred.deferred.monitorId).map(_.asInstanceOf[a]).getOrElse(completeDeferred.a)
              Left(LoopState(Some(() => completeDeferred.body()), tail, count, locks, refs, deferreds.updated(completeDeferred.deferred.monitorId, newA)))
          }

        // if we have outstanding awaits but no active fibers, then we're deadlocked
        case None =>
          locks.forall(_._2.isEmpty).asRight[LoopState].pure[M]
      }
    }
  }

  private[this] def constK[M[_]]: M ~> Const[Option[String], *] =
    new (M ~> Const[Option[String], *]) {
      def apply[a](ma: M[a]) = Const(None)
    }

  def prettyPrint[M[_]: Monad, A: Show](
      target: ThreadT[M, A],
      render: M ~> Const[Option[String], *] = constK[M],
      limit: Int = 512)   // sanity limit on the number of bytes allowed in the output
      : M[String] = {
    val TurnRight = "╰"
    val TurnLeft = "╯"

    val InverseTurnRight = "╭"
    val InverseTurnLeft = "╮"

    val IndentStr = TurnRight + InverseTurnLeft
    val DedentStr = InverseTurnRight + TurnLeft
    val IndentSpacing = IndentStr.length - 1

    val Junction = "├"
    val Line = "│"

    val ForkStr = Line + " " + TurnRight + InverseTurnLeft
    val ForkSpacing = ForkStr.length - 2

    def drawSpaces(num: Int): String =
      (0 until num).map(_ => ' ').mkString

    def drawIndent(level0: List[Boolean], term: String): String = {
      def loop(level: List[Boolean]): String =
        level match {
          case true :: tail =>
            Line + drawSpaces(ForkSpacing) + loop(tail)

          case false :: tail =>
            drawSpaces(IndentSpacing) + loop(tail)

          case Nil =>
            term
        }

      loop(level0.reverse)
    }

    def drawId(id: MonitorId): String = "0x" + id.hashCode.toHexString.toUpperCase

    def drawRef(ref: Ref[_], a: Any): String = "Ref(id = " + drawId(ref.monitorId) + ") =" + a.toString

    def drawDeferred(deferred: Deferred[_]): String = "Deferred(id = " + drawId(deferred.monitorId) + ")"

    case class LoopState(
        target: ThreadT[M, A],
        acc: String,
        indent: List[Boolean],
        init: Boolean = false,
        monitorCount: Int,
        refs: Map[MonitorId, _],
        deferreds: Map[MonitorId, _]
    )

    def loop(target: ThreadT[M, A], acc: String, indent: List[Boolean], init: Boolean, monitorCount: Int, refs: Map[MonitorId, _], deferreds: Map[MonitorId, _]): M[String] = {
      Monad[M].tailRecM(LoopState(target, acc, indent, init, monitorCount, refs, deferreds)) { ls =>
        val LoopState(target, acc0, indent, init, count, refs, deferreds) = ls

        val junc0 = if (init) InverseTurnRight else Junction
        val trailing = if (indent != Nil) "\n" + drawIndent(indent, "") else ""

        val resumed = target.resume

        val (junc, front) = render(resumed) match {
          case Const(Some(str)) => (Junction, drawIndent(indent, junc0 + " " + str) + "\n")
          case Const(None) => (junc0, "")
        }

        val acc = acc0 + front

        if (acc.length >= limit) {
          (acc + "\n...").asRight[LoopState].pure[M]
        } else {
          // stack is still proportional to the number of forked fibers, but not the number of binds
          resumed flatMap {
            case Left(Fork(left, right)) =>
              val leading = drawIndent(indent, junc + " Fork") + "\n" + drawIndent(indent, ForkStr)

              loop(right(), "", true :: indent, false, count, refs, deferreds) map { rightStr =>
                val acc2 = acc + leading + "\n" + rightStr + "\n"
                LoopState(left(), acc2, indent, false, count, refs, deferreds).asLeft[String]
              }

            case Left(Cede(results)) =>
              val acc2 = acc + drawIndent(indent, junc + " Cede") + "\n"
              LoopState(results(), acc2, indent, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(Done) =>
              (acc + drawIndent(indent, TurnRight + " Done" + trailing)).asRight[LoopState].pure[M]

            case Left(Monitor(f)) =>
              val id = new MonitorId(count)
              LoopState(f(id), acc, indent, init, count + 1, refs, deferreds).asLeft[String].pure[M]   // don't render the creation

            case Left(Await(id, results)) =>
              val acc2 = acc + drawIndent(indent, junc + " Await ") + drawId(id) + "\n"
              LoopState(results(), acc2, indent, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(Notify(id, results)) =>
              val acc2 = acc + drawIndent(indent, junc + " Notify ") + drawId(id) + "\n"
              LoopState(results(), acc2, indent, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(Annotate(name, results)) =>
              val acc2 = acc + drawIndent(indent, junc + s" $name") + "\n"
              LoopState(results(), acc2, indent, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(Indent(results)) =>
              val acc2 = acc + drawIndent(indent, IndentStr) + "\n"
              LoopState(results(), acc2, false :: indent, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(Dedent(results)) =>
              val indent2 = indent.tail
              val acc2 = acc + drawIndent(indent2, DedentStr) + "\n"
              LoopState(results(), acc2, indent2, false, count, refs, deferreds).asLeft[String].pure[M]

            case Left(mkRef: MkRef[a, b]) =>
              val ref = new Ref[a](mkRef.id)
              val acc2 = acc + drawIndent(indent, junc + " Create ref ") + drawRef(ref, mkRef.a) + "\n"
              LoopState(mkRef.body(ref), acc2, indent, init, count, refs + (ref.monitorId -> mkRef.a), deferreds).asLeft[String].pure[M]

            case Left(modifyRef: ModifyRef[a, b, c]) =>
              val a = refs(modifyRef.ref.monitorId).asInstanceOf[a]
              val (newA, b) = modifyRef.f(a)
              val acc2 = acc + drawIndent(indent, junc + " Modify ref ") + drawRef(modifyRef.ref, newA) + ", produced " + b.toString + "\n"
              LoopState(modifyRef.body(b), acc2, indent, init, count, refs.updated(modifyRef.ref.monitorId, newA), deferreds).asLeft[String].pure[M]

            case Left(mkDeferred: MkDeferred[a, b]) =>
              val deferred = new Deferred[a](mkDeferred.id)
              val acc2 = acc + drawIndent(indent, junc + " Create deferred ") + drawDeferred(deferred) + "\n"
              LoopState(mkDeferred.body(deferred), acc2, indent, init, count, refs, deferreds).asLeft[String].pure[M]

            case Left(tryGetDeferred: TryGetDeferred[a, b]) =>
              val optA = deferreds.get(tryGetDeferred.deferred.monitorId).map(_.asInstanceOf[a])
              val acc2 = acc + drawIndent(indent, junc + " Try get deferred ") + drawDeferred(tryGetDeferred.deferred) + " = " + optA.toString + "\n"
              LoopState(tryGetDeferred.body(optA), acc2, indent, init, count, refs, deferreds).asLeft[String].pure[M]

            case Left(completeDeferred: CompleteDeferred[a, b]) =>
              val newA = deferreds.get(completeDeferred.deferred.monitorId).map(_.asInstanceOf[a]).getOrElse(completeDeferred.a)
              val acc2 = acc + drawIndent(indent, junc + " Complete deferred ") + drawDeferred(completeDeferred.deferred) + " with value " + newA.toString + "\n"
              LoopState(completeDeferred.body(), acc2, indent, init, count, refs, deferreds.updated(completeDeferred.deferred.monitorId, newA)).asLeft[String].pure[M]

            case Right(a) =>
              (acc + drawIndent(indent, TurnRight + " Pure " + a.show + trailing)).asRight[LoopState].pure[M]
          }
        }
      }
    }

    loop(target, "", Nil, true, 0, Map.empty, Map.empty)
  }
}
