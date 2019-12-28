# coop

Based on http://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html?m=1 All credit to Gabriel Gonzales. The minor API and implementation differences from his Haskell version are due to Scala being Scala, and not any sign of original thought on my part.

## Usage

```sbt
libraryDependencies += "com.codecommit" %% "coop" % "<version>"
```

Published for Scala 2.13.1.

```scala
import coop.ThreadT

// with cats-effect
import cats.effect.IO
import cats.implicits._

val thread1 = (ThreadT.liftF(IO(println("yo"))) >> ThreadT.cede(())).foreverM
val thread2 = (ThreadT.liftF(IO(println("dawg"))) >> ThreadT.cede(())).foreverM

val main = ThreadT.start(thread1) >> ThreadT.start(thread2)

val ioa = ThreadT.roundRobin(main)     // => IO[Unit]
ioa.unsafeRunSync()
```

This will print the following endlessly:

```
yo
dawg
yo
dawg
yo
dawg
...
```

Or if you think that some witchcraft is happening due to the use of `IO` in the above, here's an example using `State` instead:

```scala
import coop.ThreadT

import cats.data.State
import cats.implicits._

import scala.collection.immutable.Vector

val thread1 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 0)) >> ThreadT.cede(())
  mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
}

val thread2 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 1)) >> ThreadT.cede(())
  mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
}

val main = ThreadT.start(thread1) >> ThreadT.start(thread2)

ThreadT.roundRobin(main).runS(Vector()).value   // => Vector(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
```

Of course, it's quite easy to see what happens if we're *not* cooperative and a single thread hogs all of the resources:

```scala
import coop.ThreadT

import cats.data.State
import cats.implicits._

import scala.collection.immutable.Vector

val thread1 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 0)) // >> ThreadT.cede(())
  mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
}

val thread2 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 1)) // >> ThreadT.cede(())
  mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
}

val main = ThreadT.start(thread1) >> ThreadT.start(thread2)

ThreadT.roundRobin(main).runS(Vector()).value   // => Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
```

The first thread runs until it has filled the entire vector with `0`s, after which it *finally* yields and the second thread only has a chance to insert a single value (which immediately overflows the length).

## MTL Style

A `ApplicativeThread` typeclass, in the style of [Cats MTL](https://github.com/typelevel/cats-mtl), is also provided to make it easier to use `ThreadT` as part of more complex stacks:

```scala
def thread1[F[_]: Monad: ApplicativeThread](implicit F: MonadState[F, Vector[Int]]): F[Unit] = {
  val mod = F.modify(_ :+ 0) >> ApplicativeThread[F].cede_
  mod.untilM(F.get).map(_.length >= 10)
}

def main[F[_]: Apply: ApplicativeThread](thread1: F[Unit], thread2: F[Unit]): F[Unit] =
  ApplicativeThread[F].start(thread1) *> ApplicativeThread[F].start(thread2)
```

`ApplicativeThread` will inductively derive over `Kleisli` and `EitherT`, and defines a base instance for `FreeT[S[_], F[_], ?]` given an `InjectK[ThreadF, S]` (so, strictly more general than just `ThreadT` itself). It notably will not auto-derive over `StateT` or `WriterT`, due to the value loss which occurs in `fork`/`start`. If you need `StateT`-like functionality, it is recommended you either include some sort of `StateF` in your `FreeT` suspension, or nest the `State` functionality *within* the `FreeT`.

You will probably want to `import FreeTInstances._` to ensure that the appropriate Cats MTL machinery for `FreeT` itself is made available in scope, otherwise instances will not propagate correctly across `FreeT` in the transformer stack. This machinery is probably going to get contributed upstream into a Cats MTL submodule.

## `MVar`

An experimental implementation of `MVar` is made available, mostly because it makes it theoretically possible to define all the things. The API mirrors Haskell's. The implementation assumes an `ApplicativeAsk` (from Cats MTL) of an `UnsafeRef` which contains the uniquely indexed state for every `MVar`. This could have just been done using a private `var` instead, but I wanted to be explicit about the state management.

`UnsafeRef` is used rather than `StateT` or `InjectK[StateF[S, ?], F]` to avoid issues with `MonadError` instances in the stack. Additionally, as mentioned earlier, `ApplicativeThread` will not auto-derive over `StateT` due to issues with value loss, so all in all it's a better approach if we aren't going to use a `var`.

All blocking operations are implemented as spin waits with interleaving `cede`s, ensuring that execution is not blocked. A more efficient implementation is possible by introducing the concept of a monitor in `ThreadF`, but efficiency is the *last* thing you should be worried about if you're using this library.
