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
  val bounded = mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
  bounded >> ThreadT.done
}

val thread2 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 1)) >> ThreadT.cede(())
  val bounded = mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
  bounded >> ThreadT.done
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
  val bounded = mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
  bounded >> ThreadT.done
}

val thread2 = {
  val mod = ThreadT.liftF(State.modify[Vector[Int]](_ :+ 1)) // >> ThreadT.cede(())
  val bounded = mod.untilM_(ThreadT.liftF(State.get[Vector[Int]]).map(_.length >= 10))
  bounded >> ThreadT.done
}

val main = ThreadT.start(thread1) >> ThreadT.start(thread2)

ThreadT.roundRobin(main).runS(Vector()).value   // => Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
```

The first thread runs until it has filled the entire vector with `0`s, after which it *finally* yields and the second thread only has a chance to insert a single value (which immediately overflows the length).
