package it.unibo.pps.util

object Streams:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def apply[A](as: A*): Stream[A] =
      if as.isEmpty then empty()
      else cons(as.head, apply(as.tail*))

    def iterate[A](start: A)(f: A => A): Stream[A] =
      cons(start, iterate(f(start))(f))

    def rangeClosed(start: Int, end: Int, step: Int = 1): Stream[Int] =
      Stream.iterate(start)(_ + step).takeWhile(i => i <= end)

    extension [A](stream: Stream[A])

      def toList: Sequence[A] = stream match
        case Stream.Cons(h, t) => Sequence.Cons(h(), t().toList)
        case _ => Sequence.Nil()

      def concat(stream2: => Stream[A]): Stream[A] = stream match
        case Stream.Cons(h, t) => Stream.cons(h(), t().concat(stream2))
        case _ => stream2

      def flatMap[B](f: A => Stream[B]): Stream[B] = stream match
        case Stream.Cons(h, t) => f(h()).concat(t().flatMap(f))
        case _ => Stream.empty()
        
      def map[B](f: A => B): Streams.Stream[B] = stream.flatMap(x => Stream.cons(f(x), Stream.empty()))

      def filter(pred: A => Boolean): Streams.Stream[A] = stream.flatMap:
        case x if pred(x) => Stream.cons(x, Stream.empty())
        case _ => Stream.empty()

      def take(n: Int): Streams.Stream[A] = (stream, n) match
        case (Stream.Cons(head, tail), n) if n > 0 => Stream.cons(head(), tail().take(n - 1))
        case _ => Stream.Empty()

      def takeWhile(pred: A => Boolean): Stream[A] = 
        def _takeWhile(s: Stream[A])(pred: A => Boolean): Stream[A] = s match
          case Empty() => Empty()
          case Cons(h, t) if pred(h()) => cons(h(), _takeWhile(t())(pred))
          case _ => Empty()
        _takeWhile(stream)(pred)
    
  end Stream

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = str1.map(_ + 1) // {1,2,3,4,..}
  val str3 = str2.filter(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = str3.take(10) // {1,2,21,22,..,28}
  println(str4.toList) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(corec.take(10).toList) // [1,1,..,1]