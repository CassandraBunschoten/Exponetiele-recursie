sealed trait Pure[A]: 

  def compute: A = 
    this match
      case Done(a)    => a 
      case Call(p)    => p().compute
      case Cont(p, f) => 
        p match
          case Done(a)      => f(a).compute
          case Call(p1)     => p1().flatMap(f).compute
          case Cont(p1, f1) => p1.flatMap(x => f1(x).flatMap(f)).compute


  def flatMap[B](f: A => Pure[B]): Pure[B] =
    this match
      case Done(a)      => f(a)
      case p: Call[A]   => Cont(p, f)
      case Cont(p, f1)  => Cont(p, p => f1(p).flatMap(f))

  def map[B](f: A => B): Pure[B] = 
    flatMap(a => Done(f(a)))
    

case class Done[A](done: A) extends Pure[A]
case class Call[A](func: () => Pure[A]) extends Pure[A]
case class Cont[A,B](pure: Pure[A], func: A => Pure[B]) extends Pure[B]


def done[A](a: => A): Pure[A] =
  Done(a)

def call[A](p: => Pure[A]): Pure[A] =
  Call(() => p)

object Main extends App:

  def a1(m: Int, n: Int): Pure[Int] =
    (m, n) match
      case (0, _) => done(n + 1)
      case (_, 0) => call(a1(m - 1, 1))
      case (_, _) => 
        for{
          x <- call(a1(m, n-1))
          y <- call(a1(m-1, x))
        } yield y

  println(a1(4,1).compute)