package chapter4

import chapter3.Cons

/**
  * Created by 周平 on 2017/01/21.
  */
trait Option[+A] {

  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  // 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  // 4.1 Ans
  def flatMapAns[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  // 4.1 ×
  //def orElse[B >: A](ob: => Option[B]): Option[B] = flatMapAns(B => Option[B]).getOrElse(ob)
  def orElseImpl[B >:A](ob: => Option[B]):Option[B] = map(Some(_)).getOrElse(ob)
  def orElseAns2[B >: A](ob: => Option[B]):Option[B] = this match {
    case None => ob
    case _ => this
  }
  // 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if(f(a)) this else None
    case _ => None
  }

  // ×
  def filter_1(f: A => Boolean): Option[A] = map(a => if(f(a)) Some(a) else None).getOrElse(None)

  def filter_2(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  // 4-2 ×
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMapAns(m => mean(xs.map(x => math.pow(x - m, 2))))
    // xs.flatMapAns((x: Double) => math.pow(x - (xs.sum) / xs.length, 2))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)
  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age + numberOfSpeedingTickets).toDouble

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  // 4-3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case Some(a) => b match {
      case Some(b) => Some(f(a, b))
      case None => None
    }
    case None => None
  }

  def map2Ans[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMapAns(aa => b map(bb => f(aa,bb)))

  // 4-4 ×
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMapAns(hh => sequence(t).map(hh :: _))
  }

  def parseInts(a: List[String]): Option[List[Int]] = sequence(a.map(i => Try(i.toInt)))

  // 4-5 × flatMapとその中の再起処理の関係理解ができていなかった。。
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMapAns(hh => traverse(t)(f).map(hh :: _))
  }

  def map2For[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for{
      aa <- a
      bb <- b
    } yield f(aa, bb)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def main(args: Array[String]): Unit = {

    val a: Option[Int] = Some(10);
    val b: Option[String] = a.map((_).toString() + 1);
    //println(a, b);
    val c: Option[Int] = None;
    val d: Int = a.getOrElse(3);
    val e:Int = c.getOrElse(3);
    //println(d, e);
    val f:Option[Int] = Some(5)
    val g: Option[Int] = f.filter(_ == 5)
    val h: Option[Int] = f.filter(_ == 2)
    println(g, h)
    val i:Option[Int] = Some(5)
    val j: Option[Int] = i.filter_1(_ == 5)
    val k: Option[Int] = i.filter_1(_ == 2)
    println(j, k)

    val l:Option[Int] = Some(5)
    val n: Option[Int] = i.filter_2(_ == 5)
    val m: Option[Int] = l.filter_2(_ == 2)
    println(n, m)
  }

}
