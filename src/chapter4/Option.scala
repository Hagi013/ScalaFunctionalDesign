package chapter4

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

  // 4-2
  //def variance(xs: Seq[Double]): Option[Double]
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
