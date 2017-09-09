package chapter3

/**
  * Created by 周平 on 2017/01/14.
  */
sealed trait Tree[+A] {
  // 3.25 Ans
  def sizeAns(): Int = this match {
    case Leaf(z) => 1
    case Branch(x, y) => {
      println(x, y)
      1 + x.sizeAns + y.sizeAns
    }
  }

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25 Branchも数える必要があった。。ww
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(x, y) => 1 + size(x) + size(y)
  }
  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(z) => z
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(z) => 0
    case Branch(x, y) => 1 + (depth(x)).max(depth(y))
  }

  // 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(z) => Leaf(f(z))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  // 3.29 OK(3時間くらいかかった。)
  def fold[A, B](t: Tree[A], z: (A => B))(f: (B, B) => B): B = t match {
    case  Leaf(a) => z(a)
    case  Branch(x, y) => f(fold(x, (z))(f), fold(y, (z))(f))
  }

  // 3.29Ans
  def foldAns[A, B](t: Tree[A])(g: (A => B))(f: (B, B) => B): B = t match {
    case  Leaf(a) => g(a)
    case  Branch(x, y) => f(foldAns(x)(g)(f), foldAns(y)(g)(f))
  }


  def sizeImpl[A](t: Tree[A]): Int =
    fold[A, Int](t, (_ => 1))((a, b) => 1 + a + b)

  def depthImpl[A](t: Tree[A]): Int =
    fold[A, Int](t, (_ => 0))((a, b) => 1 + a.max(b))

  def maximumImpl(t: Tree[Int]): Int =
    fold[Int, Int](t, (z => z))((a, b) => a.max(b))

  def mapImpl[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t, (z => Leaf(f(z))))((a, b) => Branch(a, b))

  def mapImplAns[A, B](t: Tree[A])(f: A => B): Tree[B] =
    foldAns(t)(z => Leaf(f(z)): Tree[B])(Branch(_, _))


  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val treeInt = Branch(Branch(Leaf(100), Leaf(20)), Branch(Leaf(102), Leaf(1010)))
    val tree2 = Branch(Branch(Leaf("a"), Branch(Branch(Leaf("a"), Leaf("B")),Leaf("b"))), Branch(Leaf("c"), Leaf("d")))
    /*println(size(tree))
    println(maximum(treeInt))*/
    //println(depth(tree2))
    //println(sizeImpl(tree))
    //println(depthImpl(tree2))
    println(maximumImpl(treeInt))
    println(mapImpl(treeInt)(_ + 1))
  }

}