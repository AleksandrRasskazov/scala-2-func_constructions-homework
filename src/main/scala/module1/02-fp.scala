package module1

import akka.io.dns.RecordType.A
import module1.list.List.Cons

import scala.annotation.tailrec

/**
 *  Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+A] {

    import Option.{None, Some}

    /**
     *
     * Реализовать метод isEmpty, который будет возвращать true если Option не пуст и false в противном случае
     */

    def isEmpty: Boolean = this match {
      case Some(_) => false
      case None => true
    }

    /**
     *
     * Реализовать метод get, который будет возвращать значение
     */

    def get[T]: T = this match {
      case Some(v: T) => v
      case None => throw new Exception("get on empty Option")
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny(): Unit = this match {
      case Some(v: A) => println(v)
      case _ => ()
    }

    /**
     *
     * реализовать метод orElse который будет возвращать другой Option, если данный пустой
     */

//    def orElse2[T](x: T) = this match {
//      case Some(v: T) => v
//      case None => x
//    }

      def orElse[T](x: Option[T]): Option[T] =
        if (isEmpty) x: Option[T] else this.get


    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

//    def zip[T](x: Option[T]): Option[(T, T)] = {
//      case None => None
//      case Some(_) => Some(this.get, x.get)
//    }

    def zip2[T](x: Option[T]): Option[(T, T)] = if (this.isEmpty || x.isEmpty) None
      else Some(this.get, x.get)

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f: A => Boolean ): Option[A] = if (!isEmpty || f(this.get)) this.get else throw new Exception("get on empty Option")


  }


  object Option {
    case class Some[A](v: A) extends Option[A]
    case object None extends Option[Nothing]
  }


}

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def !!(n: Int): Long = {
    if(n <= 1) 1
    else n * !!(n - 1)
  }

  def !(n: Int): Long = {
    @tailrec
    def loop(n1: Int, acc: Long): Long = {
      if(n <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }
    loop(n, 1)
  }

}

object list {
  /**
   *
   * Реализовать односвязанный имутабельный список List
   */

  sealed trait List[+A] {
    import List._

    def isEmpty: Boolean = this eq Nil


    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[A] = {
      @tailrec
      def reverse_loop(input: List[A], acc: List[A] = List[A]()): List[A] = {
        input match {
          case List.Nil => acc
          case List.::(head, tail) => reverse_loop(tail, head :: acc)
        }
      }
      reverse_loop(this)
    }

    /**
     *
     * Реализовать метод конс :: который позволит добавлять элемент в голову списка
     */

    def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

    /**
     *
     * Реализовать метод mkString который позволит красиво представить список в виде строки
     */

    def mkString: String = mkString(", ")

    def mkString(sep: String): String = {
      @tailrec
      def loop(l: List[A], acc: StringBuilder): StringBuilder = l match {
        case List.Nil => acc
        case h :: Nil => acc.append(s"$h")
        case h :: t => loop(t, acc.append(s"$h$sep"))
      }
      loop(this, new StringBuilder()).toString()
    }

    /**
     *
     * Реализовать метод для списка который будет применять некую ф-цию к элементам данного списка
     *
     */

    def map[AA >: A](f: AA => AA): List[AA] = {
      def map_loop(l: List[AA], res: List[AA] = List[A]()): List[AA] = {
        l match {
          case Nil => res
          case head :: tail => map_loop(tail, f(head):: res)
        }
      }
      map_loop(this)
    }

  }

  object List{
    case object Nil extends List[Nothing]
    case class ::[A](head: A, tail: List[A]) extends List[A]
    val Cons = ::

    def apply[T](arg: T*): List[T] = {
      var l: List[T] = List.Nil
      arg.foreach(el => l = el :: l)
      l
    }



    /**
     *
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
    def incList(xs: List[Int], inc: Int=1): List[Int] = {
      @tailrec
      def incList_loop(xs: List[Int], acc: List[Int] = List[Int]()): List[Int] = {
        xs match {
          case List.Nil => acc
          case List.::(head, tail) => incList_loop(tail, head + inc :: acc)
        }
      }
      incList_loop(xs)
    }


  }






  /**
   *
   * Реализовать конструктор, для создания списка n элементов
   */






  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

  def shoutString(l:List[String], someChar: Char = '!'): List[String] = {
    @tailrec
    def addChar_loop(l: List[String], res: List[String] = List[String]()): List[String] = {
      l match {
        case List.Nil => res
        case List.::(head, tail) => addChar_loop(tail,  someChar + head :: res )
      }
    }
    addChar_loop(l)
  }





}

