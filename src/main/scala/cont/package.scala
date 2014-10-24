import scala.language.higherKinds
import scala.language.experimental.macros

package object cont {

  case class Context[+A, -B, +C](cc: (A => B) => C) extends AnyVal

  def shift[A, B, C](cc: (A => B) => C) = Context(cc)

  implicit class CC[A](val a: A) extends AnyVal {
    def cc[B](f: A => B) = f(a)
  }

  trait Ans[A] { type Type }

  trait AnsImplicits {
    implicit def ans[A] = new Ans[A] { type Type = A }
  }

  object Ans extends AnsImplicits {
    implicit def context[A, B, C] = new Ans[Context[A, B, C]] { type Type = A }
    implicit def fa[F[_], A](implicit a: Ans[A]) = new Ans[F[A]] { type Type = F[a.Type] }
    implicit def fab[F[_, _], A, B](implicit a: Ans[A], b: Ans[B]) = new Ans[F[A, B]] { type Type = F[a.Type, b.Type] }
  }

  def reset[A](expr: A)(implicit ans: Ans[A]): ans.Type = macro resetImpl

  def resetImpl(c: scala.reflect.macros.blackbox.Context)(expr: c.Tree)(ans: c.Tree) = {
    import c.universe._

    def apply(expr: Tree, name: TermName)(cont: Tree => Tree) =
      q"$expr.cc(${q"val $name: ${tq""}"} => ${cont(Ident(name))})"

    def application(exprs: List[Tree])(cont: List[Tree] => Tree): Tree =
      exprs match {
	case Nil => cont(exprs)
	case e :: es => 
	  transform(e)(x => application(es)(xs => cont(x :: xs)))
      }

    def transform(expr: Tree, name: TermName = TermName(c.freshName))(cont: Tree => Tree): Tree =
      expr match {
	case q"$e; ..$es" =>
	  es match {
	    case Nil =>
	      e match {
		case q"$f[..$_](..$args)" => application(args)(as => apply(q"$f(..$as)", name)(cont))
		case q"val $x: $t = $a" => transform(a, x)(cont)
		case _ => apply(e, name)(cont)
	      }
	    case _ => transform(e)(_ => transform(q"{ ..$es }")(cont))
	  }
	case _ => transform(q"{ $expr }")(cont)
      }

    transform(c.untypecheck(expr))(x => x)
  }

}
