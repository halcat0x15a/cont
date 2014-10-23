import scala.language.experimental.macros

package object cont {

  case class Context[+A, -B, +C](cc: (A => B) => C) extends AnyVal

  def shift[A, B, C](cc: (A => B) => C) = Context(cc)

  implicit class CC[A](val a: A) extends AnyVal {
    def cc[B](f: A => B) = f(a)
  }

  def reset(expr: Any): Any = macro resetImpl

  def resetImpl(c: scala.reflect.macros.whitebox.Context)(expr: c.universe.Tree): c.universe.Tree = {
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
