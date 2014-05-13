package combinators

/*
Copyright [2013] [Tindr Solutions]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

import scala.scalajs.js
//import org.scalajs.dom._
import js.Dynamic.{ global => g }
import Html._
import Markup.{static, addMarkup}
import js.annotation.JSExport

sealed trait Form[A] {
  def +[B](b: Form[B]) = Pair(this, b)
  def listener(onChange: A => Unit) = Listener(onChange, this)
  def map[B](initialize: B => A, validate: A => B) = Map(initialize, validate, this)
  def note(html: A => Html) = Note(this, html)
}
case class Labeled[A](label: String, element: Element[A]) extends Form[A]
case class Pair[A, B](a: Form[A], b: Form[B]) extends Form[(A,B)]
case class Listener[A](onChange: A => Unit, a: Form[A]) extends Form[A]
case class Map[A, B](initialize: B => A, validate: A => B, a: Form[A]) extends Form[B]
case class Note[A](a: Form[A], html: A => Html) extends Form[A]

sealed trait Element[A]
case class TextField(numChars: Int) extends Element[String]
case class PasswordField(numChars: Int) extends Element[String]


object Form {
  def render[A](f: Form[A], initial: A, onChange: A => Unit) : Markup = f match {
    case Labeled(l, e) => static(row(text(l), e match {
      case TextField(n) => textInput(n, initial, onChange)
      case PasswordField(n) => passwordInput(n, initial, onChange)
    }))
    case Pair(a, b) => renderPair(a, b, initial, onChange)
    case Listener(l, a) => render(a, initial, (n : A) => {l(n); onChange(n)})
    case Map(i, v, a) => renderMap(i, v, a, initial, onChange)
    case Note(a, html) => renderNote(a, html, initial, onChange)
  }
  def row(e: Html*) = tr()(e.map(td()(_)) : _*)
  def renderMap[A, B](i: B => A, v: A => B, a: Form[A], initial: B, onChange: B => Unit) =
    render(a, i(initial), onChange compose v)
  def renderPair[A, B](a: Form[A], b: Form[B], initial: (A, B), onChange: ((A, B)) => Unit) = {
    var v = initial;
    Markup.div()(
      render(a, initial._1, (n : A) => {v = (n, v._2); onChange(v)}),
      render(b, initial._2, (n : B) => {v = (v._1, n); onChange(v)}))
  }
  def renderNote[A](a: Form[A], html: A => Html, initial: A, onChange: A => Unit) = {
    def note(a: A) = row(text(""), html(a))
    Markup.withOnDiff(onDiff => Markup.div()(
      render(a, initial, (n:A) => {
          onDiff(HtmlDiff.atChild(HtmlDiff(Some(note(n)), Nil), 1, 2))
          onChange(n)
        }),
      static(note(initial))))
  }

  implicit class Same[A](f: Form[(A, A)]) {
    def same(default: A) = Map[(A, A), Option[A]](
      oa => {val a = oa.getOrElse(default); (a, a)},
      t => if (t._1 == t._2) Some(t._1) else None, f)
  }

  implicit class Render[A](f: Form[A]) {
    def render(initial: A) : Markup = Form.render(f, initial, (_:A) => {})
  }
}

case class User(name: String, password:String, email: String)

@JSExport
object Combinators {
  val form : Form[Option[User]] =
    ((Labeled("Username", TextField(20)) +
     Labeled("Password", PasswordField(20)).note(passwordStrength _)) +
    (Labeled("E-Mail", TextField(30)) +
     Labeled("Retype E-Mail", TextField(30)))
      .same("").note(m => text(if (m.isEmpty) "E-Mails do not match" else "")))
     .map(
        user => {
          val u = user.getOrElse(User("","",""))
          ((u.name, u.password), Some(u.email))},
        {case ((name, pw), email) => email.map(User(name, pw,_))})

  def passwordStrength(p: String) = text(
    List((p.length < 8) -> "Too short",
      (p.toSet.size < 5) -> "Weak",
      (p.toSet.size < 7) -> "Fair",
      (p.toSet.size < 9) -> "Good")
      .find(_._1).fold("Strong")(_._2))

  val page : Markup = Markup.p()(
    static(p()(text("A simple form:"))),
    form.listener(v => g.console.log(v.toString))
      .note(v => div()(br(), text("Current form value: " + v.toString)))
      .render(None)
  )
  @JSExport
  def main() : Unit = {
    val p = addMarkup(doc.getElementById("playground"), page)
  }
}
