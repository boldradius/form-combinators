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
import org.scalajs.dom._
import js.Dynamic.{ global => g }
import Html._

sealed trait Html
case class text(t: String) extends Html
case class Div(children: Seq[Html]) extends Html
case class P(children: Seq[Html]) extends Html
case class Tr(children: Seq[Html]) extends Html
case class Td(children: Seq[Html]) extends Html
case class Br() extends Html
case class TextInput(password: Boolean, size: Int, value: String, onChange: String => Unit) extends Html

object Html {
  val doc = g.document
  val jQuery = js.Dynamic.global.jQuery
  private def elem(name: String, attrs: Seq[(String, js.Any)], children: Seq[js.Dynamic]) = {
    val element = doc.createElement(name)
    attrs.foreach{case (a, v) => element.setAttribute(a, v)}
    children.foreach(element.appendChild(_))
    element
  }
  def div()(children : Html*) = Div(children)
  def p()(children : Html*) = P(children)
  def tr()(children : Html*) = Tr(children)
  def td()(children : Html*) = Td(children)
  def br() = Br()
  def textInput(size: Int, value: String, onChange: String => Unit) = TextInput(false, size, value, onChange)
  def passwordInput(size: Int, value: String, onChange: String => Unit) = TextInput(true, size, value, onChange)

  def test() = g.console.log("hey")

  def render(h: Html) : js.Dynamic = h match {
    case text(s) => doc.createTextNode(s)
    case Div(c) => elem("div", Nil, c.map(render(_)))
    case P(c) => elem("p", Nil, c.map(render(_)))
    case Tr(c) => elem("tr", Nil, c.map(render(_)))
    case Td(c) => elem("td", Nil, c.map(render(_)))
    case Br() => elem("br", Nil, Nil)
    case TextInput(password, size, value, onChange) => {
      val e : js.Dynamic = elem("input", List(
        "type" -> (if (password) "password" else "text"),
        "value" -> value,
        "size" -> size.toString
      ), Nil)
      jQuery(e).on("input", {(event: js.Object) => onChange(e.value.asInstanceOf[String])})
      e
    }
  }
}

