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
import js.Dynamic.{ global => g }
import Html._

case class Markup(markup: (HtmlDiff => Unit) => Html)
object Markup {
  def static(html: Html) : Markup = Markup(_ => html)
  private def lift(element: Seq[Html] => Html) : Seq[Markup] => Markup = children => Markup(onDiff =>
    element(children.zipWithIndex.map{case (Markup(ch), i) => ch(onDiff.compose(HtmlDiff.atChild(_, i, children.size)))}))

  def div()(children : Markup*) = lift(Html.div())(children)
  def p()(children : Markup*) = lift(Html.p())(children)
  def br() = static(Html.br())

  def withOnDiff(f: (HtmlDiff => Unit) => Markup) : Markup =
    Markup(onDiff => f(onDiff).markup(onDiff))

  def addMarkup(parentElement: js.Dynamic, m: Markup) : Unit =
    parentElement.appendChild(Html.render(m.markup(diff =>
    {HtmlDiff.applyDiff(diff, parentElement.lastChild); g.console.log(diff.toString)}))) // TODO handle return value of applyDiff
}