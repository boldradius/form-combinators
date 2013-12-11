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
import Markup.{static, addMarkup}

// Children of head are ignored, newChildren chery picks all new children from their previous index
case class HtmlDiff(head: Option[Html], newChildren: Seq[ChildDefs])
sealed trait ChildDefs
case class PickChild(oldIndex: Int, diff: Option[HtmlDiff]) extends ChildDefs
case class NewChild(html: Html) extends ChildDefs

object HtmlDiff {
  def applyDiff(diff: HtmlDiff, in: js.Dynamic) : js.Dynamic = {
    var newHead = diff.head.fold(in)(Html.render(_))
    var children = js.Array.asInstanceOf[js.Dynamic].prototype.slice.call(newHead.childNodes, 0).asInstanceOf[js.Array[js.Dynamic]]

    diff.newChildren.zipWithIndex.foreach{case (c, newIndex) =>
      val newChild = c match {
        case PickChild(i, d) => {var n = children(i); d.fold(n)(applyDiff(_, n))}
        case NewChild(h) => Html.render(h)
      }
      if (newChild.parentNode != newHead && newChild.parentNode != null) newChild.parentNode.removeChild(newChild)
      if (newChild.parentNode == null) {
        while (newHead.childNodes.length.asInstanceOf[js.Number] > newIndex) newHead.removeChild(newHead.lastChild)
        newHead.appendChild(newChild)
      }
      else while (newHead.childNodes.asInstanceOf[js.Array[js.Dynamic]](newIndex) != newChild) newHead.removeChild(newHead.childNodes.asInstanceOf[js.Array[js.Dynamic]](newIndex))
    }
    newHead
  }
  def atChild(childDiff: HtmlDiff, childIndex: Int, numChildren: Int) : HtmlDiff =
    HtmlDiff(None, (0 to (numChildren - 1)).toList.map(i =>
      PickChild(i, if (i == childIndex) Some(childDiff) else None)))
}
