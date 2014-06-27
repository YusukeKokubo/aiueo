package example

import org.scalajs.dom
import scalatags.JsDom._
import all._
import tags2.section
import rx._
import scala.scalajs.js.annotation.JSExport
import scala.Some
import scala.util.Random
import scala.scalajs.js
import org.scalajs.dom.{HTMLElement, Event}


case class Word(txt: String, done: Var[Boolean]=Var(false))

@JSExport
object ScalaJSExample {
  import Framework._

  val inputBox = input(
    id:="new-todo",
    autofocus:=true,
    autocomplete:=false
  ).render

  val numbers = Seq(
    Word("one"),
    Word("two"),
    Word("three"),
    Word("four"),
    Word("five"),
    Word("six"),
    Word("seven"),
    Word("eight"),
    Word("nine"),
    Word("ten")
  )

  val currentPosition = Var(0)

  val word = Rx{numbers(currentPosition())}

  val currentInput = Var("")

  val hint = Rx{if (currentInput() == word().txt) "  - Enter!" else ""}
  
  @JSExport
  def main(): Unit = {

    dom.document.body.innerHTML = ""
    dom.document.body.appendChild(
      section(id:="todoapp") (
        header(id:="header")(
          h1(Rx{word().txt + hint()}),
          form(
            inputBox,
            onsubmit := { () =>
              if (currentInput() == word().txt) {
                word().done() = true
                currentPosition() += 1
                inputBox.value = ""
                currentInput() = ""
                inputBox.placeholder = word().txt
              }
              false
            }
          )
        ),

        section(id:="main")(
          Rx {
            ul(id := "todo-list")(
              for (word <- numbers) yield {
                li(`class`:= Rx{
                    if (word.done())"completed"
                    else ""
                  },
                  div(`class`:="view")(
                    input(
                      `class` := "toggle",
                      `type` := "checkbox",
                      cursor := "pointer",
                      if (word.done()) checked := true
                    ),
                    label(word.txt)
                  ))
              }
            )
          },

          footer(id:="footer")(
            span(id:="todo-count")(strong(Rx{currentPosition()}), "'s inputed")
          )
        ),

        footer(id:="info")(
          p("Created by ", a(href:="http://github.com/YusukeKokubo")("Yusuke Kokubo."))
        )
      ).render
    )

    dom.document.getElementById("new-todo").oninput = { (e:Event) =>
      currentInput() = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
    }
  }

}
