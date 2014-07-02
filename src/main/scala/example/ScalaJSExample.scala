package example

import org.scalajs.dom
import scalatags.JsDom._
import all._
import tags2.section
import rx._
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js
import org.scalajs.dom.{KeyboardEvent, Event}


case class Word(txt: String, done: Var[Boolean]=Var(false))
case class Lesson(name: String, words: Seq[Word], requireReturn: Boolean, checked: Var[Boolean]=Var(true)) {
  def reset(): Unit = {
    words.foreach(_.done() = false)
  }

  def number(word: Word): Int = {
    words.indexOf(word)
  }
}

object Lesson {
  val numbers = Lesson("Numbers", Seq(
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
  ), true)

  val alphabets = Lesson("ABC's", ('A' to 'Z').map{c: Char => Word(c.toString())}, false)

  val lessons = Seq(alphabets, numbers)
}

object Speaker {
  def speak(word: Word): Unit = {
    js.eval("window.speechSynthesis.speak(new SpeechSynthesisUtterance('" + word.txt + "'));")
  }
}

@JSExport
object ScalaJSExample {
  import Framework._

  val lessons = Rx{Lesson.lessons.filter(_.checked())}

  val currentLessonPosition = Var(0)

  val currentPosition = Var(0)

  val currentLesson = Rx{
    if (currentLessonPosition() >= lessons().length) {
      currentLessonPosition() = 0
      currentPosition() = 0
    }
    lessons()(currentLessonPosition())
  }

  val word: Rx[Word] = Rx{
    if (currentPosition() >= currentLesson().words.length) {
      currentLessonPosition() += 1
      currentLesson().reset()
      currentPosition() = 0
    }
    currentLesson().words(currentPosition())
  }

  val currentInput = Var("")

  val isCorrectInput = Rx{currentInput() == word().txt}

  val hint = Rx{if (isCorrectInput()) "  - Enter!" else ""}

  val inputBox = input(
    id:="new-todo",
    autofocus:=true,
    autocomplete:=false,
    placeholder := Rx{word().txt},
    `class` := Rx{if (word().txt.contains(currentInput())) "ok" else "ng"}
  ).render

  @JSExport
  def main(): Unit = {

    Speaker.speak(word())
    dom.document.body.innerHTML = ""
    dom.document.body.appendChild(
      section(id:="aiueo") (
        header(id:="header")(
          h1(Rx{"typing lesson - " + currentLesson().name}),
          for (lesson <- lessons()) yield {
            div(
              label(lesson.name,
                input(`type`:="checkbox",
                  onchange:= {() => lesson.checked() = !lesson.checked() },
                  if (lesson.checked()) checked:=true
                )
              )
            )
          },
          form(inputBox, onsubmit := { () =>
            if (isCorrectInput() && currentLesson().requireReturn) {
              nextWord()
            }
            false
          })
        ),

        section(id:="words")(
          Rx {
            div(
              for (word <- currentLesson().words) yield {
                span(`class`:= Rx{
                    "word " + (if (word.done()) "completed"
                               else if (currentPosition() == currentLesson().number(word)) "current"
                               else "")
                  },
                  word.txt
                )
              }
            )
          }
        ),

        footer(id:="info")(
          p(a(href:="http://github.com/YusukeKokubo/aiueo")("Source Code")),
          p("Created by ", a(href:="http://github.com/YusukeKokubo")("Yusuke Kokubo."))
        )
      ).render
    )

    dom.document.getElementById(inputBox.id).oninput = { (e:Event) =>
      currentInput() = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
      if (isCorrectInput()) {
        if (!currentLesson().requireReturn) {
          nextWord()
        } else {
          Speaker.speak(word())
        }
      }
    }
  }

  def nextWord(): Unit = {
    word().done() = true
    currentPosition() += 1
    inputBox.value = ""
    currentInput() = ""
    Speaker.speak(word())
  }

}
