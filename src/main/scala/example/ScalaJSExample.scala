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
case class Lesson(name: String, words: Seq[Word], requireReturn: Boolean, checked: Var[Boolean]=Var(false)) {
  def reset(): Unit = {
    words.foreach(_.done() = false)
  }

  def number(word: Word): Int = {
    words.indexOf(word)
  }
}

object Lesson {
  val alphabet_numbers = Lesson("one two three's", Seq(
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
  ), true, Var(true))

  val alphabets = Lesson("ABC's", ('A' to 'Z').map{c: Char => Word(c.toString())}, false)
  
  val numbers = Lesson("123's", (1 to 100).map{c: Int => Word(c.toString())}, false)

  val hiragana = Lesson("ひらがな", Seq(
    Word("あいうえお"),
    Word("かきくけこ"),
    Word("さしすせそ"),
    Word("たちつてと"),
    Word("なにぬねの"),
    Word("はひふへほ"),
    Word("まみむめも"),
    Word("やゆよ"),
    Word("らりるれろ"),
    Word("わをん")
  ), true)

  val lessons = Seq(alphabets, alphabet_numbers, numbers, hiragana)
}

case class Lang(name: String, code: String)

object Lang{
  val en = Lang("English",  "en-US")
  val ja = Lang("Japanese", "ja-JP")
  val cn = Lang("Chinese",  "zh-CN")

  val langs = Seq(en, ja, cn)
}

object Speaker {
  def speak(word: Word)(implicit lang: Var[String]): Unit = {
    js.eval("var msg = new SpeechSynthesisUtterance('" + word.txt + "'); msg.lang='" + lang() + "'; window.speechSynthesis.speak(msg);")
  }
}

@JSExport
object ScalaJSExample {
  import Framework._

  val lessons = Lesson.lessons

  val selectedLessons = Rx{lessons.filter(_.checked())}

  val currentLessonPosition = Var(0)

  val currentPosition = Var(0)

  val currentLesson = Rx{
    if (currentLessonPosition() >= selectedLessons().length) {
      currentLessonPosition() = 0
      currentPosition() = 0
    }
    selectedLessons()(currentLessonPosition())
  }

  val word: Rx[Word] = Rx{
    if (currentPosition() >= currentLesson().words.length) {
      currentLessonPosition() += 1
      currentLesson().reset()
      currentPosition() = 0
    }
    currentLesson().words(currentPosition())
  }

  implicit val lang = Var(Lang.en.code)

  val currentInput = Var("")

  val isCorrectInput = Rx{currentInput() == word().txt}

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
          select(
            for (lang <- Lang.langs) yield {
              option(lang.name, value := lang.code)
            },
            onchange := {(e:Event) =>
              lang() = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
            }
          ),

          for (lesson <- lessons) yield {
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
