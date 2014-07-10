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
case class Lesson(name: String, words: Seq[Word], requireReturn: Boolean, lang: Lang) {
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
  ), true, Lang.en)

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
  ), true, Lang.ja)

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

  val position = Var(0)

  val lesson = Var(Lesson.lessons.head)

  val word: Rx[Word] = Rx{
    if (position() >= lesson().words.length) {
      lesson().reset()
      position() = 0
    }
    lesson().words(position())
  }

  implicit val lang = Var(Lang.langs.head.code)

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
          h1(Rx{lesson().name}),
          Rx{
            select(
              for (l <- Lang.langs) yield {
                option(l.name, value := l.code, if (l.code == lang()) "selected".attr := true)
              },
              onchange := { (e: Event) =>
                lang() = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
              }
            )
          },

          for (l <- Lesson.lessons) yield {
            div(
              label(l.name,
                input(`type`:="radio",
                  onchange:= {() =>
                    lesson() = l
                    lang() = lesson().lang.code
                  },
                  name := "lesson",
                  if (l == lesson()) checked:=true
                )
              )
            )
          },

          form(inputBox, onsubmit := { () =>
            if (isCorrectInput() && lesson().requireReturn) {
              nextWord()
            }
            false
          })
        ),

        section(id:="words")(
          Rx {
            div(
              for (word <- lesson().words) yield {
                span(`class`:= Rx{
                    "word " + (if (word.done()) "completed"
                               else if (position() == lesson().number(word)) "current"
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
        if (!lesson().requireReturn) {
          nextWord()
        } else {
          Speaker.speak(word())
        }
      }
    }
  }

  def nextWord(): Unit = {
    word().done() = true
    position() += 1
    inputBox.value = ""
    currentInput() = ""
    Speaker.speak(word())
  }

}
