package tasks.mvc

import u04.monads.States.State
import u04.monads.{CounterState, Monads, States, WindowState}

object WindowWithField:

  trait WindowStateWithField extends WindowState:
    def addField(text: String, name: String): State[Window, Unit]
    def getFieldText(fieldName: String): State[Window, String]

  object WindowStateWithFieldImpl extends WindowStateWithField:

    import u04.monads.SwingFunctionalFacade.*
    import u03.extensionmethods.Streams.*

    type Window = Frame

    override def initialWindow: Window = createFrame

    override def setSize(width: Int, height: Int): State[Window, Unit] =
      State(w => (w.setSize(width, height), {}))

    override def addButton(text: String, name: String): State[Window, Unit] =
      State(w => (w.addButton(text, name), {}))

    override def addLabel(text: String, name: String): State[Window, Unit] =
      State(w => (w.addLabel(text, name), {}))

    override def addField(text: String, name: String): State[Window, Unit] =
      State(w => (w.addField(text, name), {}))

    override def getFieldText(fieldName: String): State[Window, String] =
      State(w => (w, w.getFieldText(fieldName)))

    override def toLabel(text: String, name: String): State[Window, Unit] =
      State(w => (w.showToLabel(text, name), {}))

    override def show(): State[Window, Unit] =
      State(w => (w.show, {}))

    override def exec(cmd: => Unit): State[Window, Unit] =
      State(w => (w, cmd))

    override def eventStream(): State[Window, Stream[String]] =
      State(w => (w, Stream.generate(() => w.events().get)))

object CounterWithSets:

  trait CounterWithSet extends CounterState:
    def set(value: Int): State[Counter, Unit]

  object CounterStateWithSetImpl extends CounterWithSet:
    opaque type Counter = Int

    override def initialCounter(): Counter = 0

    // giving (new_counter, result)
    override def inc(): State[Counter, Unit] = State(i => (i + 1, ()))
    override def dec(): State[Counter, Unit] = State(i => (i - 1, ()))
    override def reset(): State[Counter, Unit] = State(i => (0, ()))
    override def get(): State[Counter, Int] = State(i => (i, i))
    override def nop(): State[Counter, Unit] = State(i => (i, ()))
    override def set(value: Int): State[Counter, Unit] = State(_ => (value, ()))

@main def runMVC =
  import Monads.*, Monad.*, States.*, State.*, tasks.mvc.CounterWithSets.CounterStateWithSetImpl.*
  import tasks.mvc.WindowWithField.WindowStateWithFieldImpl.*
  import u03.extensionmethods.Streams.*

  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def vmv[SM, SV, AM, AV, T](fv: State[SV, T], m1: T => State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (_, t) = fv.run(sv)
      val (sm2, am) = m1(t).run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def parseNumber(text: String): Option[Int] =
    try
      Option.apply(text.toInt)
    catch
      case _ => Option.empty

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(600, 600)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addField(text = "", name = "SetField")
    _ <- addButton(text = "set", name = "SetButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(seq(reset(), get()), i => windowCreation(i.toString))
    _ <- seqN(events.map(_ match
      case "IncButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
      case "DecButton" => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
      case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
      case "SetButton" => vmv(getFieldText("SetField"), t => parseNumber(t).fold(get())(v => seq(set(v), get())), i => toLabel(i.toString, "Label1"))
      case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((initialCounter(), initialWindow))