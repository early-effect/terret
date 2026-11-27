package terret.transcript

/** How many events one session log keeps. The oldest entries drop once the log is past `limit`. */
opaque type JournalBound = Int

object JournalBound:
  val DefaultSize: Int = 4096

  def default: JournalBound = DefaultSize

  def from(n: Int): Either[JournalBoundError, JournalBound] =
    if n > 0 then Right(n) else Left(JournalBoundError.NonPositive(n))

  extension (bound: JournalBound) def limit: Int = bound
end JournalBound

enum JournalBoundError:
  case NonPositive(value: Int)
