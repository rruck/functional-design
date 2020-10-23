package net.degoes

import java.io.File
import net.degoes.data_processing.Schema.DateTime
import java.time.temporal.TemporalField

/*
 * INTRODUCTION
 *
 * In Functional Design, a functional domain consists of three things:
 *
 * 1. A set of types that model a solution to a domain problem.
 *
 * 2. Constructors that allow constructing simple solutions.
 *
 * 3. Operators that solving more complex problems by transforming
 *    and combining solutions for subproblems.
 *
 * Functional domains allow modeling solutions to problems in a specific domain.
 * Done properly, a small set of primitives can be so powerful, they can be used
 * compositionally to describe all possible solutions in that domain.
 *
 * A functional domain can be regarded as a type of internal domain-specific
 * language (DSL), which is designed specifically for expressing compositional
 * solutions to some category of domain problems.
 *
 * ZIO is an example of a domain for input/output, whose effect type lets you
 * solve async/concurrent/resourceful problems, and whose operators let you
 * assemble large solutions from small solutions.
 *
 * In this section, you'll learn about designing domains using ADTS,
 * constructors, and composable operators.
 */

/**
 * SPREADSHEET - EXERCISE SET 1
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet {
  trait Spreadsheet {
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  sealed trait Value
  object Value {
    final case class Error(message: String) extends Value
    final case class Str(value: String)     extends Value
    final case class Dbl(value: Double)     extends Value
  }

  /**
   * EXERCISE 1
   *
   * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
   * computed from a `Spreadsheet`.
   */
  final case class CalculatedValue(eval: Spreadsheet => Value) { self =>

    /**
     * EXERCISE 2
     *
     * Add an operator that returns a new `CalculatedValue` that is the negated version of this one.
     */
    // def unary_- : CalculatedValue = CalculatedValue { sheet =>
    //   self.eval(sheet) match {
    //     case Value.Str(v)   => Value.Str(s"-$v")
    //     case Value.Dbl(v)   => Value.Dbl(v * -1)
    //     case Value.Error(e) => Value.Error(e)
    //   }
    // }

    def unary_- : CalculatedValue = CalculatedValue { sheet =>
      self.eval(sheet) match {
        case Value.Str(v)   => Value.Error("Cannot negate string")
        case Value.Dbl(v)   => Value.Dbl(v * -1)
        case Value.Error(e) => Value.Error(e)
      }
    }

    /**
     * EXERCISE 3
     *
     * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
     * calculated values.
     */
    def +(that: CalculatedValue): CalculatedValue =
      binaryOp(that)("") {
        case (Value.Dbl(v1), Value.Dbl(v2)) => Value.Dbl(v1 + v2)
      }

    /**
     * EXERCISE 4
     *
     * Add a binary operator `-` that returns a new `CalculatedValue` that is the difere;nce of the
     * two calculated values.
     */
    def -(that: CalculatedValue): CalculatedValue =
      self + (-that)

    protected def binaryOp(that: CalculatedValue)(error: String)(
      f: PartialFunction[(Value, Value), Value]
    ): CalculatedValue = CalculatedValue { s =>
      f.lift((self.eval(s), that.eval(s))).getOrElse(Value.Error(error))
    }
  }
  object CalculatedValue {

    /**
     * EXERCISE 5
     *
     * Add a constructor that makes an `CalculatedValue` from a `Value`.
     */
    def const(contents: Value): CalculatedValue = CalculatedValue(_ => contents)

    /**
     * EXERCISE 6
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue = CalculatedValue { s =>
      s.valueAt(col, row).eval(s)
    }
  }

  /**
   * EXERCISE 7
   *
   * Describe a cell whose contents are the sum of the cells at (0, 0) and (1, 0).
   */
  lazy val cell1: Cell =
    Cell(2, 0, CalculatedValue.at(0, 0) + CalculatedValue.at(1, 0))

}

/**
 * ETL - EXERCISE SET 2
 *
 * Consider an application designed to extract, transform, and load data.
 */
object etl {
  import scala.util._

  /**
   * Represents a row of data.
   */
  final case class DataRow(row: Map[String, DataValue]) { self =>
    def delete(name: String): DataRow = DataRow(row - name)

    def map(name: String)(f: PartialFunction[DataValue, DataValue]): DataRow =
      row.get(name).fold(self)(v => f.lift(v).fold(self)(v => DataRow(row.updated(name, v))))

    def rename(oldName: String, newName: String): DataRow =
      DataRow(row.get(oldName).fold(row)(value => (row - oldName).updated(newName, value)))

    def coerce(name: String, dtype: DataType): DataRow =
      row.get(name).fold(self)(v => v.coerce(dtype).fold(self)(v => DataRow(row + (name -> v))))
  }

  /**
   * Represents a stream of data.
   */
  final case class DataStream(foreach: (Try[DataRow] => Unit) => Unit) { self =>
    def coerce(name: String, dtype: DataType): DataStream = self.map(_.coerce(name, dtype))

    def delete(name: String): DataStream = self.map(_.delete(name))

    def orElse(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach {
          case Failure(exception) => that.foreach(callback)
          case x                  => callback(x)
        }
      }

    def map(f: DataRow => DataRow): DataStream =
      DataStream(callback => self.foreach(a => callback(a.map(f))))

    def mapColumn(name: String)(f: PartialFunction[DataValue, DataValue]): DataStream =
      self.map(_.map(name)(f))

    def merge(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach(callback)
        that.foreach(callback)
      }

    def rename(oldName: String, newName: String): DataStream =
      self.map(_.rename(oldName, newName))
  }

  /**
   * EXERCISE 1
   *
   * Design a data type that models sources and sinks in an ETL pipeline. Assume
   * your business requires you to extract data from (and load data to) FTP sites,
   * URLs, AWS S3 buckets, and databases described by JDBC connection strings.
   *
   * Also mock out, but do not implement, a method on each repository type called
   * `load`, which returns a `DataStream`.
   */
  sealed trait DataRepo {
    def load: DataStream = ???
  }
  object DataRepo {
    case class FTPSite(host: String, port: Int)        extends DataRepo
    case class Url(path: String, format: FileFormat)   extends DataRepo
    case class AwsS3(bucketName: String, path: String) extends DataRepo
    case class JdbcDb(connectionUrl: String)           extends DataRepo
  }
  sealed trait FileFormat
  object FileFormat {
    case object Json extends FileFormat
    case object Csv  extends FileFormat
    case object Xml  extends FileFormat
  }

  /**
   * EXERCISE 2
   *
   * Design a data type that models the type of primitives the ETL pipeline
   * has access to. This will include string, numeric, and date/time data.
   */
  sealed trait DataType
  object DataType {
    case object String   extends DataType
    case object Numeric  extends DataType
    case object DateTime extends DataType
    case object Null     extends DataType
  }

  /**
   * EXERCISE 3
   *
   * Design a data type that models a value. Every value should have a `DataType`
   * that identifies its type (string, numeric, or data/time), and a `coerce` method
   * to coerce the value into another type.
   *
   * Be sure to model null, string, and integer, at the very least!
   */
  sealed trait DataValue {
    def dataType: DataType

    def coerce(otherType: DataType): Option[DataValue]
  }
  object DataValue {
    case class Str(value: String) extends DataValue { self =>
      def dataType: DataType = DataType.String

      def coerce(otherType: DataType): Option[DataValue] =
        otherType match {
          case DataType.String   => Some(self)
          case DataType.Numeric  => ???
          case DataType.DateTime => ???
          case DataType.Null     => Some(DataValue.Null)
        }
    }

    case object Null extends DataValue { self =>
      def dataType: DataType = DataType.Null

      def coerce(otherType: DataType): Option[DataValue] =
        otherType match {
          case DataType.String   => Some(DataValue.Str("null"))
          case DataType.Numeric  => None
          case DataType.DateTime => None
          case DataType.Null     => Some(self)
        }
    }

    case class Num(value: Double) extends DataValue { self =>
      def dataType: DataType = DataType.Numeric

      def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.String   => Some(DataValue.Str(self.value.toString))
        case DataType.Numeric  => Some(self)
        case DataType.DateTime => None
        case DataType.Null     => Some(DataValue.Null)
      }
    }

    case class DateTime(value: java.time.LocalDateTime) extends DataValue { self =>
      def dataType: DataType = DataType.DateTime

      def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.String   => Some(DataValue.Str(self.value.toString))
        case DataType.Numeric  => ???
        case DataType.DateTime => Some(self)
        case DataType.Null     => Some(DataValue.Null)
      }
    }
  }

  /**
   * EXERCISE 4
   *
   * `Pipeline` is a data type that models a transformation from an input data
   * set into an output data step, as a series of one or more individual
   * operations.
   *
   * Create a model of a pipeline, using `DataStream`.
   */
  final case class Pipeline(load: () => DataStream) { self =>

    /**
     * EXERCISE 5
     *
     * Add a `merge` operator that models the merge of the output of this
     * pipeline with the output of the specified pipeline.
     *
     * {{{
     * Merge Associativity:  (p1 merge p2) merge p3 == p1 merge (p2 merge p3)
     * Merge Identity:       p merge Pipeline.empty == Pipeline.empty merge p == p
     * Merge Commutativity:  p1 merge p2 == p2 merge p1
     * Merge Duplication:    ???
     * }}}
     */
    def merge(that: Pipeline): Pipeline = Pipeline { () =>
      self.load().merge(that.load())
    }

    /**
     * EXERCISE 6
     *
     * Add an `orElse` operator that models applying this pipeline, but if it
     * fails, switching over and trying another pipeline.
     */
    def orElse(that: Pipeline): Pipeline = Pipeline { () =>
      self.load().orElse(that.load())
    }

    /**
     * EXERCISE 7
     *
     * Add an operator to rename a column in a pipeline.
     */
    def rename(oldName: String, newName: String): Pipeline = Pipeline { () =>
      self.load().rename(oldName, newName)
    }

    /**
     * EXERCISE 8
     *
     * Add an operator to coerce a column into a specific type in a pipeline.
     */
    def coerce(column: String, newType: DataType): Pipeline = Pipeline { () =>
      self.load().coerce(column, newType)
    }

    /**
     * EXERCISE 9
     *
     * Add an operator to delete a column in a pipeline.
     */
    def delete(column: String): Pipeline = Pipeline { () =>
      self.load().delete(column)
    }

    /**
     * EXERCISE 10
     *
     * To replace nulls in the specified column with a specified value.
     */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = Pipeline { () =>
      self.load().mapColumn(column) {
        case DataValue.Null => defaultValue
      }
    }
  }
  object Pipeline {

    /**
     * EXERCISE 11
     *
     * Add a constructor for `Pipeline` that models extraction of data from
     * the specified data repository.
     */
    def extract(repo: DataRepo): Pipeline = Pipeline(() => repo.load)
  }

  /**
   * EXERCISE 12
   *
   * Create a pipeline that models extracts data from a URL, replacing all null
   * "age" columns with "0" as the default age, which renames a column "fname"
   * into a column "first_name", and which coerces the "age" column into an
   * integer type.
   */
  lazy val pipeline: Pipeline =
    Pipeline
      .extract(DataRepo.Url("foo", FileFormat.Csv))
      .replaceNulls("age", DataValue.Num(0))
      .rename("fname", "first_name")
      .coerce("age", DataType.Numeric)
}

/**
 * REAL ESTATE APP - GRADUATION PROJECT
 *
 * Consider a real estate app that must regularly fetch third-party pricing data
 * according to specified schedules. These schedules can be quite complicated,
 * although they possess regular structure (e.g. every fifth Tuesday, and hourly
 * on Wednesdays). The business considers it acceptable to create the schedules in
 * code (rather than reading them from a database).
 */
object pricing_fetcher {
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Sunday    extends DayOfWeek
    case object Monday    extends DayOfWeek
    case object Tuesday   extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday  extends DayOfWeek
    case object Friday    extends DayOfWeek
    case object Saturday  extends DayOfWeek
  }

  final case class Time(minuteOfHour: Int, hourOfDay: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, monthOfYear: Int)

  /**
   * EXERCISE 1
   *
   * `Schedule` is a data type that models a schedule, which has the ability to
   * indicate whether at any given `java.time.Instant`, it is time to fetch the
   * pricing data set.
   */
  final case class Schedule(fetchNow: Time => Boolean) { self =>
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     */
    def union(that: Schedule): Schedule = Schedule { time =>
      self.fetchNow(time) || that.fetchNow(time)
    }

    def ||(that: Schedule): Schedule = union(that)

    /**
     * EXERCISE 3
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the intersection of those schedules. That is, the fetch will occur
     * only when both of the schedules would have performed a fetch.
     */
    def intersection(that: Schedule): Schedule = Schedule { time =>
      self.fetchNow(time) && that.fetchNow(time)
    }

    def &&(that: Schedule): Schedule = intersection(that)

    /**
     * EXERCISE 4
     *
     * Create a unary operator that returns a schedule that will never fetch
     * when the original schedule would fetch, and will always fetch when the
     * original schedule would not fetch.
     */
    def negate: Schedule = Schedule { time =>
      !self.fetchNow(time)
    }
  }
  object Schedule {

    /**
     * EXERCISE 5
     *
     * Create a constructor for Schedule that models fetching on specific weeks
     * of the month.
     */
    def weeks(weeks: Int*): Schedule =
      Schedule(time => weeks.contains(time.weekOfMonth))

    /**
     * EXERCISE 6
     *
     * Create a constructor for Schedule that models fetching on specific days
     * of the week.
     */
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule =
      Schedule(time => daysOfTheWeek.contains(time.dayOfWeek))

    /**
     * EXERCISE 7
     *
     * Create a constructor for Schedule that models fetching on specific
     * hours of the day.
     */
    def hoursOfTheDay(hours: Int*): Schedule =
      Schedule(time => hours.contains(time.hourOfDay))

    /**
     * EXERCISE 8
     *
     * Create a constructor for Schedule that models fetching on specific minutes
     * of the hour.
     */
    def minutesOfTheHour(minutes: Int*): Schedule =
      Schedule(time => minutes.contains(time.minuteOfHour))
  }

  /**
   * EXERCISE 9
   *
   * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 PM,
   * and at 5:30, 6:30, and 7:30 every Thursday.
   */
  lazy val wednesdaySchedule: Schedule =
    Schedule.daysOfTheWeek(DayOfWeek.Wednesday) &&
      (Schedule.hoursOfTheDay(6, 12) && Schedule.minutesOfTheHour(0))

  lazy val thursdaySchedule: Schedule =
    Schedule.daysOfTheWeek(DayOfWeek.Thursday) &&
      (Schedule.hoursOfTheDay(5, 6, 7) && Schedule.minutesOfTheHour(30))

  lazy val schedule: Schedule = wednesdaySchedule union thursdaySchedule
}
