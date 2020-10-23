package net.degoes

import java.time.Instant
import java.time.Duration
import net.degoes.events.Event.DeviceEvent

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  case class Visa(number: Int, name: String, expDate: java.time.YearMonth, securityCode: Int) extends CreditCard

  // if we want to support different card vendors with different data we want to use a sum type
  sealed trait CreditCard {
    def number: Int
    def name: String
  }

  object CreditCard {
    case class Visa(number: Int, name: String, secCode: SecurityCode)       extends CreditCard
    case class Amex(number: Int, name: String, expiry: java.time.YearMonth) extends CreditCard
  }

  // using smart constructors
  class SecurityCode private (value: Int)
  object SecurityCode {
    def fromInt(int: Int): Option[SecurityCode] =
      if (int >= 0 && int.toString.length >= 3 && int.toString.length() <= 4) Some(new SecurityCode(int))
      else None

  }

  // correct but very painful and elaborate example
  // sealed trait SecurityCode
  // object SecurityCode {
  //   case class Four(_1: Digit, _2: Digit, _3: Digit, _4: Digit) extends SecurityCode
  //   case class Three(_1: Digit, _2: Digit, _3: Digit)           extends SecurityCode
  // }

  // type Digit

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  // type Product

  // Mine
  // sealed trait Product {
  // def name: String
  // def sku: Long
  // def price: Price
  // }
  // object Product {
  // case class Physical(name: String, price: Price, sku: Long) extends Product
  // case class Digital(name: String, price: Price, sku: Long)  extends Product
  // }
  // type Price

  //============= DeGoes =================
  // best practice

  final case class Product(id: String, price: Price, productType: ProductType) { self =>
    def increasePrice: Product =
      self.copy(price = self.price)
  }
  sealed trait ProductType
  object ProductType {
    case class Book(isbn: String, title: String) extends ProductType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  // type PricingScheme

  // Mine
//   abstract class Price(value: Int, currency: Currency, frequency: Frequency)
//   object Price {
//     case class Recurring(value: Int, currency: Currency, frequency: Frequency) extends Price(value, currency, frequency)
//     case class OnceOff(value: Int, currency: Currency)                         extends Price(value, currency, Frequency.Once)
//   }

//   type Currency
//   sealed trait Frequency
//   object Frequency {
//     case object Once     extends Frequency
//     case object Monthly  extends Frequency
//     case object Annually extends Frequency
//   }
// }

// =========== Degoes ===================
  sealed trait PricingScheme
  object PricingScheme {
    final case class OneTime(price: Price)                                       extends PricingScheme
    final case class Recurring(price: Price, start: Instant, interval: Duration) extends PricingScheme
  }
  type Price
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  // final case class Event(id: Int, time: Instant, eventType: EventType)

  // sealed trait EventType
  // case class UserEvent(id: Int, userName: String) extends EventType
  // case class DeviceEvent(id: Int, time: Instant)  extends EventType

  // case class SensorUpdated(time: Instant, reading: Option[Double])    extends EventType
  // case class DeviceActivated(time: Instant)                           extends EventType
  // case class UserPurchase(item: String, price: Double, time: Instant) extends EventType
  // case class UserAccountCreated(time: Instant)                        extends EventType

  // =========== Degoes ===============
  final case class Event[+A](id: Int, time: Instant, payload: A)

  sealed trait EventType
  final case class UserEvent(id: Int, userName: String, userEventType: UserEventType) extends EventType
  final case class DeviceEvent(time: Instant, deviceEventType: DeviceEventType)       extends EventType

  sealed trait UserEventType
  object UserEventType {
    case class UserPurchase(item: String, price: Double) extends UserEventType
    case object UserAccountCreated                       extends UserEventType
  }

  sealed trait DeviceEventType
  object DeviceEventType {
    case class SensorUpdated(reading: Option[Double]) extends DeviceEventType
    case object DeviceActivated                       extends DeviceEventType
  }

  // object Event {
  //   case class UserEvent(id: Int, userName: String) extends Event
  //   case class DeviceEvent(id: Int, time: Instant)  extends Event
  // }

  // abstract class Event(val id: Int) {
  //   def time: Instant
  // }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  // trait UserEvent extends Event {
  //   def userName: String
  // }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  // trait DeviceEvent extends Event {
  //   def deviceId: Int
  // }

}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  type Document

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  type AccessType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  type DocPermissions
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  type Customer

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  type AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  type Account
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
