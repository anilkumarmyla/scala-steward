package org.scalasteward.core.repoconfig

import cats.Eq
import io.circe.{Decoder, Encoder}

sealed trait IncludeScalaStrategy {
  def name: String
}

object IncludeScalaStrategy {
  final case object Yes extends IncludeScalaStrategy { val name = "yes" }
  final case object Draft extends IncludeScalaStrategy { val name = "draft" }
  final case object No extends IncludeScalaStrategy { val name = "no" }

  val default: IncludeScalaStrategy = Draft

  def fromString(value: String): IncludeScalaStrategy =
    value.trim.toLowerCase match {
      case Yes.name   => Yes
      case Draft.name => Draft
      case No.name    => No
      case _          => default
    }

  def fromBoolean(value: Boolean): IncludeScalaStrategy =
    if (value) Yes else No

  implicit val includeScalaStrategyDecoder: Decoder[IncludeScalaStrategy] =
    Decoder[Boolean].map(fromBoolean).or(Decoder[String].map(fromString))

  implicit val includeScalaStrategyEncoder: Encoder[IncludeScalaStrategy] =
    Encoder[String].contramap(_.name)

  implicit val includeScalaStrategyEq: Eq[IncludeScalaStrategy] =
    Eq.fromUniversalEquals
}
