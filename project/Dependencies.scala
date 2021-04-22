import sbt._

object Dependencies {

  object Versions {
    val cats = "2.5.0"
    val zio = "1.0.5"

    val scalaTest = "3.2.7"
  }

  object Libraries {
    val cats = "org.typelevel" %% "cats-core" % Versions.cats
    val zio =  "dev.zio" %% "zio" % Versions.zio

    val scalaTest = "org.scalatest" %% "scalatest" %  Versions.scalaTest % Test
  }
}
