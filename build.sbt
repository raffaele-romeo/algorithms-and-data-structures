import Dependencies.Libraries

name := "algorithms-and-data-structures"
version := "0.1"
scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  Libraries.cats,
  Libraries.zio,

  //TEST
  Libraries.scalaTest
)
