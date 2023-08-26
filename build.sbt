name := "fp scala"

version := "0.1"

scalaVersion := "2.13.11"

lazy val taxReport = (project in file("./tax-report"))
  .settings(name := "TaxReport")

libraryDependencies += "com.vividsolutions" % "jts" % "1.13"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4"