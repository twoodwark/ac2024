val scala3Version = "3.5.2"
fork := true
run / connectInput := true
lazy val root = project
  .in(file("."))
  .settings(
    name := "ac2024",
    version := "0.1.0-SNAPSHOT",
    outputStrategy := Some(StdoutOutput),
    scalacOptions := Seq("-unchecked", "-deprecation"),
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
