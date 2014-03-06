name := "moira"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.0" % "test",
                            "org.scalafx" %% "scalafx" % "1.0.0-M7")

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

fork := true
