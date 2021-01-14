import com.dslplatform.compiler.client.parameters.{Settings => DslSettings}
import com.dslplatform.compiler.client.parameters.Targets.{Option => DslTarget}
import com.dslplatform.sbt.SbtDslPlatformPlugin.autoImport.{dsl => revenj}

name := "$name$"
version := "1.0"
scalaVersion := "2.13.4"

enablePlugins(SbtDslPlatformPlugin)

libraryDependencies += "net.revenj" %% "revenj-akka" % "1.1.2"

Compile/revenj/dslNamespace := "$organization$"
Compile/revenj/dslSources += (DslTarget.REVENJ_SCALA -> sourceManaged.value / "main")
Compile/revenj/dslPostgres := "127.0.0.1:5432/$name$_db?user=$name$_db&password=$name$"
Compile/revenj/dslApplyMigration := true
Compile/revenj/dslSettings ++= Seq(DslSettings.Option.JACKSON)
Compile/revenj/dslResourcePath := Some((Compile/revenj/resourceDirectory).value / "META-INF" / "services")
Compile/revenj/dslForce := true
