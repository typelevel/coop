import sbt._, Keys._

import sbtspiewak.SpiewakSonatypePlugin
import xerial.sbt.Sonatype.SonatypeKeys._

object Publish extends AutoPlugin {

  override def requires = SpiewakSonatypePlugin
  override def trigger = allRequirements

  override def projectSettings = Seq(
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    // Workaround until sbt-spiewak-sonatype updates kind-projector
    // https://github.com/djspiewak/sbt-spiewak/pull/131
    libraryDependencies ~= {
      _.map { dep =>
        if (dep.name == "kind-projector") dep.withRevision("0.13.3")
        else dep
      }
    })
}
