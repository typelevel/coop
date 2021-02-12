/*
 * Copyright 2020 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

ThisBuild / baseVersion := "1.0"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"

ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / strictSemVer := false

ThisBuild / crossScalaVersions := Seq("3.0.0-M2", "3.0.0-M3", "2.12.12", "2.13.4")

// Restore running the CI on Java 15 (https://github.com/lampepfl/dotty/issues/10131).
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@1.11", "graalvm-ce-java8@20.2.0")

ThisBuild / homepage := Some(url("https://github.com/typelevel/coop"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/typelevel/coop"),
    "git@github.com:typelevel/coop.git"))

lazy val root = project.in(file(".")).aggregate(core.jvm, core.js)
  .enablePlugins(NoPublishPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    name := "coop",
    libraryDependencies += "org.specs2" %%% "specs2-core" % "4.10.6" % Test)
  .settings(dottyLibrarySettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % "2.4.1",
      "org.typelevel" %%% "cats-mtl"  % "1.1.1"))

