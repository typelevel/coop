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

ThisBuild / baseVersion := "1.2"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"

ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / strictSemVer := false

ThisBuild / crossScalaVersions := Seq("3.1.3", "2.12.16", "2.13.8")

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@1.11", "adopt@1.14", "graalvm-ce-java8@20.2.0")

ThisBuild / homepage := Some(url("https://github.com/typelevel/coop"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/typelevel/coop"),
    "git@github.com:typelevel/coop.git"))

lazy val root = project.in(file(".")).aggregate(core.jvm, core.js, core.native)
  .enablePlugins(NoPublishPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("core"))
  .settings(
    name := "coop",
    libraryDependencies += "org.specs2" %%% "specs2-core" % "4.16.0" % Test)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % "2.11.0",
      "org.typelevel" %%% "cats-mtl"  % "1.3.0"))
