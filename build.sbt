/*
 * Copyright 2019 Daniel Spiewak
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

ThisBuild / baseVersion := "0.6"

ThisBuild / organization := "com.codecommit"
ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / strictSemVer := false

ThisBuild / crossScalaVersions := Seq("0.24.0", "0.25.0-RC1", "2.12.11", "2.13.2")

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@11", "adopt@14", "graalvm@20.1.0")

Global / homepage := Some(url("https://github.com/djspiewak/coop"))

Global / scmInfo := Some(
  ScmInfo(
    url("https://github.com/djspiewak/coop"),
    "git@github.com:djspiewak/coop.git"))

lazy val root = project.in(file(".")).aggregate(core.jvm, core.js)
  .settings(noPublishSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    name := "coop",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % "2.1.1",
      "org.typelevel" %%% "cats-mtl"  % "1.0-9b8941d",

      "org.specs2" %%% "specs2-core" % "4.9.4" % Test))
  .settings(dottyLibrarySettings)
  .settings(dottyJsSettings(ThisBuild / crossScalaVersions))

