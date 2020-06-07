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

name := "coop"

ThisBuild / baseVersion := "0.5"

ThisBuild / organization := "com.codecommit"
ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / strictSemVer := false

ThisBuild / crossScalaVersions := Seq("2.12.11", "0.24.0-RC1", "2.13.2")

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@11", "adopt@14", "graalvm@20.1.0")
ThisBuild / githubWorkflowBuild := WorkflowStep.Sbt(List("ci"))
ThisBuild / githubWorkflowPublishTargetBranches := Seq()    // disable the publication job

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "2.1.1",
  "org.typelevel" %% "cats-mtl"  % "1.0-9b8941d",

  "org.specs2" %% "specs2-core" % "4.8.1" % Test)

libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))

Global / homepage := Some(url("https://github.com/djspiewak/coop"))

Global / scmInfo := Some(
  ScmInfo(
    url("https://github.com/djspiewak/coop"),
    "git@github.com:djspiewak/coop.git"))
