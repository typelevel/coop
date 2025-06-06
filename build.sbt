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

ThisBuild / tlBaseVersion := "1.3"
ThisBuild / startYear := Some(2021)

ThisBuild / crossScalaVersions := Seq("3.3.3", "2.12.20", "2.13.15")
ThisBuild / tlVersionIntroduced := Map("3" -> "1.1.1")

ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / tlCiScalafmtCheck := false

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure).in(file("core"))
  .settings(
    name := "coop",
    libraryDependencies += "org.specs2" %%% "specs2-core" % "4.20.9" % Test)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % "2.13.0",
      "org.typelevel" %%% "cats-mtl"  % "1.5.0"))
  .nativeSettings(
    tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "1.3.0").toMap
  )
