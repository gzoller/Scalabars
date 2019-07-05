
# Scalabars  
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/scalabars/images/download.svg)](https://bintray.com/blocke/releases/scalabars/_latestVersion)
[![Build Status](https://img.shields.io/travis/gzoller/Scalabars.svg?branch=master)](https://travis-ci.org/gzoller/Scalabars)
[![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/Scalabars?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/Scalabars&amp;utm_campaign=Badge_Grade)
[![Coveralls branch](https://img.shields.io/coveralls/gzoller/Scalabars/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/Scalabars)

 ![scalabars](scalabars.jpg)
Scalabars is a Scala implementation of the Handlebars templating engine.  It is mostly compatible with  4.x series Handlebars, with some exceptions that just don't make sense in the Scala world.

Helpers can be written in Scala, or in JavaScript.  JavaScript helpers should be compatible with Handlebars as long as authors don't do anything too crazy in their scripts (e.g. call external JavaScript libraries, for example).

The advantage of writing helpers in pure Scala and not using any JavaScript helpers is that the engine should then be entirely thread-safe, which of course JavaScript-native Handlebars is not.

## Use

To install and use Scalabars include it in your projects by adding the following to your build.sbt:

    libraryDependencies ++= Seq("co.blocke" %% "scalabars" % "0.1.0")

## Developing Your Own Helpers

* [Scala](scalaHelper.md)
* [JavaScript](jsHelper.md)

## Included Helpers (thread-safe)

#### Stock Handlebars Built-In Helpers
* each
* if
* lookup
* until
* with

#### Extra Helpers We Included
Most of these (and the examples/documentation) can be found at [https://assemble.io/helpers/](https://assemble.io/helpers/)
* and
* any
* contains
* default
* empty
* eq
* first
* join
* last
* length
* lengthEquals
* include
* markdown
* ne
* raw
* sortEach
* or
* url
* withDrop
* withFirst
* withLast
* withLookup
* withTake

*Blöcke*