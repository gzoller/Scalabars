
# Scalabars

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg)](https://bintray.com/blocke/releases/scalabars/_latestVersion)
[![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/Scalabars)
[![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/Scalabars?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/Scalabars&amp;utm_campaign=Badge_Grade)
[![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/Scalabars)

Scalabars is a Scala implementation of the Handlebars templating engine.  It is mostly compatible with Javascript-native Handlebars, with some exceptions that just don't make sense in the Scala world.

Helpers can be written in Scala, or in JavaScript.  JavaScript helpers should be compatible with Handlebars as long as authors don't do anything too crazy in their scripts (e.g. call a bunch of external JavaScript libraries, for example).

The advantage of writing helpers in pure Scala and not using any JavaScript helpers is that the engine should then be entirely thread-safe, which of course JavaScript-native Handlebars is not.

## Use

stuff here

## Included Helpers (thread-safe)

#### Stock Handlebars Built-In Helpers
* each
* if
* until
* with

#### Extra Helpers We Included
* and
* eq
* ne
* or

### A word about Javascript on the JVM...
Scalabars currently uses the Nashorn JavaScript engine embedded in the JVM, or at least it was until recently.  Unfortunately Nashorn is deprecated in favor of GraalVM, but since this VM isn't (as of this writing) even GA 1.0, it's not something we're going to support just yet as we need solutions that run in the enterprise today.

Although it works just fine, you'll see noisy Nashorn deprecation warnings if you do run any JavaScript helpers in Scalabars.  (Nashorn use is lazy in Scalabars so if you're 100% Scala you'll never see these warnings.)

This is an open invitation to anyone wishing to contribute an implementation using another JavaScript engine that's not going away!  I did try using the graal.js engine, but couldn't get it to work, but don't let that stop you from trying!

At some point when Graalvm takes off, we'll circle back and do an implementation for it if the demand is there.

*Blöcke*
