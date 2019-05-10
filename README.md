
# Scalabars

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg)](https://bintray.com/blocke/releases/scalabars/_latestVersion)
[![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/Scalabars)
[![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/Scalabars?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/Scalabars&amp;utm_campaign=Badge_Grade)
[![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/Scalabars)

Scalabars is a Scala implementation of the Handlebars templating engine.  It is closely compatible with Javascript-native Handlebars with some exceptions that just don't make sense in the Scala world.

Helpers can be written in Scala, or in JavaScript.  JavaScript helpers should be compatible with Handlebars as long as authors don't do anything too crazy in their scripts (e.g. call a bunch of external JavaScript libraries for example).

The advantage of writing helpers in pure Scala and not using any JavaScript helpers is that the engine should then be entirely thread-safe, which of course JavaScript-native Handlebars is not.

## Use

stuff here


