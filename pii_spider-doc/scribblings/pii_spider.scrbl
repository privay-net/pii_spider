#lang scribble/manual
@require[@for-label[pii_spider
                    racket/base]]

@title{pii_spider}
@author[(@author+email "Robert Postill" "robert@grinning-cat.com")]

@defmodule[pii_spider]

PII Spider is an application for finding private data.

It is the sad reality for many organisations that managing the private data they hold is beyond them.  Not least becuase they can't find it.  PII Spider intends to help by providing an automated way to scour data stores looking for data.

Right now Privay is alpha quality.  It has had no performance work done on it nor does it support anything but the most basic Postgresql databases.  Ideally it will be run out of a docker container but that's something else on the to-do list :)

@section{High Level Architecture}
The diagram below describes the high level architecture of PII Spider:
@image["pii_spider_architecture.png"]

The idea is that the rules are easily seperable and extendable.  I'm fully expecting rules and the crawler itself to get a lot more complicated.

The reporting is already complicated and needs to be less so.  That probably depends on how performance over large datastores is handled.
