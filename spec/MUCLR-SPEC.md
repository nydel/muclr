## MUCLR ##
*    Multi-User Common Lisp REPL
*    Version 1.10.101 Alpha

**Official MUCLR/muclr.org Canonical Specification Version 1.10.101**

#### Table of Contents ####
+ 01. PURPOSE
+ 02. ABSTRACT
+ 03. LEGAL
+ 04. SECURITY
 + 01. LOGIN CREDENTIALS
 + 02. PLATFORM LEASE
 + 03. CLIENT TOKENS
 + 04. REPL STATE BACKUP
+ 05. MUCLR PACKAGE
 + 01. REGISTRAR
 + 02. SERVER
 + 03. CLIENT
+ 06. MUCLR API
 + 01. CLIENT TO SERVER
 + 02. SERVER TO REGISTRAR
 + 03. CLIENT TO REGISTRAR
 

## 01. PURPOSE ##

`Multi-User Common Lisp REPL`, or `MUCLR`, means to connect Common Lisp programmers to a shared REPL to which they can all send forms for evaluation
in real time.

Each user of any given `MUCLR Platform` connects from his own instance of
Common Lisp using the `:MUCLR` package. The `REPL` from which the `CLIENT`
connects is separate from the `PLATFORM` so that users may issue forms for
evaluation to the `MUCLR SERVER INSTANCE` aka `PLATFORM` but may not control
the `REPL` of any other connected user.

## 02. ABSTRACT ##

We begin first with `Multi-User *Common Lisp* REPL` intending to connect `CL` hackers to a shared space, but we design the specification that it may soon be forked, canonically or otherwise, to apply the same theory to other dialects of the `Lisp` programming languages as well as non-Lisp interpreted languages such as `Python` and `Perl` which can, like `Common Lisp`, be used as `Read Evaluate Print Loop` instances.


## 03. LEGAL ##

See file `LICENSE` in the project's root directory for licensing information.


## 04. SECURITY ##

Any `SOCKET` connection over the IP Suite is potentially dangerous, but limitless control, simple credentials, server leasing and token-based session validation are just a few of the tools implemented into the canonical distribution of `MUCLR` at the moment.

#### 01. LOGIN CREDENTIALS ####

more soon

#### 02. PLATFORM LEASE ####

more soon

#### 03. CLIENT TOKENS ####

more soon

#### 04. REPL STATE BACKUP ####

more soon

## 05. MUCLR PACKAGE ##

The Common Lisp package `MUCLR` is composed of three primary sub-systems, the
systems `MUCLR-REGISTRAR`, `MUCLR-SERVER`, and `MUCLR-CLIENT`.

### 01. REGISTRAR ###

The `REGISTRAR` serves primarily as intermediary between `SERVER` and `CLIENT`, a balancing act that requires several primary features. Many of these depend upon class objects defined within the scope of `CLOS` and most - perhaps all - of them at least deal with the `Common Lisp Object System` as utilized by `MUCLR` specifically.

#### 01. PLATFORM ####

After a `SERVER` is created, it can be registered with the `REGISTRAR`, at which point it becomes a `PLATFORM`.

The `CLIENT` can query the `REGISTRAR` for a list of `PLATFORMS` and connect to one directly using the information provided.

#### 02. LEASE ####

The `LEASE` object informs the validity of a `SERVER` aka `PLATFORM` or `CLIENT` registration. A `SERVER` must request &/or present a valid `LEASE` in order to proceed to query for `PLATFORM` status aproval. A `CLIENT` must request &/or present a valid `LEASE` as the initialization of its credentials for querying the `REGISTRAR` - as in for a list of `PLATFORMS` - as well as for querying a `PLATFORM` aka querying a `SERVER` through the `REGISTRAR` - and as well as for querying a `SERVER` directly.

Each `LEASE` is comprised, in part, of a start-time and a duration. Should a `SERVER` allow for its `LEASE` to expire, it will no longer be validated as, or listed among other, `PLATFORM` objects. However, `LEASE` objects can be renewed through query to the `REGISTRAR` for both `PLATFORMS` and validated `CLIENTS` alike, that is, a `LEASE` can be made to continue to exist with validity for an indefinite period of time.

#### 03. CLIENT ####

As a `SERVER` becomes a `PLATFORM` with a valid `LEASE` and a query for registration, so does a `CLIENT` of default type `TOURIST` become of the registered type `VISA` in the same manner. Some `PLATFORM` instances will require a `VISA CLIENT` whereas others will accept `TOURIST CLIENT` connections as `SPECTATORS` or even as fully-fledged users.

### 02. SERVER ###

creating instances of muclr

### 03. CLIENT ###

A `CLIENT` is considered of type `TOURIST` unless it has obtained a `LEASE` as well as validity from the `REGISTRAR` at which point it may become a `VISA`.

## 05. MUCLR API ###
yay this is the actual API
### 01. CLIENT TO SERVER ###
stuff
### 02. SERVER TO REGISTRAR ###
stuff
### 03. CLIENT TO REGISTRAR ###
stuff