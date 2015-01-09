## MUCLR ##
*    Multi-User Common Lisp REPL
*    Version 1.10.101 Alpha

**Official MUCLR/muclr.org Canonical Specification Version 1.10.101**

#### Table of Contents ####
+ 01. PURPOSE
+ 02. ABSTRACT
+ 03. LEGAL
+ 04. MUCLR PACKAGE
 + 01. REGISTRAR
 + 02. SERVER
 + 03. CLIENT
+ 05. MUCLR API
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

abstract


## 03. LEGAL ##

See file `LICENSE` in the project's root directory for licensing information.


## 04. MUCLR PACKAGE ##

The Common Lisp package `MUCLR` is composed of three primary sub-systems, the
systems `MUCLR-REGISTRAR`, `MUCLR-SERVER`, and `MUCLR-CLIENT`.

### 01. REGISTRAR ###
for servers to be catalogued
### 02. SERVER ###
creating instances of muclr
### 03. CLIENT ###
querying registrar for servers and communicating with server instances
## 05. MUCLR API ###
yay this is the actual API
### 01. CLIENT TO SERVER ###
stuff
### 02. SERVER TO REGISTRAR ###
stuff
### 03. CLIENT TO REGISTRAR ###
stuff