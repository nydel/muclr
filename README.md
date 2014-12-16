About :MUCLR

Know this Common Lisp system to comprise a part of the larger :MUCLR project,
which may contain multiple subsystems/subpackages nodally-related to the master.

Here have we :MUCLR-SERVER which creates server objects to optionally register
with the :MUCLR master listing - accept and handle incoming requests from end-
users - log in detail all activity optionally and with transparency.

The :MUCLR project is a management system involving a server and end-users of
different privilege levels. Its goal is ultimately to incorporate an abstract
social aspect, and its implementation, right into the Common Lisp REPL. This
will include a number of inherent prospects canonically yet will feature an
expansion mechanism allowing users to modify existing and create new widgets
whose purposes fit into the scope of :MUCLR. Also featured will be simple chat
and file sharing, both facets including encryption transparent to the user and
configurable to suit the cryptographic tastes of the individual.

A :MUCLR hub (we must decide on what we call a running instance of this software
along with the hub description, which will need to be registered somewhere for
listing) will be an object created inside a deamonized Common Lisp - or possibly
binary - process allocated as a thread on the serving computer system. We do not
like hub for the official term and we put a pin in this.

Temporarily referring to a MUCLR instance as a Platform.
