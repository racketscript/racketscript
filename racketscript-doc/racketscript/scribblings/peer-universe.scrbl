#lang scribble/manual
@(require (for-label racket/base
                     2htdp/universe))

@title{Peer-Universe for RacketScript}
@author[(author+email "Ayden Diel" "aydendiel@gmail.com")]

@defmodule[racketscript/htdp/peer-universe]
@;{
    for some reason I can't link to the 2htdp/universe docs, so I just linked to the htdp docs instead
    }
Experimental implementation of Racket's @racket[2htdp/universe] library for @seclink["top" #:doc '(lib "racketscript/scribblings/racketscript.scrbl") "RacketScript"] using peer-to-peer connections. Used to create distributed programs where both the server and the clients run in the browser.

@itemlist[@item{@secref["getting-started"]}
          @item{@secref["how-it-works"]}
          @item{@secref["differences"]}]

@section[#:tag "getting-started"]{Getting Started}
Since this library is primarily an implementation of the @hyperlink["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{2htdp/universe} API, use those docs as your main reference. These docs will contain info about how @seclink["Peer-Universe_for_RacketScript"]{peer-universe} works and how it differs from the original, but won't contain an in-depth API description.

@margin-note*{
    See @hyperlink["https://github.com/leiDnedyA/rs-universe-server-test/blob/master/src/app.rkt"]{this example on github} for an example where the landing page has the user choose whether to start a @racket[universe] or @racket[big-bang] instance.
}

To use the library, you need to be running a separate @racket[universe] and @racket[big-bang] instance at the same time, both in separate browser windows. You then need to pass the server's peer id (which is an optional argument for @racket[universe], and logged once the server is started) to the client's @racket[big-bang] call, and a connection will be established.

@section[#:tag "how-it-works"]{How does it work?}

We use @hyperlink["https://peerjs.com/"]{PeerJS} under the hood to mimic client-server behavior where both the client and server run in browser tabs. In reality everything is done with peer connections.

@margin-note{PeerJS's @hyperlink["https://peerjs.com/peerserver"]{PeerServer Cloud Serrvice} handles all of the traffic behind the scenes so that you don't have to worry about it.}

@section[#:tag "differences"]{Differences from the 2htdp/universe API}

In practice, this library only differs from @hyperlink["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{2htdp/universe} when setting up connections (plus some slight differences in dependencies). Here's everything you need to know on top of the original docs.

@subsection{Differences for @racket[big-bang] Function}

@italic{Original @racket[big-bang] docs.}

Differences from the original @racket[big-bang] API include:
@nested[#:style 'inset]{@itemlist[
          @item{@racket[big-bang] takes and optional @italic{#:dom-root} keyword argument to specify a root element for the canvas that big-bang draws to.}
          @item{@racket[register] takes a @racket[peer-id] argument instead of an @racket[ip-expr].}
          @item{No @racket[on-pad] clause (as of now).}
          @item{No @racket[record?] clause.}
          @item{No @racket[close-on-stop] clause (yet).}
          @item{No @racket[display-mode] clause.}
          @item{No @racket[state] caluse.}
          @item{No @racket[port] clause.}]}

@defform[(register peer-id)#:contracts ([peer-id string?])]{
    Tells racket what the @racket[peer-id] of the @racket[universe] that you want your world to connect to, instead of an ip address. Because of this, racketscript-universe has no @racket[port] clauses, as they're not needed to connect via @racket[peer-id].
}

@margin-note{Because our peer connections are handled by one server in the cloud, clients can connect to servers on different networks as long as they know the server id.}

@subsection{Differences for @racket[universe] Function}

@italic{Original @racket[universe] docs.}

@nested[#:style 'inset]{@itemlist[
                                  @item{The @racket[server-id] clause can be used with @racket[universe] to specify its peer id (which gets passed into the @racket[register] clause of a @racket[big-bang] call). Note that two servers should not have the same peer id, or problems will occur.}
                                  @item{@racket[universe] takes and optional @italic{#:dom-root} keyword argument to specify a root element to insert the logging gui into.}
                                  @item{No @racket[port] clause.}
                                  @item{No @racket[state] clause (yet).}
                                  @item{No @racket[to-string] clause (yet).}
                                  @item{No @racket[check-with] clause (yet).}]}

@defform[(server-id peer-id)#:contracts ([peer-id string?])]{
    Lets you specify the @racket[peer-id] of the @racket[universe] that you're initializing. Use this @racket[peer-id] with the @racket[register] clause in a @racket[big-bang] call to connect a client.
}

