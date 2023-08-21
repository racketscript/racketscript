#lang scribble/manual
@(require (for-label racketscript/base
                     (except-in 2htdp/universe
                                register)
                     (only-in racketscript/htdp/peer-universe
                              server-id
                              register
                              create-login-form)))

@title{Peer-Universe for RacketScript}
@author[(author+email "Ayden Diel" "aydendiel@gmail.com")]

@defmodule[racketscript/htdp/peer-universe]
@;{
    for some reason I can't link to the 2htdp/universe docs, so I just linked to the htdp docs instead
    }
Experimental implementation of Racket's @racket[2htdp/universe] library for @seclink["top" #:doc '(lib "racketscript/scribblings/racketscript.scrbl") "RacketScript"] using peer-to-peer connections. Used to create distributed programs where both the server and the clients run in the browser.

@itemlist[@item{@secref["getting-started"]}
          @item{@secref["how-it-works"]}
          @item{@secref["differences"]}
          @item{@secref["page-setup"]}]

@section[#:tag "getting-started"]{Getting Started}
Since this library is primarily an implementation of the @hyperlink["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{2htdp/universe} API, use those docs as your main reference. These docs will contain info about how @seclink["Peer-Universe_for_RacketScript"]{peer-universe} works and how it differs from the original, but won't contain an in-depth API description.

@margin-note{I reccomend using @racket[create-login-form] to quickly and conveniently set up connections between your clients and server.}

To use the library, run a separate @racket[universe] and @racket[big-bang] instance in separate browser windows, and then pass the server's @racket{peer-id} to the client's @racket[big-bang] call, and a connection will be established.

@bold{IMPORTANT:} This library requires you use the @racket[racketscript/htdp/image] module, which implements the @seclink["image" #:doc '(lib "teachpack/teachpack.scrbl")]{htdp/image} library. Refer to the @seclink["image" #:doc '(lib "teachpack/teachpack.scrbl")]{htdp/image} docs for help using the library, but note that some features are not yet supported in the racketscript port.

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
                                  @item{The @racket[server-id] clause can be used with @racket[universe] to specify its peer id (which gets passed into the @racket[register] clause of a @racket[big-bang] call).}
                                  @item{@racket[universe] takes and optional @italic{#:dom-root} keyword argument to specify a root element to insert the logging gui into.}
                                  @item{No @racket[port] clause.}
                                  @item{No @racket[state] clause (yet).}
                                  @item{No @racket[to-string] clause (yet).}
                                  @item{No @racket[check-with] clause (yet).}]}


@margin-note{If the @racket[server-id] clause is not provided, a random id will be generated and logged.}

@defform[(server-id peer-id)#:contracts ([peer-id string?])]{
    Lets you specify the @racket[peer-id] of the @racket[universe] that you're initializing. Use this @racket[peer-id] with the @racket[register] clause in a @racket[big-bang] call to connect a client.
}

@section[#:tag "page-setup"]{Starting a Server & Logging In}

The @racket[create-login-form] function sets up some convenient boilerplate to start your app by generating this HTML form.

Here's an example.

@codeblock{
    ;; client.rkt
    #lang racketscript/base
    (require racketscript/htdp/peer-universe
             racketscript/htdp/image)
    (provide start-world)

    ;;
    ;; define all of your event handlers here
    ;;

    (define (start-world client-name server-id)
      (big-bang WORLD0
                [on-tick move]
                [to-draw draw]
                [on-receive receive]
                [register server-id]
                [name client-name]
                [on-key handle-key]
                [stop-when stop?]))
}

@codeblock{
    ;; server.rkt
    #lang racketscript/base
    (require racketscript/htdp/peer-universe)
    (provide start-universe)

    ;;
    ;; define all of your event handlers here
    ;;
    
    (define (start-universe)
      (universe '()
                [on-new        handle-new]
                [on-msg        handle-msg]
                [on-tick       handle-tick]
                [on-disconnect handle-disconnect]))
}

@codeblock{
    ;; app.rkt
    #lang racketscript/base
    (require racketscript/htdp/peer-universe
             "./client.rkt"
             "./server.rkt")

    (create-login-form start-world start-universe)
}

@linebreak{}

Here's what you'll see:

@image["create-login-form.png" #:style "border: 1px solid black;"]

The @italic{Username} and @italic{Universe's Peer ID} fields allow the user to pick their username and the @racket[peer-id] of the @racket[universe] that they want to connect to. The @italic{Join!} button calls @racket[start-world] passing in the username and @racket[peer-id], and the @italic{Start Universe} button calls @racket[start-universe]. When either button is pressed, the form is removed from the document and replaced by the UI for the @racket[universe] or @racket[big-bang] respectively.

@defform/subs[(create-login-form bb-callback
                            u-callback
                            root)
              [(bb-callback bb-callback?)
               (u-callback u-callback?)
               (root html-element?)]]{
    Generates an HTML form for your application that allows users to join an existing @racket[universe] server as a @racket[big-bang] client, or start a new @racket[universe] server. The @racket[root] parameter allows you to provide a parent element which peer-universe will insert your app's HTML into. By default, the page's body tag will be used. If you do provide an alternate root, I reccomend you use a div unless you know what you're doing.}

