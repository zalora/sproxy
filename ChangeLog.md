For differences with the original Sproxy scroll down.

1.91.0
======

  * In addition to good old PostgreSQL data source, made it possible
    to import permission data from a YAML file. This means that Sproxy2
    can work without any PostgreSQL database, just using file-only configuration.
    Useful for development or trivial deployments. Added new `datafile` option
    in configuration file.


1.90.2
======

  * Make sure all Sproxy-specific HTTP headers are UTF8-encoded.

  * `/.sproxy/logout` just redirects if no cookie. Previously
    it was returning HTTP 404 to unauthenticated users, and redirecting
    authenticated users with removal of the cookie. The point is not to
    reveal cookie name.

  * Made Warp stop printing exceptions, mostly "client closed connection",
    which happens outside of our traps.


1.90.1
======

  * Fixed headers processing. Wrong headers were making Chromium drop connection in HTTP/2.
    Firefox sometimes couldn't handle gzipped and chunked responses in HTTP/1.1.

  * After authenticating, redirect to original path with query parameters if
    method was GET.  Otherwise redirect to "/". Previously, when unauthenticated
    users click on "https://example.net/foo?bar", they are redirected to
    "https://example.net/foo" regardless of the method.



1.90.0 (Preview Release)
========================

Sproxy2 is overhaul of original [Sproxy](https://github.com/zalora/sproxy)
(see also [Hackage](https://hackage.haskell.org/package/sproxy)).
Here are the key differences (with Sproxy 0.9.8):

  * Sproxy2 can work with remote PostgreSQL database. Quick access to the database is essential
    as sproxy does it on every HTTP request. Sproxy2 pulls data into local SQLite3 database.

  * At this release Sproxy2 is compatible with Sproxy database with one exception:
    SQL wildcards are not supported for HTTP methods. E. i. you have to change '%' in
    the database to specific methods like GET, POST, etc.

  * OAuth2 callback URLs changed: Sproxy2 uses `/.sproxy/oauth2/:provider`,
    e. g. `/.sproxy/oauth2/google`. Sproxy used `/sproxy/oauth2callback` for Google
    and `/sproxy/oauth2callback/linkedin` for LinkedIn.

  * Sproxy2 does not allow login with email addresses not known to it.

  * Sproxy2: OAuth2 callback state is serialized, signed and passed base64-encoded.
    Of course it's used to verify the request is legit.

  * Sproxy2: session cookie is serialized, signed and sent base64-encoded.

  * Path `/.sproxy` belongs to Sproxy2 completely. Anything under this path is never passed to backends.

  * Sproxy2 supports multiple backends. Routing is based on the Host HTTP header.

  * Sproxy2 uses [WAI](https://hackage.haskell.org/package/wai) / [Warp](https://hackage.haskell.org/package/warp)
    for incoming connections. As a result Sproxy2 supports HTTP2.

  * Sproxy2 uses [HTTP Client](https://hackage.haskell.org/package/http-client) to talk to backends.
    As a result Sproxy2 reuses backend connections instead of closing them after each request to the backend.

  * Sproxy2 optionally supports persistent key again (removed in Sproxy 0.9.2).
    This can be used in load-balancing multiple Sproxy2 instances.

  * Configuration file has changed. It's still YAML, but some options are renamed, removed or added.
    Have a look at well-documented [sproxy.yml.example](./sproxy.yml.example)

