0.9.9
=====

* This is the last release of this Sproxy. See [Sproxy2](http://hackage.haskell.org/package/sproxy2).
* Google: prompt for account only, don't ask for offline access.


0.9.8
=====

* If the user is not authenticated, show login page with [HTTP status code
  511](https://tools.ietf.org/html/rfc6585), instead of 302 -> 200.  It had
  bad UX for AJAX calls.
* Always convert authenticated user's email to lowercase. This affects the
  cookie and the `From` header.
* Stop using the `string-conversions` package.
* Print authentication code in debug mode only.


0.9.7.1
=======

* Fixed cabal source distribution


0.9.7
=====

* Added support for [LinkedIn OAuth2 API](https://developer.linkedin.com/docs/oauth2).
  Added new options `linkedin_client_id` and `linkedin_client_secret`. They are optional
  as well as Google's `client_id` and `client_secret`. The user is now redirected to the
  `sproxy/login` page to choose an OAuth2 provider.


0.9.6
=====

* Session shelf life is configurable with the `session_shelf_life` option
  in configuration file. Defaults to 30 days. It was hard-coded before.
* Dropped dependency on `logsink` / `logging-facade` (fail to build).
  Log to stderr only, log levels: error, warning, info, debug.
  The `log_target` option is ignored.


0.9.5
=====

* Allow running as unprivileged user: added option `user` in the configuration file.
* Default log level is `debug` if omitted in the configuration file.


0.9.4
=====

* Combine the multiple header fields into one "field-name: field-value" pair
  with a comma-separated list for the field-value (Instead of removing duplicates).
* `sproxy [ -h | --help ]` works (using [docopt](https://hackage.haskell.org/package/docopt))
* Stop counting client parsing failures as errors.


0.9.3
=====

* Made some options in configuration file optional with reasonable default values:
  - `log_target`: `stderr`
  - `listen`: `443`
  - `redirect_http_to_https`: yes if `listen == 443`
  - `backend_address`: `"127.0.0.1"`
  - `backend_port`: `8080`
* Allow backend at UNIX socket: new option `backend_socket`.
* Removed tests (unsupported).
* Don't build Sproxy library.


0.9.2
=====

* Deny SSLv3.
* Removed the `auth_token_key` option from the config file.
  The token is generated randomly on startup.
	Restarting sproxy invalidates existing sessions.
* Added `ChangeLog.md`

