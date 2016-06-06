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

