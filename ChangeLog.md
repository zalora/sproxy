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

