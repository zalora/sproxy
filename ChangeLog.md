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

