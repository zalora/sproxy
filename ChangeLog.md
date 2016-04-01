0.9.2

- Deny SSLv3.

- Removed the `auth_token_key` option from the config file.
  The token is generated randomly on startup.
	Restarting sproxy invalidates existing sessions.

- Added ChangeLog

