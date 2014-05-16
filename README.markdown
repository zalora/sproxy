# sproxy - secure proxy - HTTP proxy for authenticating users via Google OAuth2

## Motivation

Why use a proxy for doing OAuth? Isn't that up to the application?

 * sproxy is secure by default. No requests make it to the web server if they haven't been explicitly whitelisted.
 * sproxy is independent. Any web application written in any language can use it.

## How it Works

When an HTTP client makes a request, sproxy checks for a `gauth` cookie. If it doesn't exist (or it's invalid), it redirects the client to a Google authentication page. The user is then prompted to allow the application to access information on the user (email address). If the user proceeds, they're redirected back to sproxy with a code from Google. We then take that code and send it to Google ourselves to get back an access token. Then, we use the access token to make another call to Google, this time to their user info API to retrieve the user's email address. Finally, we store the the email address in a cookie: signed with a hash to prevent tampering, set for HTTP only (to prevent malicious JavaScript from reading it), and set it for secure (since we don't want it traveling over plaintext HTTP connections).

From that point on, when sproxy detects a valid `gauth` cookie it extracts the email, checks it against the access rules, and relays the request to the back-end server (if allowed).

## Permissions system

Permissions are stored in a PostgreSQL database. See sproxy.sql for details.

## HTTP headers passed to the back-end server:

header            | value
----------------- | -----
`From:`           | visitor's email address
`X-Groups:`       | all groups that granted access to this resource, separated by commas
`X-Given-Name:`   | the visitor's given (first) name
`X-Family-Name:`  | the visitor's family (last) name
`X-Forwarded-For` | the visitor's IP address (added to the end of the list if header is already present in client request)

## Configuration File

By default `sproxy` will read its configuration from `config/sproxy.yml`.  You
can specify a custom path with:

```
sproxy --config /path/to/sproxy.yml
```

## Development

```
$ cp config/sproxy.yml.example config/sproxy.yml
```

Make sure that you have the follwing entry in `/etc/hosts`:

```
127.0.2.1       dev.zalora.com
```


### Create OAuth credentials

Create a project in the [Google Developers Console](https://console.developers.google.com/project).

 - visit *APIs & auth* -> *Credentials*
 - select *CREATE NEW CLIENT ID*
 - use `https://dev.zalora.com` as *Authorized JavaScript origins*
 - leave *Authorized redirect URI* to `https://dev.zalora.com/oauth2callback`

Put the `Client ID` in `config/sproxy.yml` and the `Client secret` in a file
called `config/client_secret`.

Make sure that you set an *Email address* and a  *Product name* under *APIs & auth* -> *Consent screen*!

### Create a database

In `example/privileges.sql` replace `me@zalora.com` with your actual email
address.

```
$ createdb sproxy && psql sproxy < sproxy.sql && psql sproxy < example/privileges.sql
```

Make sure that the `database` setting in `config/sproxy.yml` is suitable for
your database setup.

### Build and run

Build and run `sproxy`:

```
$ cabal build && sudo ./dist/build/sproxy/sproxy
```

Run example backend application:

```
$ runhaskell example/app.hs
```

Make a request to <https://dev.zalora.com/>.

### Troubleshooting

If you already have a cookie for `.zalora.com` sproxy gets confused and you
will repeatedly be redirected to Google's sign-in page.  Delete that cookie to
resolve this.
