# Sproxy - HTTP proxy for authenticating users via OAuth2

## Motivation

Why use a proxy for doing OAuth? Isn't that up to the application?

 * sproxy is secure by default.  No requests make it to the web server if they
   haven't been explicitly whitelisted.
 * sproxy is independent.  Any web application written in any language can use
   it.

## How it Works

When an HTTP client makes a request, sproxy checks for a *session cookie*.  If it
doesn't exist (or it's invalid), it redirects the client to a Google
authentication page.  The user is then prompted to allow the application to
access information on the user (email address).  If the user proceeds, they're
redirected back to sproxy with a code from Google.  We then take that code and
send it to Google ourselves to get back an access token.  Then, we use the
access token to make another call to Google, this time to their user info API
to retrieve the user's email address.  Finally, we store the the email address
in a session cookie: signed with a hash to prevent tampering, set for HTTP only (to
prevent malicious JavaScript from reading it), and set it for secure (since we
don't want it traveling over plaintext HTTP connections).

From that point on, when sproxy detects a valid session cookie it extracts the
email, checks it against the access rules, and relays the request to the
back-end server (if allowed).

## Logout

Hitting the endpoint `/sproxy/logout` will invalidate the session
cookie.  The user will be redirected to `/` after logout.  The
query parameter `state` can be provided to specify an alternate redirect path
(the path has to be percent-encoded, e.g. with `urlEncode` from
`Network.HTTP.Types.URI`).

## Robots

Since all sproxied resources are private, it doesn't make sense for web crawlers
to try to index them. In fact, crawlers will index only the Google authentication
page. To prevent this, sproxy returns the following for `/robots.txt`:

```
User-agent: *
Disallow: /
```

## Permissions system

Permissions are stored in a PostgreSQL database. See sproxy.sql for details.
Here are the main concepts:

- A `group` is identified by a name. Every group has
  - members (identified by email address, through `group_member`) and
  - associated privileges (through `group_privilege`).
- A `privilege` is identified by a name _and_ a domain. It has associated rules
  (through `privilege_rule`) that define what the privilege gives access to.
- A `rule` is a combination of sql patterns for a `domain`, a `path` and an
  HTTP `method`. A rule matches an HTTP request, if all of these components
  match the respective attributes of the request. However of all the matching
  rules only the rule with the longest `path` pattern will be used to determine
  whether a user is allowed to perform a request. This is often a bit
  surprising, please see the following example:

### Privileges example

Consider this `group_privilege` and `privilege_rule` relations:

group            | privilege | domain
---------------- | --------- | -----------------
`readers`        | `basic`   | `wiki.zalora.com`
`readers`        | `read`    | `wiki.zalora.com`
`editors`        | `basic`   | `wiki.zalora.com`
`editors`        | `read`    | `wiki.zalora.com`
`editors`        | `edit`    | `wiki.zalora.com`
`administrators` | `basic`   | `wiki.zalora.com`
`administrators` | `read`    | `wiki.zalora.com`
`administrators` | `edit`    | `wiki.zalora.com`
`administrators` | `admin`   | `wiki.zalora.com`

privilege   | domain            | path           | method
----------- | ----------------- | -------------- | ------
`basic`     | `wiki.zalora.com` | `/%`           | `GET`
`read`      | `wiki.zalora.com` | `/wiki/%`      | `GET`
`edit`      | `wiki.zalora.com` | `/wiki/edit/%` | `%`
`admin`     | `wiki.zalora.com` | `/admin/%`     | `%`

With this setup, everybody (that is `readers`, `editors` and `administrators`s)
will have access to e.g. `/imgs/logo.png` and `/favicon.ico`, but only
administrators will have access to `/admin/index.php`, because the longest
matching path pattern is `/admin/%` and only `administrator`s have the `admin`
privilege.

Likewise `readers` have no access to e.g. `/wiki/edit/delete_everything.php`.


## HTTP headers passed to the back-end server:

header               | value
-------------------- | -----
`From:`              | visitor's email address
`X-Groups:`          | all groups that granted access to this resource, separated by commas (see the note below)
`X-Given-Name:`      | the visitor's given (first) name
`X-Family-Name:`     | the visitor's family (last) name
`X-Forwarded-Proto:` | the visitor's protocol of an HTTP request, always `https`
`X-Forwarded-For`    | the visitor's IP address (added to the end of the list if header is already present in client request)


`X-Groups` denotes an intersection of the groups the visitor belongs to and the groups that granted access:

Visitor's groups | Granted groups | `X-Groups`
---------------- | -------------- | ---------
all              | all, devops    | all
all, devops      | all            | all
all, devops      | all, devops    | all,devops
all, devops      | devops         | devops
devops           | all, devops    | devops
devops           | all            | Access denied

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

Make sure that you have the following entry in `/etc/hosts`:

```
127.0.0.1       dev.zalora.com
```


### Create OAuth credentials

Create a project in the [Google Developers Console](https://console.developers.google.com/project).

 - visit *APIs & auth* -> *Credentials*
 - select *CREATE NEW CLIENT ID*
 - use `https://dev.zalora.com` as *Authorized JavaScript origins*
 - use `https://dev.zalora.com/sproxy/oauth2callback` as *Authorized redirect URI*

Put the `Client ID` in `config/sproxy.yml` and the `Client secret` in a file
called `config/client_secret`.

