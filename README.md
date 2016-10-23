# Sproxy - HTTP proxy for authenticating users via OAuth2

## Motivation

Why use a proxy for doing OAuth? Isn't that up to the application?

 * sproxy is secure by default.  No requests make it to the web server if they
   haven't been explicitly whitelisted.
 * sproxy is independent.  Any web application written in any language can use
   it.

## Use cases

 * Existing web applications with concept of roles. For example,
   [Mediawiki](https://www.mediawiki.org), [Jenkins](https://jenkins.io),
   [Icinga Web 2](https://www.icinga.org/products/icinga-web-2/). In
   this case you configure Sproxy to allow unrestricted access
   to the application for some groups defined by Sproxy. These
   groups are mapped to the application roles.  There is a [plugin for
   Jenkins](https://wiki.jenkins-ci.org/display/JENKINS/Reverse+Proxy+Auth+Plugin)
   which can be used for this. Mediawiki and Icinga Web 2 were also
   successfully deployed in this way, though it required changes to their
   source code.

 * New web applications designed to work specifically behind Sproxy. In this case
   you define Sproxy rules to control access to the
   application's API.  It would likely be [a single-page
   application](https://en.wikipedia.org/wiki/Single-page_application).
   Examples are [MyWatch](https://hackage.haskell.org/package/mywatch) and
   [Juan de la Cosa](https://hackage.haskell.org/package/juandelacosa)

## How it works

When an HTTP client makes a request, Sproxy checks for a *session cookie*.
If it doesn't exist (or it's invalid, expired), it responses with [HTTP
status 511](https://tools.ietf.org/html/rfc6585) with the page, where the
user can choose an [OAuth2](https://tools.ietf.org/html/rfc6749) provider to
authenticate with.  Finally, we store the the email address in a session
cookie: signed with a hash to prevent tampering, set for HTTP only (to prevent
malicious JavaScript from reading it), and set it for secure (since we don't
want it traveling over plaintext HTTP connections).

From that point on, when sproxy detects a valid session cookie it extracts the
email, checks it against the access rules, and relays the request to the
back-end server (if allowed).


## Logout

Hitting the endpoint `/sproxy/logout` will invalidate the session cookie.
The user will be redirected to `/` after logout.  The query parameter `state`
can be provided to specify an alternate URL-encoded redirect path


## Robots

Since all sproxied resources are private, it doesn't make sense for web
crawlers to try to index them. In fact, crawlers will index only the login
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
`readers`        | `basic`   | `wiki.example.com`
`readers`        | `read`    | `wiki.example.com`
`editors`        | `basic`   | `wiki.example.com`
`editors`        | `read`    | `wiki.example.com`
`editors`        | `edit`    | `wiki.example.com`
`administrators` | `basic`   | `wiki.example.com`
`administrators` | `read`    | `wiki.example.com`
`administrators` | `edit`    | `wiki.example.com`
`administrators` | `admin`   | `wiki.example.com`

privilege   | domain            | path           | method
----------- | ----------------- | -------------- | ------
`basic`     | `wiki.example.com` | `/%`           | `GET`
`read`      | `wiki.example.com` | `/wiki/%`      | `GET`
`edit`      | `wiki.example.com` | `/wiki/edit/%` | `%`
`admin`     | `wiki.example.com` | `/admin/%`     | `%`

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


## Configuration file

By default `sproxy` will read its configuration from
`config/sproxy.yml`.  There is example file with documentation
[config/sproxy.yml.example](config/sproxy.yml.example). You can specify a
custom path with:

```
sproxy --config /path/to/sproxy.yml
```

