# sproxy - secure proxy - HTTP proxy for authenticating users via Google OAuth2

## Motivation

Why use a proxy for doing OAuth? Isn't that up to the application?

 * sproxy is secure by default. No requests make it to the web server if they haven't been explicitly whitelisted.
 * sproxy is independent. Any web application written in any language can use it.

## How it Works

When an HTTP client makes a request, sproxy checks for a `gauth` cookie. If it doesn't exist (or it's invalid), it redirects the client to a Google authentication page. The user is then prompted to allow the application to access information on the user (email address). If the user proceeds, they're redirected back to sproxy with a code from Google. We then take that code and send it to Google ourselves to get back an access token. Then, we use the access token to make another call to Google, this time to their user info API to retrieve the user's email address. Finally, we store the the email address in a cookie: signed with a hash to prevent tampering, set for HTTP only (to prevent malicious JavaScript from reading it), and set it for secure (since we don't want it traveling over plaintext HTTP connections).

From that point on, when sproxy detects a valid `gauth` cookie it extracts the email, checks it against the access rules, and relays the request to the back-end server (if allowed).

## Permissions system

The current state of the permissions system is as follows:

All permissions (and groups and users) are configured in the sproxy config file. There are three entity types:

- **user** - A user has an email address and belongs to multiple **groups**.
- **group** - A group has a name and allows access to multiple so-called **url-patterns**.
- **url-pattern** - A url-pattern has a name and a domain (e.g. `csqa.ds.zalora.com`).

(Here's an example configuration file: https://bitbucket.org/zalorasea/sproxy/src/HEAD/example.config?at=master .)

A user is allowed access to a url when they are in at least one group that contains at least one url-pattern that is identical
to the domain of the url.

### Roadmap for the Permissions System

The permissions system is not where we want it to be yet. Ideas for improvement:

- Put the configuration for users, groups, etc. in a database.
- Allow more complex url-patterns. One possible design: Allow url-patterns to be
  a domain + path (e.g. `csqa.ds.zalora.com/deleteEverything`). Deciding whether a user
  has access to a url would work like this:
  
    - Collect **all** configured url-patterns (regardless of whether the current user 
      has access to them or not).
    - Find the longest url pattern, that is a prefix of the url.
    - If the user is configured to have access to that url-pattern, let him through. Otherwise not.

  This would make it possible to disallow users access to certain subpaths of web services.

- Include the request's HTTP method (GET, POST, etc.) in the url-pattern.

## HTTP headers passed to the back-end server:

header    | value
--------- | -----
`From:`   | visitor's email address
`Groups:` | all sproxy groups, seperated by spaces

## Configuration File

```
sproxy --config example.conf
```

See the included `example.config`
