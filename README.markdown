# sproxy - secure proxy - HTTP proxy for authenticating users via Google OAuth2

## Motivation

Why use a proxy for doing OAuth? Isn't that up to the application?

 * sproxy is secure by default. No requests make it to the web server if they haven't been explicitly whitelisted.
 * sproxy is independent. Any web application written in any language can use it.

## How it Works

When an HTTP client makes a request, sproxy checks for a `gauth` cookie. If it doesn't exist (or it's invalid), it redirects the client to a Google authentication page. The user is then prompted to allow the application to access information on the user (email address). If the user proceeds, they're redirected back to sproxy with a code from Google. We then take that code and send it to Google ourselves to get back an access token. Then, we use the access token to make another call to Google, this time to their user info API to retrieve the user's email address. Finally, we store the the email address in a cookie: signed with a hash to prevent tampering, set for HTTP only (to prevent malicious JavaScript from reading it), and set it for secure (since we don't want it traveling over plaintext HTTP connections).

From that point on, when sproxy detects a valid `gauth` cookie it extracts the email, checks it against the access rules, and relays the request to the back-end server (if allowed). It also sets the `From:` header for the request so that the web application can send the user email if necessary.

## Configuration File

The configuration is read on stdin. For example:

```
sproxy < example.conf
```

See the included `example.config`

## TODO

 * Allow more than 1 redirect URL.
 * Allow more complex access rules.
