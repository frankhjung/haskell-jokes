# Jokes

Read a Joke from [https://official-joke-api.appspot.com/](https://official-joke-api.appspot.com/).

This is an example program to:

- read a JSON response from a web site using
- parse JSON response using Aeson

```json
{
  "type": "general",
  "setup": "What do you call a pig with three eyes?",
  "punchline": "Piiig",
  "id": 216
}
```

## Build

See the [Makefile](Makefile) for details on how to build the project using
[Cabal](https://www.haskell.org/cabal/).

A typical build would look like this:

```bash
make
```

This will run the `default` dummy target in the Makefile which will run the
following targets: `format`, `check`, `build,` `test` and `exec`.

## Pages

- [GitLab](https://frankhjung1.gitlab.io/haskell-jokes/)
- [GitHub](https://frankhjung.github.io/haskell-jokes/)

## Resources

- [HTTP Client](https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md)
- [HTTP Response Either](https://dev.to/csaltos/haskell-http-response-either-4ncg)
- [Aeson Package](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html)
- [Aeson Tutorial](https://web.archive.org/web/20201121162141mp_/https://artyom.me/aeson)
- [A Cheatsheet to JSON Handling](https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html)
