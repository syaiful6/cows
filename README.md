# Cows

Provides a sensible way to write websocket server on top of Cohttp-eio.

## Nix support

Since we use nixpkgs version 25.11, cohttp-eio is not available in nixpkgs. To solve this, we override the relevant packages
in our own overlay. We override `http`, `cohttp`, and `cohttp-eio` with
Eio-capable builds that track the versions `cows` needs.

When you import this flake/overlay those packages shadow the ones from nixpkgs. They are also exported as
regular packages in `flake.nix` / `release.nix`, so you can `nix build .#cohttp`
or overlay on top to change versions/tests as needed.

You can override the versions of those packages as usual, by overriding them in your own overlay.

## Development

Pull requests are welcome.

The library is tested with autobahn testsuite. To run it, use our [example](./example/echo.ml) echo
server with `dune exec cows-echo` then run autobahn in another terminal with:

```bash
docker run -it --rm -v "${PWD}/example:/config" -v "$PWD/reports:/reports" --network=host --name fuzzingclient crossbario/autobahn-testsuite:25.10.1 wstest --mode fuzzingclient --spec /config/fuzzingclient.json
```
