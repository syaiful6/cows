# Cows

Provides a sensible way to write websocket server on top of Cohttp-eio.

## Development

Pull requests are welcome.

The library is tested with autobahn testsuite. To run it, use our [example](./example/echo.ml) echo
server with `dune exec cows-echo` then run autobahn in another terminal with:

```bash
docker run -it --rm -v "${PWD}/example:/config" -v "$PWD/reports:/reports" --network=host --name fuzzingclient crossbario/autobahn-testsuite:25.10.1 wstest --mode fuzzingclient --spec /config/fuzzingclient.json
```
