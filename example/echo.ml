let rec handler conn =
  match Cows.recv conn with
  | `Text msg ->
    Cows.send conn (`Text msg);
    handler conn
  | `Binary msg ->
    Cows.send conn (`Binary msg);
    handler conn
  | `Ping payload ->
    Cows.send conn (`Pong payload);
    handler conn
  | `Pong _ -> handler conn
  | `Close (code, reason) -> Cows.close ~code ~reason conn

let () =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9001) in
  let socket = Eio.Net.listen ~reuse_addr:true ~backlog:256 ~sw env#net addr in
  Logs.info (fun f -> f "Listening on ws://127.0.0.1:9001\n%!");
  let server =
    Cohttp_eio.Server.make_response_action
      ~callback:(fun _conn req _body -> Cows.upgrade req handler)
      ()
  in
  Cohttp_eio.Server.run
    ~on_error:(fun exn ->
      Logs.warn (fun f -> f "Uncaught exception %s" (Printexc.to_string exn)))
    socket
    server
