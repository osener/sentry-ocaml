open Core_kernel
open Util

type t =
  { event_id : Uuidm.t sexp_opaque
  ; timestamp : Time.t sexp_opaque
  ; logger : string sexp_option
  ; platform : Platform.t
  ; sdk : Sdk.t
  ; level : Severity_level.t sexp_option
  ; culprit : string sexp_option
  ; server_name : string sexp_option
  ; release : string sexp_option
  ; tags : string String.Map.t
  ; environment : string sexp_option
  ; modules : string String.Map.t
  ; extra : Json.t String.Map.t
  ; fingerprint : string list sexp_option
  ; exception_ : Exception.t list option sexp_opaque
  ; message : Message.t sexp_option
  ; breadcrumbs : Breadcrumb.t list }
[@@deriving sexp_of]

let make ?event_id ?timestamp ?context ?tags ?logger
      ?(platform=`Other) ?(sdk=Sdk.default) ?level ?culprit ?fingerprint
      ?message ?exn () =
  let { Context.server_name ; release ; environment ; extra
      ; tags = context_tags ; breadcrumbs ; _ } =
    match context with
    | Some context -> context
    | None -> Context.empty ()
  in
  let tags =
    begin match tags with
    | None
    | Some [] -> context_tags
    | Some tags ->
      let merged = Hashtbl.copy context_tags in
      List.iter tags ~f:(fun (key, data) ->
        Hashtbl.set merged ~key ~data);
      merged
    end
    |> Hashtbl.to_alist
    |> String.Map.of_alist_exn
  in
  let event_id =
    match event_id with
    | Some id -> id
    | None -> Uuidm.create `V4
  in
  let timestamp =
    match timestamp with
    | Some ts -> ts
    | None -> Time.now ()
  in
  { event_id ; timestamp ; logger ; platform ; sdk ; level ; culprit
  ; server_name ; release ; tags ; environment ; modules = String.Map.empty
  ; extra = String.Table.to_alist extra |> String.Map.of_alist_exn
  ; fingerprint ; message ; exception_ = exn
  ; breadcrumbs = Queue.to_list breadcrumbs }

let to_payload { event_id ; timestamp ; logger ; platform ; sdk ; level
               ; culprit ; server_name ; release ; tags ; environment ; modules
               ; extra ; fingerprint ; exception_ ; message ; breadcrumbs } =
  { Payloads_t.event_id ; timestamp ; logger ; platform
  ; sdk = Sdk.to_payload sdk
  ; level ; culprit ; server_name ; release
  ; tags = map_to_alist_option tags
  ; environment
  ; modules = map_to_alist_option modules
  ; extra = map_to_alist_option extra
  ; fingerprint
  ; exception_ = Option.map ~f:Exception.list_to_payload exception_
  ; message = Option.map ~f:Message.to_payload message
  ; breadcrumbs = (match breadcrumbs with
      | [] -> None
      | _ ->
        List.map breadcrumbs ~f:Breadcrumb.to_payload
        |> Util.empty_list_option) }

let to_json_string t =
  to_payload t
  |> Payloads_j.string_of_event

let%expect_test "to_json_string basic" =
  let event_id = Uuid.wrap "bce345569e7548a384bac4512a9ad909" in
  let timestamp = Time.of_string "2018-08-03T11:44:21.298019Z" in
  make ~event_id ~timestamp ()
  |> to_json_string
  |> print_endline;
  [%expect {| {"event_id":"bce345569e7548a384bac4512a9ad909","timestamp":"2018-08-03T11:44:21.298019","platform":"other","sdk":{"name":"sentry-ocaml","version":"0.1"}} |}]

