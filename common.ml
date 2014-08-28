open ExtLib
let (>>) f g = g f

let () =
  let i = Xmlm.make_input (`Channel stdin) in
  let x1 = ref 999.
  and x2 = ref (-999.)
  and y1 = ref 999.
  and y2 = ref (-999.)
  and points_count = ref 0
  and track_segments = ref 0 in
  let rec skip i d =
    match Xmlm.input i with
    | `El_start ((_, s), _) -> skip i (d + 1)
    | `El_end -> if d = 1 then () else skip i (d - 1)
    | s -> skip i d in
  let rec pull i =
    match Xmlm.input i with
    | `El_start ((_, "gpx"), _) ->
        let rec pull i =
          match Xmlm.input i with
          | `El_end -> ()
          | `El_start ((_, "trk"), _) ->
              let rec pull_trkseg i =
                match Xmlm.input i with
                | `El_end -> ()
                | `El_start ((_, "trkseg"), _) ->
                    incr track_segments;
                    let rec pull i =
                      match Xmlm.input i with
                      | `El_end -> ()
                      | `El_start ((_, "trkpt"), attrs) ->
                          incr points_count;
                          attrs >> List.iter (function
                            | ((_, "lon"), lon) ->
                                let lon = float_of_string lon in
                                if lon < !x1 then x1 := lon;
                                if lon > !x2 then x2 := lon
                            | ((_, "lat"), lat) ->
                                let lat = float_of_string lat in
                                if lat < !y1 then y1 := lat;
                                if lat > !y2 then y2 := lat
                            | _ -> ());
                          (match Xmlm.input i with
                          | `El_end -> pull i
                          | `El_start _ -> skip i 2; pull i
                          | _ -> skip i 1; pull i)
                      | `El_start _ -> skip i 1; pull i
                      | _ -> pull i in
                    pull i; pull_trkseg i
                | `El_start _ -> skip i 1; pull_trkseg i
                | _ -> pull_trkseg i in
              pull_trkseg i
          | `El_start _ -> skip i 1; pull i
          | _ -> pull i in
        pull i
    | `El_start _ -> skip i 1; pull i
    | _ -> pull i in
  pull i;
  if !points_count = 0 then (prerr_endline "no points"; exit 1);
  let lon = (!x1 +. !x2) /. 2.
  and lat = (!y1 +. !y2) /. 2. in
  Printf.printf "center point: %f %f\n" lon lat;
  Printf.printf "points count: %d\n" !points_count;
  Printf.printf "track segments: %d\n" !track_segments;
  exit 0
