open ExtLib
let (>>) f g = g f

type latlon = float * float (* who cares *)

let distance (lat1, lon1) (lat2, lon2) =
  let r = 6378137. in (* radius of earth *)
  let pi = 4.0 *. atan 1.0 in
  let dlat = (lat2 -. lat1) *. pi /. 180.
  and dlon = (lon2 -. lon1) *. pi /. 180. in
  let a = (sin (dlat /. 2.) ** 2.) +.
    cos (lat1 *. pi /. 180.) *. cos (lat2 *. pi /. 180.) *.
    (sin (dlon /. 2.) ** 2.) in
  let c = 2. *. atan2 (sqrt a) (sqrt (1. -. a)) in
  r *. c

let () =
  let i = Xmlm.make_input (`Channel stdin) in
  let x1 = ref 999.
  and x2 = ref (-999.)
  and y1 = ref 999.
  and y2 = ref (-999.)
  and points = ref 0
  and tracks = ref 0
  and track_segments = ref 0
  and length = ref 0.
  and start_point = ref None in
  let rec skip i d =
    match Xmlm.input i with
    | `El_start ((_, s), _) -> skip i (d + 1)
    | `El_end -> if d = 1 then () else skip i (d - 1)
    | s -> skip i d in
  let rec pull i =
    match Xmlm.input i with
    | `El_start ((_, "gpx"), _) ->
        let rec pull_trk i =
          match Xmlm.input i with
          | `El_end -> ()
          | `El_start ((_, "trk"), _) ->
              incr tracks;
              let rec pull_trkseg i =
                match Xmlm.input i with
                | `El_end -> ()
                | `El_start ((_, "trkseg"), _) ->
                    incr track_segments;
                    let track_segment_length = ref 0. in
                    let last_point = ref None in
                    let rec pull i =
                      match Xmlm.input i with
                      | `El_end -> length := !length +. !track_segment_length
                      | `El_start ((_, "trkpt"), attrs) ->
                          incr points;
                          let point_lat = ref None in
                          let point_lon = ref None in
                          attrs >> List.iter (function
                             | ((_, "lat"), lat) ->
                                let lat = float_of_string lat in
                                point_lat := Some lat;
                                if lat < !y1 then y1 := lat;
                                if lat > !y2 then y2 := lat
                           | ((_, "lon"), lon) ->
                                let lon = float_of_string lon in
                                point_lon := Some lon;
                                if lon < !x1 then x1 := lon;
                                if lon > !x2 then x2 := lon
                            | _ -> ());
                          (match !point_lat, !point_lon with
                          | Some lat, Some lon ->
                              (match !start_point with
                              | None -> start_point := Some (lat, lon)
                              | _ -> ());
                              (match !last_point with
                              | None -> ()
                              | Some p ->
                                  track_segment_length := !track_segment_length +. distance p (lat, lon));
                              last_point := Some (lat, lon);
                          | _ (* wtf I just prased? *) -> assert false);
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
          | `El_start _ -> skip i 1; pull_trk i
          | _ -> pull_trk i in
        pull_trk i
    | `El_start _ -> skip i 1; pull i
    | _ -> pull i in
  pull i;
  if !points = 0 then (prerr_endline "no points"; exit 1);
  let lat = (!y1 +. !y2) /. 2.
  and lon = (!x1 +. !x2) /. 2. in
  Printf.printf "center point: %f %f\n" lat lon;
  (match !start_point with
  | None -> assert false
  | Some (lat, lon) ->
      Printf.printf "start point: %f %f\n" lat lon);
  Printf.printf "points: %d\n" !points;
  Printf.printf "tracks: %d\n" !tracks;
  Printf.printf "track segments: %d\n" !track_segments;
  Printf.printf "length: %f\n" !length;
  exit 0
