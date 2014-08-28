open ExtLib
open Xml
let (>>) f g = g f

let () =
  let xml = Xml.parse_in stdin in
  match xml with
  | Element ("gpx", _, xmls) -> (
      let touched = ref false in
      let x1 = ref 0.
      and x2 = ref 0.
      and y1 = ref 0.
      and y2 = ref 0. in
      xmls >> List.iter (function
        | Element ("trk", _, xmls) ->
            xmls >> List.iter (function
              | Element ("trkseg", _, xmls) ->
                  xmls >> List.iter (function
                    | Element ("trkpt", attrs, _) ->
                        attrs >> List.iter (function
                          | ("lon", lon) ->
                              touched := true;
                              let lon = float_of_string lon in
                              if lon < !x1 then x1 := lon;
                              if lon > !x2 then x2 := lon
                          | ("lat", lat) ->
                              touched := true;
                              let lat = float_of_string lat in
                              if lat < !y1 then y1 := lat;
                              if lat > !y2 then y2 := lat
                          | _ -> ())
                    | _ -> ())
              | _ -> ())
        | _ -> ());
      let lon = (!x1 +. !x2) /. 2.
      and lat = (!y1 +. !y2) /. 2. in
      Printf.printf "Center point: %f %f\n" lon lat
  )
  | _ -> assert false
