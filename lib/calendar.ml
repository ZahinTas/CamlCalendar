(** Calendar module providing data types and operations for days, todos, events,
    user authentication, file I/O, invitations, and calendar utilities such as
    date computations and formatting. *)

open Unix
open Sys
open Yojson.Basic

type todo = {
  id : int;
  name : string;
  description : string;
  completed : bool;
}

type event = {
  id : int;
  name : string;
  start_time : string;
  end_time : string;
  location : string;
  invited_people : string list;
  email_addresses : string list;
  picture_path : string option;
}

type day_data = {
  date : string;
  special_day_name : string option;
  todos : todo list;
  events : event list;
}

(** [todo_to_json t] converts todo [t] to JSON format. *)
let todo_to_json (t : todo) =
  `Assoc
    [
      ("id", `Int t.id);
      ("name", `String t.name);
      ("description", `String t.description);
      ("completed", `Bool t.completed);
    ]

(** [todo_of_json json] parses JSON to a todo, raising on invalid format. *)
let todo_of_json json =
  let open Yojson.Basic.Util in
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    completed = json |> member "completed" |> to_bool;
  }

(** [event_to_json e] is the JSON representation of the event [e], encoding all
    its fields including times, location, invited people, email addresses, and
    optional picture path. *)
let event_to_json e =
  `Assoc
    [
      ("id", `Int e.id);
      ("name", `String e.name);
      ("startTime", `String e.start_time);
      ("endTime", `String e.end_time);
      ("location", `String e.location);
      ("invitedPeople", `List (List.map (fun p -> `String p) e.invited_people));
      ("emailAddresses", `List (List.map (fun p -> `String p) e.email_addresses));
      ( "picturePath",
        match e.picture_path with
        | Some p -> `String p
        | None -> `Null );
    ]

(** [event_of_json json] parses the JSON object [json] into an [event] record,
    extracting all fields including optional picture path. *)
let event_of_json json =
  let open Yojson.Basic.Util in
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    start_time = json |> member "startTime" |> to_string;
    end_time = json |> member "endTime" |> to_string;
    location = json |> member "location" |> to_string;
    invited_people =
      json |> member "invitedPeople" |> to_list |> List.map to_string;
    email_addresses =
      json |> member "emailAddresses" |> to_list |> List.map to_string;
    picture_path =
      (match json |> member "picturePath" with
      | `String p -> Some p
      | _ -> None);
  }

let send_invitations ~recipient ~subject ~body ~attachment =
  let command =
    match attachment with
    | Some path ->
        Printf.sprintf "echo \"%s\" | mutt -s \"%s\" -a \"%s\" -- %s" body
          subject path recipient
    | None ->
        Printf.sprintf "echo \"%s\" | mutt -s \"%s\" -- %s" body subject
          recipient
  in
  Sys.command command

let send_event_invitations event =
  match event.email_addresses with
  | [] -> []
  | emails ->
      let body =
        Printf.sprintf
          "You are invited to: %s\n\
           Time: %s - %s\n\
           Location: %s\n\n\
           Please RSVP.\n\n\
           Best regards,\n\
           CamlCalendar"
          event.name event.start_time event.end_time event.location
      in

      let subject = Printf.sprintf "Invitation: %s" event.name in

      List.map
        (fun email ->
          let exit_code =
            send_invitations ~recipient:email ~subject ~body
              ~attachment:event.picture_path
          in
          (email, exit_code = 0))
        emails

let day_to_json day =
  let special_field =
    match day.special_day_name with
    | Some s -> [ ("specialDayName", `String s) ]
    | None -> []
  in
  `Assoc
    ([ ("date", `String day.date) ]
    @ special_field
    @ [
        ("todos", `List (List.map todo_to_json day.todos));
        ("events", `List (List.map event_to_json day.events));
      ])

let day_of_json json =
  let open Yojson.Basic.Util in
  let special_name =
    try Some (json |> member "specialDayName" |> to_string) with _ -> None
  in
  {
    date = json |> member "date" |> to_string;
    special_day_name = special_name;
    todos = json |> member "todos" |> to_list |> List.map todo_of_json;
    events = json |> member "events" |> to_list |> List.map event_of_json;
  }

let data_folder = "data"
let user_db_path = "user/users.json"

let ensure_data_folder () =
  if not (Sys.file_exists data_folder) then Unix.mkdir data_folder 0o755;
  if not (Sys.file_exists "user") then Unix.mkdir "user" 0o755

let save_day_to_file ~username filename day =
  let user_folder = Filename.concat data_folder username in
  if not (Sys.file_exists user_folder) then Unix.mkdir user_folder 0o755;
  let path = Filename.concat user_folder filename in
  Yojson.Basic.to_file path (day_to_json day)

let load_day_from_file ~username filename =
  let user_folder = Filename.concat data_folder username in
  let path = Filename.concat user_folder filename in
  if Sys.file_exists path then day_of_json (Yojson.Basic.from_file path)
  else { date = filename; special_day_name = None; todos = []; events = [] }

let load_users () =
  if not (Sys.file_exists user_db_path) then []
  else
    match Yojson.Basic.from_file user_db_path with
    | `Assoc lst ->
        List.map
          (fun (u, v) ->
            match v with
            | `String pw -> (u, pw)
            | _ -> failwith "Invalid format")
          lst
    | _ -> failwith "Invalid format"

(** [save_users users] saves the list of username-password pairs [users] to the
    user database file. *)
let save_users users =
  let json = `Assoc (List.map (fun (u, pw) -> (u, `String pw)) users) in
  Yojson.Basic.to_file user_db_path json

let signup ~username ~password =
  let users = load_users () in
  if List.mem_assoc username users then Error "Username exists!"
  else (
    save_users ((username, password) :: users);
    Ok ())

let login ~username ~password =
  match List.assoc_opt username (load_users ()) with
  | Some pw when pw = password -> Ok ()
  | Some _ -> Error "Wrong password"
  | None -> Error "Username not found"

let setup_signal_handlers () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         (* This is minimal printing that's acceptable in a library for critical
            signals *)
         print_endline "\nSIGINT received. Exiting safely.";
         exit 130))

let next_id items get_id =
  match items with
  | [] -> 1
  | _ -> List.fold_left (fun acc x -> max acc (get_id x)) 0 items + 1

(** [toggle_todo day todo_id] toggles completion status of todo with [todo_id]
    in [day]. *)
let toggle_todo day todo_id =
  let updated_todos =
    List.map
      (fun (t : todo) ->
        if t.id = todo_id then { t with completed = not t.completed } else t)
      day.todos
  in
  { day with todos = updated_todos }

let delete_todo day todo_id =
  let updated_todos =
    List.filter (fun (t : todo) -> t.id <> todo_id) day.todos
  in
  { day with todos = updated_todos }

let add_todo day name description =
  let new_id = next_id day.todos (fun t -> t.id) in
  let new_todo = { id = new_id; name; description; completed = false } in
  { day with todos = day.todos @ [ new_todo ] }

let delete_event day event_id =
  let updated_events =
    List.filter (fun (e : event) -> e.id <> event_id) day.events
  in
  { day with events = updated_events }

let time_to_minutes t =
  match String.split_on_char ':' t with
  | [ hour; min ] -> (int_of_string hour * 60) + int_of_string min
  | _ -> failwith "Invalid time format. Use HH:MM"

let sort_events_by_time events =
  List.sort
    (fun e1 e2 ->
      let start_diff =
        compare (time_to_minutes e1.start_time) (time_to_minutes e2.start_time)
      in
      if start_diff <> 0 then start_diff
      else compare (time_to_minutes e1.end_time) (time_to_minutes e2.end_time))
    events

let add_event day event_data =
  let name, start_time, end_time, location, invited, emails, picture_path =
    event_data
  in
  let new_event =
    {
      id = next_id day.events (fun e -> e.id);
      name;
      start_time;
      end_time;
      location;
      invited_people = invited;
      email_addresses = emails;
      picture_path;
    }
  in
  { day with events = sort_events_by_time (day.events @ [ new_event ]) }

let update_event day event_id ~name ~start_time ~end_time ~location =
  let updated_events =
    List.map
      (fun e ->
        if e.id = event_id then { e with name; start_time; end_time; location }
        else e)
      day.events
  in
  { day with events = sort_events_by_time updated_events }

let month_name m =
  match m with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> "Unknown"

(** [holidays] is a list of (month, day) tuples representing fixed holidays. *)
let holidays =
  [
    (1, 1);
    (1, 15);
    (2, 19);
    (5, 28);
    (6, 19);
    (7, 4);
    (9, 3);
    (10, 8);
    (11, 4);
    (11, 11);
    (11, 27);
    (11, 28);
    (12, 25);
  ]

let is_holiday month day =
  List.exists (fun (m, d) -> m = month && d = day) holidays

let days_in_month year month =
  let tm =
    {
      Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = 1;
      tm_mon = month - 1;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  let next_month, next_year =
    if month = 12 then (1, year + 1) else (month + 1, year)
  in
  let next_tm =
    { tm with tm_mon = next_month - 1; tm_year = next_year - 1900 }
  in
  (Unix.localtime (fst (Unix.mktime next_tm) -. 86400.0)).tm_mday

let first_weekday year month =
  let first_tm =
    {
      Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = 1;
      tm_mon = month - 1;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  match (Unix.localtime (fst (Unix.mktime first_tm))).tm_wday with
  | 0 -> 7
  | n -> n

let get_today () =
  let now = Unix.localtime (Unix.time ()) in
  (now.tm_year + 1900, now.tm_mon + 1, now.tm_mday)

let is_past_day year month day =
  let curr_year, curr_month, curr_day = get_today () in
  (year, month, day) < (curr_year, curr_month, curr_day)

let is_today year month day =
  let curr_year, curr_month, curr_day = get_today () in
  year = curr_year && month = curr_month && day = curr_day

let set_special_day_name day name = { day with special_day_name = Some name }

let get_todo_by_id day todo_id =
  List.find_opt (fun (t : todo) -> t.id = todo_id) day.todos

let get_event_by_id day event_id =
  List.find_opt (fun e -> e.id = event_id) day.events

let get_week_range year month day =
  let tm =
    {
      Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = day;
      tm_mon = month - 1;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  let weekday = (Unix.localtime (fst (Unix.mktime tm))).tm_wday in
  let weekday = if weekday = 0 then 7 else weekday in

  let monday_day = day - (weekday - 1) in
  let monday_tm = { tm with tm_mday = monday_day } in
  let monday_time = fst (Unix.mktime monday_tm) in
  let monday = Unix.localtime monday_time in

  let sunday_time = monday_time +. (6. *. 86400.) in
  let sunday = Unix.localtime sunday_time in

  let start_date =
    Printf.sprintf "%04d-%02d-%02d" (monday.tm_year + 1900) (monday.tm_mon + 1)
      monday.tm_mday
  in
  let end_date =
    Printf.sprintf "%04d-%02d-%02d" (sunday.tm_year + 1900) (sunday.tm_mon + 1)
      sunday.tm_mday
  in
  (start_date, end_date)

let get_week_number year month day =
  let tm =
    {
      Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = day;
      tm_mon = month - 1;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in

  let week_num = ((Unix.localtime (fst (Unix.mktime tm))).tm_yday / 7) + 1 in
  week_num

let get_week_dates () =
  let current_year, current_month, current_day = get_today () in
  let start_date, _ = get_week_range current_year current_month current_day in

  let year_start = int_of_string (String.sub start_date 0 4) in
  let month_start = int_of_string (String.sub start_date 5 2) in
  let day_start = int_of_string (String.sub start_date 8 2) in

  let rec generate_days acc year month day count =
    if count >= 7 then List.rev acc
    else
      let days_in_current_month = days_in_month year month in
      if day > days_in_current_month then
        let next_month, next_year =
          if month = 12 then (1, year + 1) else (month + 1, year)
        in
        generate_days
          ((year, month, day) :: acc)
          next_year next_month 1 (count + 1)
      else
        generate_days
          ((year, month, day) :: acc)
          year month (day + 1) (count + 1)
  in
  generate_days [] year_start month_start day_start 0

let get_month_range start_year start_month count =
  let rec generate_months acc year month remaining =
    if remaining <= 0 then List.rev acc
    else
      let next_acc = (year, month) :: acc in
      let next_month, next_year =
        if month = 12 then (1, year + 1) else (month + 1, year)
      in
      generate_months next_acc next_year next_month (remaining - 1)
  in
  generate_months [] start_year start_month count
