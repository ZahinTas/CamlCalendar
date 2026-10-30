open Calendar
(* ANSI COLOR FUNCTIONS *)
let red s = "\027[31m" ^ s ^ "\027[0m"
let blue s = "\027[34m" ^ s ^ "\027[0m"
let gray s = "\027[90m" ^ s ^ "\027[0m"
let reset = "\027[0m"

(** [read_line_safe prompt] prints [prompt] and reads a line from standard
    input. *)
let read_line_safe prompt =
  print_string prompt;
  flush Stdlib.stdout;
  try read_line () with _ -> ""

(** [read_list prompt] prints [prompt] and reads a comma-separated list of
    strings from standard input. *)
let read_list prompt =
  print_string (prompt ^ " (comma-separated): ");
  flush Stdlib.stdout;
  try
    String.split_on_char ',' (read_line ())
    |> List.map String.trim
    |> List.filter (( <> ) "")
  with _ -> []

(** [print_calendar_header year month] prints a header with the full month name
    and year. Requires: [month] is between 1 and 12. *)
let print_calendar_header year month =
  let title = Printf.sprintf "%s %d" (month_name month) year in
  Printf.printf "\n=============================\n";
  Printf.printf "        %s\n" title;
  Printf.printf "=============================\n"

(* Helper functions for month names *)

(** [short_month_name month] is the three-letter abbreviation of the month
    number [month]. Requires: [month] is between 1 and 12. Raises:
    [Invalid_argument] if [month] is outside this range. *)
let short_month_name month =
  match month with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> invalid_arg "Invalid month"


(** [center width str] is a new string where [str] is centered in a field of
    [width] characters. Requires: [width] is non-negative. *)
let center width str =
  let len = String.length str in
  if len >= width then str
  else
    let padding = (width - len) / 2 in
    let left_padding = String.make padding ' ' in
    let right_padding = String.make (width - len - padding) ' ' in
    left_padding ^ str ^ right_padding

(** [pad_to_width width str] pads [str] with spaces on the right to reach
    [width]. Requires: [width] is non-negative. *)
let pad_to_width width str =
  let len = String.length str in
  if len >= width then str else str ^ String.make (width - len) ' '


(** A version of the standard [List] module extended with two utility functions.
    [take n lst] is the first [n] elements of [lst] (or all elements if [lst]
    has fewer than [n]). [drop n lst] is [lst] without its first [n] elements
    (or [[]] if [n] exceeds the length of [lst]). *)
module List = struct
  include List

  let take n lst =
    let rec aux acc n = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | x :: xs -> aux (x :: acc) (n - 1) xs
    in
    aux [] n lst

  let drop n lst =
    let rec aux n' lst' =
      match lst' with
      | [] -> []
      | _ when n' <= 0 -> lst'
      | _ :: xs -> aux (n' - 1) xs
    in
    aux n lst
end
(** [print_yearly_calendar start_year start_month] prints a calendar view for 12
    months starting from [start_year] and [start_month]. Requires: [start_month]
    is between 1 and 12. *)
let print_yearly_calendar start_year start_month =
  let months = get_month_range start_year start_month 12 in

  Printf.printf
    "\n===================================================================\n";
  Printf.printf "                     YEARLY CALENDAR VIEW\n";
  Printf.printf
    "===================================================================\n\n";

  let months_per_row = 3 in
  let rec print_months_group month_list =
    match month_list with
    | [] -> ()
    | _ ->
        let group =
          if List.length month_list >= months_per_row then
            List.take months_per_row month_list
          else month_list
        in
        let rest =
          if List.length month_list >= months_per_row then
            List.drop months_per_row month_list
          else []
        in

        List.iter
          (fun (year, month) ->
            Printf.printf "      %s %-4d         "
              (String.sub (month_name month) 0 3)
              year)
          group;
        Printf.printf "\n\n";


        List.iter (fun _ -> Printf.printf " M  T  W  T  F  S  S  ") group;
        Printf.printf "\n";

        let month_data =
          List.map
            (fun (year, month) ->
              let first_wday = first_weekday year month in
              let days = days_in_month year month in
              (first_wday, days, year, month))
            group
        in

        for week = 0 to 5 do
          List.iter
            (fun (first_wday, days, year, month) ->
              for col = 1 to 7 do
                let day_num = (week * 7) + col - (first_wday - 1) in
                if day_num >= 1 && day_num <= days then
                  let fmt =
                    if is_today year month day_num then
                      red (Printf.sprintf "%2d" day_num)
                    else if is_holiday month day_num then
                      blue (Printf.sprintf "%2d" day_num)
                    else if is_past_day year month day_num then
                      gray (Printf.sprintf "%2d" day_num)
                    else Printf.sprintf "%2d" day_num
                  in
                  Printf.printf "%s " fmt
                else Printf.printf "   "
              done;
              Printf.printf " ")
            month_data;
          Printf.printf "\n"
        done;
        Printf.printf "\n";

        print_months_group rest
  in

  print_months_group months;
  Printf.printf "====================================================\n"

(** [print_weekly_calendar ~username year month day] prints a weekly calendar
    view containing the date specified by [year], [month], and [day]. Displays a
    header with the week range, prints a mini-calendar grid highlighting today /
    holidays / past days, and then lists any events or todos for that week
    loaded from the user's files. Requires: [month] is between 1 and 12, and
    [day] is a valid day in the given month and year. *)
let print_weekly_calendar ~username year month day =
  let start_date, end_date = get_week_range year month day in

  Printf.printf "\n====================================================\n";
  Printf.printf "           WEEKLY VIEW: %s to %s\n" start_date end_date;
  Printf.printf "====================================================\n\n";

  Printf.printf "Week %d\n\n" (get_week_number year month day);

  Printf.printf " Mon Tue Wed Thu Fri Sat Sun\n";

  let start_year = int_of_string (String.sub start_date 0 4) in
  let start_month = int_of_string (String.sub start_date 5 2) in
  let start_day = int_of_string (String.sub start_date 8 2) in

  for d = 0 to 6 do
    let current_day = start_day + d in
    let current_year, current_month =
      if current_day > days_in_month start_year start_month then
        if start_month = 12 then (start_year + 1, 1)
        else (start_year, start_month + 1)
      else (start_year, start_month)
    in
    let actual_day =
      if current_day > days_in_month start_year start_month then
        current_day - days_in_month start_year start_month
      else current_day
    in

    let fmt =
      if is_today current_year current_month actual_day then
        red (Printf.sprintf "%2d" actual_day)
      else if is_holiday current_month actual_day then
        blue (Printf.sprintf "%2d" actual_day)
      else if is_past_day current_year current_month actual_day then
        gray (Printf.sprintf "%2d" actual_day)
      else Printf.sprintf "%2d" actual_day
    in
    Printf.printf "  %s" fmt
  done;
  Printf.printf "\n\n";

  Printf.printf "Events this week:\n";
  Printf.printf "-----------------\n";

  for d = 0 to 6 do
    let current_day = start_day + d in
    let current_year, current_month =
      if current_day > days_in_month start_year start_month then
        if start_month = 12 then (start_year + 1, 1)
        else (start_year, start_month + 1)
      else (start_year, start_month)
    in
    let actual_day =
      if current_day > days_in_month start_year start_month then
        current_day - days_in_month start_year start_month
      else current_day
    in

    let date_str =
      Printf.sprintf "%04d-%02d-%02d" current_year current_month actual_day
    in
    let filename = date_str ^ ".json" in
    let day_data = load_day_from_file ~username filename in

    if day_data.events <> [] || day_data.todos <> [] then (
      let wday_names = [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" ] in
      let tm =
        {
          Unix.tm_sec = 0;
          tm_min = 0;
          tm_hour = 12;
          tm_mday = actual_day;
          tm_mon = current_month - 1;
          tm_year = current_year - 1900;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }
      in
      let wday = (Unix.localtime (fst (Unix.mktime tm))).tm_wday in
      let wday = if wday = 0 then 6 else wday - 1 in
      let day_name = List.nth wday_names wday in

      Printf.printf "%s %02d/%02d:\n" day_name current_month actual_day;
      if day_data.events <> [] then
        List.iter
          (fun e ->
            Printf.printf "  • %s (%s-%s)\n" e.name e.start_time e.end_time)
          day_data.events;
      if day_data.todos <> [] then
        List.iter
          (fun (t : todo) -> Printf.printf "  □ %s\n" t.name)
          day_data.todos;
      Printf.printf "\n")
  done;

  Printf.printf "====================================================\n"

(** [print_legend ()] prints a legend explaining the color codes used in the
    calendar display. *)
let print_legend () =
  print_endline
    "\n\
     +----------------------+\n\
     | Colors Legend        |\n\
     +----------------------+";
  print_endline "| 🔴 Today             |";
  print_endline "| 🟣 Holiday           |";
  print_endline "| 🩶 Past day          |";
  print_endline "| ⚪ Future day        |";
  print_endline "+----------------------+\n"

(** [print_calendar year month] prints a monthly calendar for the given [year]
    and [month]. It includes a legend, a header with the month and year, and
    shows each day with colors indicating today, holidays, past days, or future
    days. Requires: [month] is between 1 and 12. *)
let print_calendar year month =
  print_legend ();
  print_calendar_header year month;

  let days = days_in_month year month in
  let first_wday = first_weekday year month in

  Printf.printf " Mon Tue Wed Thu Fri Sat Sun\n";
  for _ = 1 to first_wday - 1 do
    Printf.printf "    "
  done;

  for d = 1 to days do
    let fmt =
      if is_today year month d then red (Printf.sprintf "%02d" d)
      else if is_holiday month d then blue (Printf.sprintf "%02d" d)
      else if is_past_day year month d then gray (Printf.sprintf "%02d" d)
      else Printf.sprintf "%02d" d
    in
    Printf.printf "  %s" fmt;
    if (first_wday - 1 + d) mod 7 = 0 then Printf.printf "\n"
  done;
  Printf.printf "\n"

(** [print_day_view day] prints a detailed view of the given [day], including
    its date, any special day name, to-do list items, and events. The to-do list
    shows each item’s completion status, name, ID, and description. The events
    list shows each event’s name, ID, time range, location, and invited people
    if any. Requires: [day] contains valid date and associated data. *)
let print_day_view day =
  Printf.printf "\n========================================\n";
  Printf.printf "         📅 %s\n" day.date;
  Printf.printf "========================================\n\n";

  (match day.special_day_name with
  | Some s -> Printf.printf "🎉 Special Day: %s\n\n" s
  | None -> ());

  if day.todos <> [] then begin
    Printf.printf "📝 TO-DO LIST:\n----------------------------------------\n";
    List.iter
      (fun t ->
        Printf.printf "  [%s] %s (ID:%d)\n      %s\n"
          (if t.completed then "✓" else " ")
          t.name t.id t.description)
      day.todos;
    Printf.printf "\n"
  end;

  if day.events <> [] then begin
    Printf.printf "📅 EVENTS:\n----------------------------------------\n";
    List.iter
      (fun e ->
        Printf.printf "  • %s (ID:%d)\n    Time: %s - %s\n    Location: %s\n"
          e.name e.id e.start_time e.end_time e.location;
        if e.invited_people <> [] then
          Printf.printf "    Invited: %s\n"
            (String.concat ", " e.invited_people))
      day.events;
    Printf.printf "\n"
  end;

  Printf.printf "========================================\n\n"

(** [manage_todos ~username filename day] interactively manages the to-do list
    for [day], allowing the user to add, toggle, or delete to-dos. Returns the
    updated [day] record reflecting all changes made during the session.
    Requires: [day] is a valid day record with a properly initialized to-do
    list. *)
let rec manage_todos ~username filename day =
  Printf.printf "\n📝 TO-DO MANAGEMENT:\n";
  Printf.printf
    "1. Add To-Do\n\
     2. Toggle To-Do\n\
     3. Delete To-Do\n\
     4. Back\n\
     Choose an option: ";
  flush Stdlib.stdout;

  match read_line () with
  | "1" ->
      let name = read_line_safe "Enter task name: " in
      let description = read_line_safe "Enter task description: " in
      let updated_day = add_todo day name description in
      save_day_to_file ~username filename updated_day;
      print_endline "✓ To-do added!";
      manage_todos ~username filename updated_day
  | "2" ->
      if day.todos = [] then (
        print_endline "No to-dos available!";
        manage_todos ~username filename day)
      else (
        print_endline "Select a To-Do to toggle (check/uncheck):";
        List.iter
          (fun (t : todo) ->
            Printf.printf "%d: [%s] %s\n" t.id
              (if t.completed then "✓" else " ")
              t.name)
          day.todos;

        print_string "> ";
        flush Stdlib.stdout;
        try
          let todo_id = int_of_string (read_line ()) in
          let updated_day = toggle_todo day todo_id in
          save_day_to_file ~username filename updated_day;
          print_endline "✓ To-do updated!";
          manage_todos ~username filename updated_day
        with _ ->
          print_endline "Invalid ID";
          manage_todos ~username filename day)
  | "3" ->
      if day.todos = [] then (
        print_endline "No to-dos to delete!";
        manage_todos ~username filename day)
      else (
        print_endline "Select a To-Do to delete:";
        List.iter
          (fun (t : todo) -> Printf.printf "%d: %s\n" t.id t.name)
          day.todos;

        print_string "> ";
        flush Stdlib.stdout;
        try
          let todo_id = int_of_string (read_line ()) in
          let updated_day = delete_todo day todo_id in
          save_day_to_file ~username filename updated_day;
          print_endline "✓ To-do deleted!";
          manage_todos ~username filename updated_day
        with _ ->
          print_endline "Invalid ID";
          manage_todos ~username filename day)
  | "4" -> day
  | _ ->
      print_endline "Invalid option";
      manage_todos ~username filename day

(** [print_invitation_status email success] prints the status of sending an
    invitation to [email]. Prints a success message if [success] is [true],
    otherwise prints a failure message. Requires: [email] is a valid email. *)
let print_invitation_status email success =
  if success then Printf.printf "  ✓ Invitation sent to %s\n" email
  else Printf.printf "  ✗ Failed to send invitation to %s\n" email

(** [handle_event_invitations event] sends invitations for [event] and prints
    the status of each invitation. If no invitations are sent, prints a message
    indicating no emails were available. Requires: [event] is a valid event
    record. *)
let handle_event_invitations event =
  let results = send_event_invitations event in
  match results with
  | [] -> print_endline "No email addresses to send invitations to."
  | results ->
      print_endline "\nInvitation status:";
      List.iter
        (fun (email, success) -> print_invitation_status email success)
        results

(** [manage_events ~username filename day] interactively manages events for
    [day], allowing adding, deleting, and editing events, saving all changes to
    [filename] under [username], validating event times, and automatically
    sending invitations when emails are provided. It returns the updated [day]
    with all modifications. Requires: [day] is a valid day record. *)
let rec manage_events ~username filename day =
  Printf.printf "\n📅 EVENT MANAGEMENT:\n";
  Printf.printf
    "1. Add Event\n2. Delete Event\n3. Edit Event\n4. Back\nChoose an option: ";
  flush Stdlib.stdout;

  match read_line () with
  | "1" ->
      let rec read_event () =
        let name = read_line_safe "Enter event name: " in
        let start_time = read_line_safe "Enter start time (HH:MM): " in
        let end_time = read_line_safe "Enter end time (HH:MM): " in
        if time_to_minutes end_time <= time_to_minutes start_time then (
          print_endline "❌ End time must be after start time. Please re-enter.";
          read_event ())
        else
          let location = read_line_safe "Enter location: " in
          let invited = read_list "Enter invited people" in
          let emails = read_list "Enter email addresses" in
          let pic = read_line_safe "Enter picture path (optional): " in
          let picture_path = if pic = "" then None else Some pic in
          (name, start_time, end_time, location, invited, emails, picture_path)
      in
      let event_data = read_event () in
      let updated_day = add_event day event_data in
      save_day_to_file ~username filename updated_day;
      print_endline "✓ Event added!";

      let _, _, _, _, _, emails, _ = event_data in
      if emails <> [] then (
        print_endline "\nSending invitations...";
        let new_event_id = List.length day.events + 1 in
        match get_event_by_id updated_day new_event_id with
        | Some event -> handle_event_invitations event
        | None -> ());

      manage_events ~username filename updated_day
  | "2" ->
      if day.events = [] then (
        print_endline "No events to delete!";
        manage_events ~username filename day)
      else (
        print_endline "Select an Event to delete:";
        List.iter (fun e -> Printf.printf "%d: %s\n" e.id e.name) day.events;

        print_string "> ";
        flush Stdlib.stdout;
        try
          let event_id = int_of_string (read_line ()) in
          let updated_day = delete_event day event_id in
          save_day_to_file ~username filename updated_day;
          print_endline "✓ Event deleted!";
          manage_events ~username filename updated_day
        with _ ->
          print_endline "Invalid ID";
          manage_events ~username filename day)
  | "3" ->
      if day.events = [] then (
        print_endline "No events to edit!";
        manage_events ~username filename day)
      else (
        Printf.printf "Select an Event ID to edit:\n";
        List.iter (fun e -> Printf.printf "%d: %s\n" e.id e.name) day.events;
        print_string "> ";
        flush Stdlib.stdout;
        try
          let event_id = int_of_string (read_line ()) in
          match get_event_by_id day event_id with
          | Some ev ->
              let name =
                read_line_safe ("Enter new name [" ^ ev.name ^ "]: ")
              in
              let start_time =
                read_line_safe ("Enter new start time [" ^ ev.start_time ^ "]: ")
              in
              let end_time =
                read_line_safe ("Enter new end time [" ^ ev.end_time ^ "]: ")
              in
              let location =
                read_line_safe ("Enter new location [" ^ ev.location ^ "]: ")
              in
              let name = if name = "" then ev.name else name in
              let start_time =
                if start_time = "" then ev.start_time else start_time
              in
              let end_time = if end_time = "" then ev.end_time else end_time in
              let location = if location = "" then ev.location else location in
              let updated_day =
                update_event day event_id ~name ~start_time ~end_time ~location
              in
              save_day_to_file ~username filename updated_day;
              print_endline "✓ Event updated!";
              manage_events ~username filename updated_day
          | None ->
              print_endline "Invalid ID";
              manage_events ~username filename day
        with _ ->
          print_endline "Invalid input";
          manage_events ~username filename day)
  | "4" -> day
  | _ ->
      print_endline "Invalid option";
      manage_events ~username filename day

(** [edit_day_terminal ~username filename date] enters an interactive terminal
    session to edit a day's details. Requires: [filename] corresponds to a valid
    day JSON file for [username]. *)
let rec edit_day_terminal ~username filename date =
  let day = load_day_from_file ~username filename in
  print_day_view day;

  Printf.printf
    "Options:\n\
     1. Add Special Day Name\n\
     2. Manage To-Dos\n\
     3. Manage Events\n\
     4. Return to Calendar\n\
     Choose an option: ";
  flush Stdlib.stdout;

  match read_line () with
  | "1" ->
      let name = read_line_safe "Enter special day name: " in
      let updated_day = set_special_day_name day name in
      save_day_to_file ~username filename updated_day;
      print_endline "✓ Special day name added!\n";
      edit_day_terminal ~username filename date
  | "2" ->
      let _ = manage_todos ~username filename day in
      edit_day_terminal ~username filename date
  | "3" ->
      let _ = manage_events ~username filename day in
      edit_day_terminal ~username filename date
  | "4" -> ()
  | _ ->
      print_endline "Invalid option";
      edit_day_terminal ~username filename date


(** [format_month_lines width year month] is a list of strings representing the
    calendar lines for the given [month] and [year], each string having length
    [width]. Each day is formatted as a two-character right-aligned number
    positioned correctly according to the weekday, filling up to 6 lines.
    Requires: [width] is non-negative, [month] is between 1 and 12, and [year]
    is positive. *)
let format_month_lines width year month =
  let days = days_in_month year month in
  let first_wday = first_weekday year month in
  let lines = Array.make 6 (String.make width ' ') in
  let day_str d = Printf.sprintf "%2d" d in
  let rec fill_lines day =
    if day > days then ()
    else
      let col = (first_wday + day - 2) mod 7 in
      let row = (first_wday + day - 2) / 7 in
      let line = Bytes.of_string lines.(row) in
      Bytes.blit_string (day_str day) 0 line (col * 4) 2;
      lines.(row) <- Bytes.to_string line;
      fill_lines (day + 1)
  in
  fill_lines 1;
  Array.to_list lines

(** [print_yearly_calendar start_year start_month] prints a formatted yearly
    calendar view starting from [start_year] and [start_month], displaying 12
    consecutive months arranged in rows of three months each. Requires:
    [start_month] is between 1 and 12, and [start_year] is positive. *)
let print_yearly_calendar start_year start_month =
  let months = get_month_range start_year start_month 12 in

  Printf.printf "\n====================================================\n";
  Printf.printf "           YEARLY CALENDAR VIEW\n";
  Printf.printf "====================================================\n\n";

  let months_per_row = 3 in
  let month_width = 28 in

  let rec print_groups = function
    | [] -> ()
    | remaining ->
        let group =
          if List.length remaining >= months_per_row then
            List.take months_per_row remaining
          else remaining
        in
        let rest =
          if List.length remaining >= months_per_row then
            List.drop months_per_row remaining
          else []
        in

        let infos =
          List.map
            (fun (year, month) ->
              let title =
                Printf.sprintf "%s %d" (short_month_name month) year
              in
              let title_c = center month_width title in
              let weekday_header =
                let names = [ "M "; "T "; "W "; "Th"; "F "; "Sa"; "Su" ] in
                let h = String.concat "  " names in
                pad_to_width month_width h
              in
              let week_lines = format_month_lines month_width year month in
              (title_c, weekday_header, week_lines))
            group
        in

        List.iter (fun (t, _, _) -> Printf.printf "%s    " t) infos;
        Printf.printf "\n";

        List.iter (fun (_, h, _) -> Printf.printf "%s    " h) infos;
        Printf.printf "\n";

        for i = 0 to 5 do
          List.iter
            (fun (_, _, lines) ->
              let line = List.nth lines i in
              Printf.printf "%s    " line)
            infos;
          Printf.printf "\n"
        done;
        Printf.printf "\n";

        print_groups rest
  in

  print_groups months;
  Printf.printf "====================================================\n"

(** [parse_date_input input] is [Some (year, month, day)], otherwise it's
    [None]. Requires: [input] is in a valid date format. *)
let parse_date_input input =
  if input = "" then None 
  else
    try
      match String.split_on_char '/' input with
      | [ m_str; d_str; y_str ] ->
          let month = int_of_string m_str in
          let day = int_of_string d_str in
          let year_part = int_of_string y_str in

          let year =
            if year_part >= 0 && year_part <= 99 then
              if year_part >= 70 then 1900 + year_part else 2000 + year_part
            else year_part
          in


          if month >= 1 && month <= 12 && day >= 1 && day <= 31 then
            Some (year, month, day)
          else None
      | _ -> None
    with _ -> None

(** [calendar_loop ~username year month] runs the main calendar interaction loop
    for the given user starting at [year] and [month]. Requires: [month] is
    between 1 and 12. *)
let rec calendar_loop ~username year month =
  print_calendar year month;
  let days = days_in_month year month in
  Printf.printf
    "\n\
     Select a day (1-%d), n/p for next/prev month, y for yearly view, w for \
     weekly view, s to sign out, q to quit: "
    days;
  flush Stdlib.stdout;
  match read_line () with
  | "q" ->
      print_endline "Goodbye!";
      exit 0
  | "s" ->
      print_endline "Signing out...";
      ()
  | "n" ->
      let next_month, next_year =
        if month = 12 then (1, year + 1) else (month + 1, year)
      in
      calendar_loop ~username next_year next_month
  | "p" ->
      let prev_month, prev_year =
        if month = 1 then (12, year - 1) else (month - 1, year)
      in
      calendar_loop ~username prev_year prev_month
  | "y" -> (
      print_yearly_calendar year month;
      Printf.printf "\nPress Enter to return to monthly view,\n";
      Printf.printf "or enter a date (MM/DD/YY) to jump to that month: ";
      flush Stdlib.stdout;
      let input = read_line () in
      match parse_date_input input with
      | None ->
          calendar_loop ~username year month
      | Some (new_year, new_month, new_day) ->
          let valid_day = min new_day (days_in_month new_year new_month) in
          let date =
            Printf.sprintf "%04d-%02d-%02d" new_year new_month valid_day
          in
          let filename = date ^ ".json" in
          edit_day_terminal ~username filename date;
          calendar_loop ~username new_year new_month)
  | "w" -> (
      let current_year, current_month, current_day = get_today () in
      print_weekly_calendar ~username current_year current_month current_day;

      let start_date, _ =
        get_week_range current_year current_month current_day
      in
      let start_year = int_of_string (String.sub start_date 0 4) in
      let start_month = int_of_string (String.sub start_date 5 2) in
      let start_day = int_of_string (String.sub start_date 8 2) in

      Printf.printf "\nDays: 1=Mon(%02d) 2=Tue 3=Wed 4=Thu 5=Fri 6=Sat 7=Sun\n"
        start_day;
      Printf.printf "Select a day (1-7) to edit, or press Enter to return: ";
      flush Stdlib.stdout;
      let input = read_line () in
      if input = "" then calendar_loop ~username year month
      else
        try
          let day_offset = int_of_string input in
          if day_offset < 1 || day_offset > 7 then failwith "Invalid day";

          let target_day = start_day + (day_offset - 1) in

          let days_in_start_month = days_in_month start_year start_month in

          if target_day <= days_in_start_month then (
            let date =
              Printf.sprintf "%04d-%02d-%02d" start_year start_month target_day
            in
            let filename = date ^ ".json" in
            edit_day_terminal ~username filename date;
            calendar_loop ~username start_year start_month)
          else
            let next_month, next_year =
              if start_month = 12 then (1, start_year + 1)
              else (start_month + 1, start_year)
            in
            let actual_day = target_day - days_in_start_month in
            let date =
              Printf.sprintf "%04d-%02d-%02d" next_year next_month actual_day
            in
            let filename = date ^ ".json" in
            edit_day_terminal ~username filename date;
            calendar_loop ~username next_year next_month
        with _ ->
          print_endline "Invalid selection";
          calendar_loop ~username year month)
  | day_str -> (
      try
        let d = int_of_string day_str in
        if d < 1 || d > days then failwith "Invalid day";
        let date = Printf.sprintf "%04d-%02d-%02d" year month d in
        let filename = date ^ ".json" in
        edit_day_terminal ~username filename date;
        calendar_loop ~username year month
      with _ ->
        print_endline "Invalid input";
        calendar_loop ~username year month)
        
(** [signup_menu ()] prompts the user to create a new account, otherwise [None].
*)
let signup_menu () =
  print_string "Choose username: ";
  flush Stdlib.stdout;
  let username = read_line () in
  print_string "Choose password: ";
  flush Stdlib.stdout;
  let password = read_line () in

  match signup ~username ~password with
  | Ok () ->
      print_endline "Account created!";
      Some username
  | Error msg ->
      print_endline msg;
      None

(** [login_menu ()] prompts the user to log in. *)
let login_menu () =
  print_string "Username: ";
  flush Stdlib.stdout;
  let username = read_line () in
  print_string "Password: ";
  flush Stdlib.stdout;
  let password = read_line () in

  match login ~username ~password with
  | Ok () ->
      print_endline "Login successful!";
      Some username
  | Error msg ->
      print_endline msg;
      None

(** [auth_menu ()] repeatedly prompts the user to log in or sign up, or quit.
    Raises: [Exit] if the user chooses to quit. *)
let rec auth_menu () =
  print_endline "\n1. Log in\n2. Sign up\n3. Quit";
  print_string "> ";
  flush Stdlib.stdout;

  match read_line () with
  | "1" -> (
      match login_menu () with
      | Some u -> u
      | None -> auth_menu ())
  | "2" -> (
      match signup_menu () with
      | Some u -> u
      | None -> auth_menu ())
  | "3" -> exit 0
  | _ ->
      print_endline "Invalid option";
      auth_menu ()

let () =
  ensure_data_folder ();
  setup_signal_handlers ();
  let rec main_loop () =
    let username = auth_menu () in
    let year, month, _ = get_today () in
    calendar_loop ~username year month;
    main_loop ()
  in
  main_loop ()