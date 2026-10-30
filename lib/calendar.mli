(** Calendar module interface defining data types, file operations, user
    authentication, calendar utilities, day and event management, invitations,
    and signal handling. *)

(* Data types *)

type todo = {
  id : int;
  name : string;
  description : string;
  completed : bool;
}
(** A todo task with an id, name, description, and completion status. *)

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
(** An event with id, name, start/end times, location, invitees, emails, and
    optional picture. *)

type day_data = {
  date : string;
  special_day_name : string option;
  todos : todo list;
  events : event list;
}
(** Data for a calendar day including date string, optional special name, todos,
    and events. *)

(* File/data handling *)

val ensure_data_folder : unit -> unit
(** [ensure_data_folder ()] creates "data" and "user" directories if absent. *)

val save_day_to_file : username:string -> string -> day_data -> unit
(** [save_day_to_file ~username filename day] saves the [day] data as JSON in
    the data folder under the given [username] and [filename]. *)

val load_day_from_file : username:string -> string -> day_data
(** [load_day_from_file ~username filename] loads the day data stored in the
    given [filename] under the folder for user [username]. Returns an empty day
    record if the file does not exist. *)

(* Authentication *)

val signup : username:string -> password:string -> (unit, string) result
(** [signup ~username ~password] attempts to add a new user with the given
    [username] and [password]. Returns [Ok ()] if successful, or [Error msg] if
    the username already exists. *)

val login : username:string -> password:string -> (unit, string) result
(** [login ~username ~password] checks if the given [username] and [password]
    match stored credentials. Returns [Ok ()] on success, or [Error msg] if the
    username is not found or the password is incorrect. *)

val load_users : unit -> (string * string) list
(** [load_users ()] loads all username-password pairs stored in the user
    database. *)

(* Calendar utilities *)

val month_name : int -> string
(** [month_name m] is the full name of month [m]. *)

val days_in_month : int -> int -> int
(** [days_in_month year month] is the number of days in the given [month] of the
    specified [year]. *)

val first_weekday : int -> int -> int
(** [first_weekday year month] is the weekday of the first day of [month] in
    [year], where Monday = 1 and Sunday = 7. *)

val is_holiday : int -> int -> bool
(** [is_holiday month day] is [true] if the given [month] and [day] correspond
    to a recognized holiday, [false] otherwise. *)

val is_today : int -> int -> int -> bool
(** [is_today year month day] is true if the date matches today’s date. *)

val is_past_day : int -> int -> int -> bool
(** [is_past_day year month day] is true if the given date is before today. *)

val get_today : unit -> int * int * int
(** [get_today ()] is current (year, month, day). *)

(* Day operations *)

val set_special_day_name : day_data -> string -> day_data
(** [set_special_day_name day name] is [day] updated with the special day name
    [name]. *)

val get_todo_by_id : day_data -> int -> todo option
(** [get_todo_by_id day todo_id] is [Some todo] with the given [todo_id] if it
    exists in [day], otherwise [None]. *)

val get_event_by_id : day_data -> int -> event option
(** [get_event_by_id day event_id] is [Some event] with the given [event_id] if
    it exists in [day], otherwise [None]. *)

(* Todo operations *)

val add_todo : day_data -> string -> string -> day_data
(** [add_todo day name description] is [day] updated with a new todo having the
    given [name] and [description]. *)

val toggle_todo : day_data -> int -> day_data
(** [toggle_todo day todo_id] is [day] with the completed status of the todo
    identified by [todo_id] toggled. *)

val delete_todo : day_data -> int -> day_data
(** [delete_todo day todo_id] is [day] with the todo identified by [todo_id]
    removed. *)

val next_id : 'a list -> ('a -> int) -> int
(** [next_id items get_id] is one greater than the maximum id in [items] as
    determined by [get_id], or 1 if [items] is empty. *)

(* Event operations *)

val add_event :
  day_data ->
  string * string * string * string * string list * string list * string option ->
  day_data
(** [add_event day event_data] adds a new event created from [event_data] to the
    [day]. *)

val update_event :
  day_data ->
  int ->
  name:string ->
  start_time:string ->
  end_time:string ->
  location:string ->
  day_data
(** [update_event day event_id ~name ~start_time ~end_time ~location] is [day]
    with the event identified by [event_id] updated with the new [name],
    [start_time], [end_time], and [location]. *)

val delete_event : day_data -> int -> day_data
(** [delete_event day event_id] removes the event with the given id from [day].
*)

val time_to_minutes : string -> int
(** [time_to_minutes t] converts a time string "HH:MM" to the total number of
    minutes since midnight. *)

val sort_events_by_time : event list -> event list
(** [sort_events_by_time events] is the list of [events] sorted by start time,
    then by end time. *)

(* Invitations *)

val send_invitations :
  recipient:string ->
  subject:string ->
  body:string ->
  attachment:string option ->
  unit
(** [send_invitations ~recipient ~subject ~body ~attachment] sends an email
    invitation and returns the system exit code (0 on success). *)

(* Signal handling *)

val setup_signal_handlers : unit -> unit
(** [setup_signal_handlers ()] installs handlers to safely exit on signals. *)

(* Yearly and Weekly views *)

val get_week_range : int -> int -> int -> string * string
(** [get_week_range year month day] is (start_date, end_date) strings for the
    week containing the given date. *)

val get_week_number : int -> int -> int -> int
(** [get_week_number year month day] is the ISO week number of the given date.
*)

val get_week_dates : unit -> (int * int * int) list
(** [get_week_dates ()] is list of (year, month, day) tuples for current week
    starting Monday. *)

val get_month_range : int -> int -> int -> (int * int) list
(** [get_month_range start_year start_month count] is list of (year, month)
    tuples for [count] months starting at given month. *)

(** Invitations *)

val send_invitations :
  recipient:string ->
  subject:string ->
  body:string ->
  attachment:string option ->
  int
(** [send_invitations ~recipient ~subject ~body ~attachment] sends an email with
    the given fields and returns the exit code from the system mailer (0
    indicates success). *)

val send_event_invitations : event -> (string * bool) list
(** [send_event_invitations event] sends invitations to all email addresses in
    [event]. Returns a list of (email, success) pairs. *)
