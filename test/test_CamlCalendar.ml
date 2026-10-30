(* test_calendar.ml *)
open OUnit2
open Calendar

let create_test_todo id name desc completed =
  { id; name; description = desc; completed }

let create_test_event id name start_time end_time location invited emails
    picture =
  {
    id;
    name;
    start_time;
    end_time;
    location;
    invited_people = invited;
    email_addresses = emails;
    picture_path = picture;
  }

let create_test_day date special_name todos events =
  { date; special_day_name = special_name; todos; events }

let setup_test_environment () =
  (* Clean up any existing test data *)
  if Sys.file_exists "test_data" then ignore (Sys.command "rm -rf test_data");

  (* Create fresh test directories *)
  Unix.mkdir "test_data" 0o755;
  Unix.mkdir "test_data/user1" 0o755;
  Unix.mkdir "test_data/user2" 0o755;

  (* Setup for authentication tests *)
  if Sys.file_exists "user" then ignore (Sys.command "rm -rf user");
  Unix.mkdir "user" 0o755

let teardown_test_environment () =
  (* Clean up after tests *)
  if Sys.file_exists "test_data" then ignore (Sys.command "rm -rf test_data");
  if Sys.file_exists "user" then ignore (Sys.command "rm -rf user");
  if Sys.file_exists "data" then ignore (Sys.command "rm -rf data")

let test_ensure_data_folder =
  "Test ensure_data_folder creates necessary directories" >:: fun _ ->
  if Sys.file_exists "data" then ignore (Sys.command "rm -rf data");

  ensure_data_folder ();

  assert_bool "data folder should exist" (Sys.file_exists "data");
  assert_bool "user folder should exist" (Sys.file_exists "user")

let test_save_and_load_day =
  "Test save_day_to_file and load_day_from_file" >:: fun _ ->
  setup_test_environment ();

  (* Create a test day *)
  let todo = create_test_todo 1 "Test todo" "Description" false in
  let event =
    create_test_event 1 "Meeting" "09:00" "10:00" "Office" [ "Alice" ]
      [ "alice@test.com" ] None
  in
  let day =
    create_test_day "2024-01-15" (Some "Special Day") [ todo ] [ event ]
  in

  (* Save to file *)
  save_day_to_file ~username:"user1" "2024-01-15.json" day;

  (* Verify file exists *)
  let filepath = "data/user1/2024-01-15.json" in
  assert_bool "File should exist" (Sys.file_exists filepath);

  (* Load from file *)
  let loaded_day = load_day_from_file ~username:"user1" "2024-01-15.json" in

  (* Verify data integrity *)
  assert_equal ~printer:(fun x -> x) day.date loaded_day.date;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    day.special_day_name loaded_day.special_day_name;
  assert_equal ~printer:string_of_int (List.length day.todos)
    (List.length loaded_day.todos);
  assert_equal ~printer:string_of_int (List.length day.events)
    (List.length loaded_day.events);

  (* Verify todo details *)
  let loaded_todo = List.hd loaded_day.todos in
  assert_equal ~printer:string_of_int todo.id loaded_todo.id;
  assert_equal ~printer:(fun x -> x) todo.name loaded_todo.name;
  assert_equal ~printer:(fun x -> x) todo.description loaded_todo.description;
  assert_equal ~printer:string_of_bool todo.completed loaded_todo.completed;

  (* Verify event details *)
  let loaded_event = List.hd loaded_day.events in
  assert_equal ~printer:string_of_int event.id loaded_event.id;
  assert_equal ~printer:(fun x -> x) event.name loaded_event.name;
  assert_equal ~printer:(fun x -> x) event.start_time loaded_event.start_time;
  assert_equal ~printer:(fun x -> x) event.end_time loaded_event.end_time;
  assert_equal ~printer:(fun x -> x) event.location loaded_event.location;

  teardown_test_environment ()

let test_load_nonexistent_day =
  "Test load_day_from_file for non-existent file" >:: fun _ ->
  setup_test_environment ();

  let day = load_day_from_file ~username:"user1" "nonexistent.json" in

  assert_equal ~printer:(fun x -> x) "nonexistent.json" day.date;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    None day.special_day_name;
  assert_equal ~printer:string_of_int 0 (List.length day.todos);
  assert_equal ~printer:string_of_int 0 (List.length day.events);

  teardown_test_environment ()

let test_signup_success =
  "Test successful user signup" >:: fun _ ->
  setup_test_environment ();

  let result = signup ~username:"testuser" ~password:"password123" in

  (match result with
  | Ok () -> ()
  | Error msg -> assert_failure ("Signup failed: " ^ msg));

  teardown_test_environment ()

let test_signup_duplicate =
  "Test signup with existing username" >:: fun _ ->
  setup_test_environment ();

  let _ = signup ~username:"testuser" ~password:"password123" in
  let result = signup ~username:"testuser" ~password:"differentpass" in

  (match result with
  | Error msg -> assert_equal ~printer:(fun x -> x) "Username exists!" msg
  | Ok _ -> assert_failure "Should have failed with duplicate username");

  teardown_test_environment ()

let test_login_success =
  "Test successful login" >:: fun _ ->
  setup_test_environment ();

  let _ = signup ~username:"testuser" ~password:"password123" in
  let result = login ~username:"testuser" ~password:"password123" in

  (match result with
  | Ok () -> ()
  | Error msg -> assert_failure ("Login failed: " ^ msg));

  teardown_test_environment ()

let test_login_wrong_password =
  "Test login with wrong password" >:: fun _ ->
  setup_test_environment ();

  let _ = signup ~username:"testuser" ~password:"password123" in
  let result = login ~username:"testuser" ~password:"wrongpass" in

  (match result with
  | Error msg -> assert_equal ~printer:(fun x -> x) "Wrong password" msg
  | Ok _ -> assert_failure "Should have failed with wrong password");

  teardown_test_environment ()

let test_login_nonexistent_user =
  "Test login with non-existent user" >:: fun _ ->
  setup_test_environment ();

  let result = login ~username:"nonexistent" ~password:"anything" in

  (match result with
  | Error msg -> assert_equal ~printer:(fun x -> x) "Username not found" msg
  | Ok _ -> assert_failure "Should have failed with non-existent user");

  teardown_test_environment ()

let test_load_users =
  "Test load_users function" >:: fun _ ->
  setup_test_environment ();

  let users1 = load_users () in
  assert_equal ~printer:string_of_int 0 (List.length users1);

  (* Add a user and reload *)
  let _ = signup ~username:"user1" ~password:"pass1" in
  let users2 = load_users () in
  assert_equal ~printer:string_of_int 1 (List.length users2);

  (* Add another user *)
  let _ = signup ~username:"user2" ~password:"pass2" in
  let users3 = load_users () in
  assert_equal ~printer:string_of_int 2 (List.length users3);

  (* Verify stored usernames *)
  let usernames = List.map fst users3 in
  assert_bool "Should contain user1" (List.mem "user1" usernames);
  assert_bool "Should contain user2" (List.mem "user2" usernames);

  teardown_test_environment ()

let test_month_name =
  "Test month_name function" >:: fun _ ->
  assert_equal ~printer:(fun x -> x) "January" (month_name 1);
  assert_equal ~printer:(fun x -> x) "February" (month_name 2);
  assert_equal ~printer:(fun x -> x) "March" (month_name 3);
  assert_equal ~printer:(fun x -> x) "April" (month_name 4);
  assert_equal ~printer:(fun x -> x) "May" (month_name 5);
  assert_equal ~printer:(fun x -> x) "June" (month_name 6);
  assert_equal ~printer:(fun x -> x) "July" (month_name 7);
  assert_equal ~printer:(fun x -> x) "August" (month_name 8);
  assert_equal ~printer:(fun x -> x) "September" (month_name 9);
  assert_equal ~printer:(fun x -> x) "October" (month_name 10);
  assert_equal ~printer:(fun x -> x) "November" (month_name 11);
  assert_equal ~printer:(fun x -> x) "December" (month_name 12);

  (* Test invalid months *)
  assert_equal ~printer:(fun x -> x) "Unknown" (month_name 0);
  assert_equal ~printer:(fun x -> x) "Unknown" (month_name 13);
  assert_equal ~printer:(fun x -> x) "Unknown" (month_name (-1))

let test_days_in_month =
  "Test days_in_month function" >:: fun _ ->
  (* Non-leap year tests *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 1);
  (* January *)
  assert_equal ~printer:string_of_int 28 (days_in_month 2023 2);
  (* February non-leap *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 3);
  (* March *)
  assert_equal ~printer:string_of_int 30 (days_in_month 2023 4);
  (* April *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 5);
  (* May *)
  assert_equal ~printer:string_of_int 30 (days_in_month 2023 6);
  (* June *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 7);
  (* July *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 8);
  (* August *)
  assert_equal ~printer:string_of_int 30 (days_in_month 2023 9);
  (* September *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 10);
  (* October *)
  assert_equal ~printer:string_of_int 30 (days_in_month 2023 11);
  (* November *)
  assert_equal ~printer:string_of_int 31 (days_in_month 2023 12);

  (* Leap year test *)
  assert_equal ~printer:string_of_int 29 (days_in_month 2024 2);

  (* Century year not divisible by 400 *)
  assert_equal ~printer:string_of_int 28 (days_in_month 1900 2);

  (* Year divisible by 400 *)
  assert_equal ~printer:string_of_int 29 (days_in_month 2000 2)

let test_first_weekday =
  "Test first_weekday function" >:: fun _ ->
  (* Jan 1, 2024 was Monday (1) *)
  assert_equal ~printer:string_of_int 1 (first_weekday 2024 1);

  (* Feb 1, 2024 was Thursday (4) *)
  assert_equal ~printer:string_of_int 4 (first_weekday 2024 2);

  (* March 1, 2024 was Friday (5) *)
  assert_equal ~printer:string_of_int 5 (first_weekday 2024 3);

  (* Jan 1, 2023 was Sunday (7) *)
  assert_equal ~printer:string_of_int 7 (first_weekday 2023 1);

  (* Same month/year should give same result *)
  assert_equal ~printer:string_of_int (first_weekday 2024 1)
    (first_weekday 2024 1)

let test_is_holiday =
  "Test is_holiday function" >:: fun _ ->
  (* Test known holidays *)
  assert_bool "Jan 1 should be holiday" (is_holiday 1 1);
  assert_bool "Jan 15 should be holiday" (is_holiday 1 15);
  assert_bool "Dec 25 should be holiday" (is_holiday 12 25);
  assert_bool "July 4 should be holiday" (is_holiday 7 4);

  (* Test non-holidays *)
  assert_bool "Feb 14 should NOT be holiday" (not (is_holiday 2 14));
  assert_bool "March 17 should NOT be holiday" (not (is_holiday 3 17));
  assert_bool "April 1 should NOT be holiday" (not (is_holiday 4 1));

  (* Invalid day *)
  assert_bool "Jan 32 should NOT be holiday" (not (is_holiday 1 32))

let test_is_today =
  "Test is_today function" >:: fun _ ->
  let year, month, day = get_today () in

  (* Should return true for today's date *)
  assert_bool "Today should be detected" (is_today year month day);

  (* Should return false for other dates *)
  assert_bool "Yesterday should NOT be today"
    (not (is_today year month (day - 1)));

  (* Handle month/year boundaries *)
  if day > 1 then
    assert_bool "Previous day should NOT be today"
      (not (is_today year month (day - 1)));

  if day < 28 then
    assert_bool "Next day should NOT be today"
      (not (is_today year month (day + 1)))

let test_is_past_day =
  "Test is_past_day function" >:: fun _ ->
  let year, month, day = get_today () in

  (* Past dates *)
  assert_bool "Yesterday should be past" (is_past_day year month (day - 1));

  (* Today should not be past *)
  assert_bool "Today should NOT be past" (not (is_past_day year month day));

  (* Future dates should not be past *)
  if day < 28 then (* Avoid month overflow *)
    assert_bool "Tomorrow should NOT be past"
      (not (is_past_day year month (day + 1)));

  (* Previous months/years *)
  if month > 1 then
    assert_bool "Last month should be past" (is_past_day year (month - 1) day);

  assert_bool "Last year should be past" (is_past_day (year - 1) month day)

let test_get_today =
  "Test get_today function" >:: fun _ ->
  let year, month, day = get_today () in

  (* Should return valid date components *)
  assert_bool "Year should be reasonable" (year >= 2000 && year <= 2100);
  assert_bool "Month should be 1-12" (month >= 1 && month <= 12);
  assert_bool "Day should be positive" (day > 0);

  (* Day should be valid for the month *)
  let max_days = days_in_month year month in
  assert_bool "Day should be valid for month" (day <= max_days);

  (* Should be consistent with Unix time *)
  let now = Unix.localtime (Unix.time ()) in
  assert_equal ~printer:string_of_int (now.tm_year + 1900) year;
  assert_equal ~printer:string_of_int (now.tm_mon + 1) month;
  assert_equal ~printer:string_of_int now.tm_mday day

let test_set_special_day_name =
  "Test set_special_day_name" >:: fun _ ->
  let day = create_test_day "2024-01-15" None [] [] in

  (* Set special name *)
  let special_day = set_special_day_name day "Birthday" in
  (match special_day.special_day_name with
  | Some name -> assert_equal ~printer:(fun x -> x) "Birthday" name
  | None -> assert_failure "Special day name should be set");

  (* Other fields should remain unchanged *)
  assert_equal ~printer:(fun x -> x) day.date special_day.date;
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.todos special_day.todos;
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.events special_day.events;

  (* Override existing special name *)
  let day_with_existing =
    create_test_day "2024-01-15" (Some "Old Name") [] []
  in
  let updated = set_special_day_name day_with_existing "New Name" in
  match updated.special_day_name with
  | Some name -> assert_equal ~printer:(fun x -> x) "New Name" name
  | None -> assert_failure "Should have new name"

let test_get_todo_by_id =
  "Test get_todo_by_id" >:: fun _ ->
  let todo1 = create_test_todo 1 "Task 1" "Desc 1" false in
  let todo2 = create_test_todo 2 "Task 2" "Desc 2" true in
  let todo3 = create_test_todo 3 "Task 3" "Desc 3" false in
  let day = create_test_day "2024-01-15" None [ todo1; todo2; todo3 ] [] in

  (* Find existing todo *)
  (match get_todo_by_id day 2 with
  | Some todo ->
      assert_equal ~printer:string_of_int 2 todo.id;
      assert_equal ~printer:(fun x -> x) "Task 2" todo.name;
      assert_equal ~printer:string_of_bool true todo.completed
  | None -> assert_failure "Should have found todo with ID 2");

  (* Find another existing todo *)
  (match get_todo_by_id day 1 with
  | Some todo -> assert_equal ~printer:string_of_int 1 todo.id
  | None -> assert_failure "Should have found todo with ID 1");

  (* Non-existent ID should return None *)
  (match get_todo_by_id day 99 with
  | Some _ -> assert_failure "Should not have found todo with ID 99"
  | None -> ());

  (* Empty todos list *)
  let empty_day = create_test_day "2024-01-15" None [] [] in
  match get_todo_by_id empty_day 1 with
  | Some _ -> assert_failure "Should not find todo in empty list"
  | None -> ()

let test_get_event_by_id =
  "Test get_event_by_id" >:: fun _ ->
  let event1 =
    create_test_event 1 "Meeting" "09:00" "10:00" "Office" [] [] None
  in
  let event2 =
    create_test_event 2 "Lunch" "12:00" "13:00" "Cafeteria" [] [] None
  in
  let day = create_test_day "2024-01-15" None [] [ event1; event2 ] in

  (* Find existing event *)
  (match get_event_by_id day 1 with
  | Some event ->
      assert_equal ~printer:string_of_int 1 event.id;
      assert_equal ~printer:(fun x -> x) "Meeting" event.name;
      assert_equal ~printer:(fun x -> x) "09:00" event.start_time
  | None -> assert_failure "Should have found event with ID 1");

  (* Non-existent ID *)
  match get_event_by_id day 99 with
  | Some _ -> assert_failure "Should not have found event with ID 99"
  | None -> ()

let test_add_todo =
  "Test add_todo" >:: fun _ ->
  let day = create_test_day "2024-01-15" None [] [] in

  (* Add first todo *)
  let day1 = add_todo day "Buy groceries" "Milk, eggs, bread" in
  assert_equal ~printer:string_of_int 1 (List.length day1.todos);

  let todo1 = List.hd day1.todos in
  assert_equal ~printer:string_of_int 1 todo1.id;
  assert_equal ~printer:(fun x -> x) "Buy groceries" todo1.name;
  assert_equal ~printer:(fun x -> x) "Milk, eggs, bread" todo1.description;
  assert_equal ~printer:string_of_bool false todo1.completed;

  (* Add second todo *)
  let day2 = add_todo day1 "Call mom" "Wish happy birthday" in
  assert_equal ~printer:string_of_int 2 (List.length day2.todos);

  let todo2 = List.nth day2.todos 1 in
  assert_equal ~printer:string_of_int 2 todo2.id;
  assert_equal ~printer:(fun x -> x) "Call mom" todo2.name;

  (* Other fields should remain unchanged *)
  assert_equal ~printer:(fun x -> x) day.date day2.date;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    day.special_day_name day2.special_day_name;
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.events day2.events

let test_toggle_todo =
  "Test toggle_todo" >:: fun _ ->
  let todo = create_test_todo 1 "Task" "Description" false in
  let day = create_test_day "2024-01-15" None [ todo ] [] in

  (* Toggle from false to true *)
  let toggled1 = toggle_todo day 1 in
  let toggled_todo1 = List.hd toggled1.todos in
  assert_equal ~printer:string_of_bool true toggled_todo1.completed;

  (* Toggle back to false *)
  let toggled2 = toggle_todo toggled1 1 in
  let toggled_todo2 = List.hd toggled2.todos in
  assert_equal ~printer:string_of_bool false toggled_todo2.completed;

  (* Toggle non-existent ID (should do nothing) *)
  let unchanged = toggle_todo day 99 in
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.todos unchanged.todos;

  (* Multiple todos *)
  let todo1 = create_test_todo 1 "Task 1" "Desc" false in
  let todo2 = create_test_todo 2 "Task 2" "Desc" true in
  let day_multi = create_test_day "2024-01-15" None [ todo1; todo2 ] [] in

  let toggled_multi = toggle_todo day_multi 1 in

  (* Use List.find on the todos list with proper typing *)
  let find_todo_by_id todos id =
    List.find (fun (t : todo) -> t.id = id) todos
  in

  let updated_todo1 = find_todo_by_id toggled_multi.todos 1 in
  let unchanged_todo2 = find_todo_by_id toggled_multi.todos 2 in

  assert_equal ~printer:string_of_bool true updated_todo1.completed;
  assert_equal ~printer:string_of_bool true unchanged_todo2.completed

let test_delete_todo =
  "Test delete_todo" >:: fun _ ->
  let todo1 = create_test_todo 1 "Task 1" "Desc" false in
  let todo2 = create_test_todo 2 "Task 2" "Desc" true in
  let todo3 = create_test_todo 3 "Task 3" "Desc" false in
  let day = create_test_day "2024-01-15" None [ todo1; todo2; todo3 ] [] in

  (* Delete middle todo *)
  let after_delete = delete_todo day 2 in
  assert_equal ~printer:string_of_int 2 (List.length after_delete.todos);

  (* Explicitly type the lambda function *)
  let remaining_ids = List.map (fun (t : todo) -> t.id) after_delete.todos in
  assert_bool "Should contain ID 1" (List.mem 1 remaining_ids);
  assert_bool "Should contain ID 3" (List.mem 3 remaining_ids);
  assert_bool "Should NOT contain ID 2" (not (List.mem 2 remaining_ids));

  (* Delete first todo *)
  let after_delete2 = delete_todo after_delete 1 in
  assert_equal ~printer:string_of_int 1 (List.length after_delete2.todos);
  assert_equal ~printer:string_of_int 3 (List.hd after_delete2.todos).id;

  (* Delete last todo *)
  let after_delete3 = delete_todo after_delete2 3 in
  assert_equal ~printer:string_of_int 0 (List.length after_delete3.todos);

  (* Delete non-existent ID (should do nothing) *)
  let unchanged = delete_todo day 99 in
  assert_equal ~printer:string_of_int 3 (List.length unchanged.todos)

let test_next_id =
  "Test next_id function" >:: fun _ ->
  (* Empty list *)
  let empty_list = [] in
  let get_id (x : todo) = x.id in
  assert_equal ~printer:string_of_int 1 (next_id empty_list get_id);

  (* List with gaps *)
  let todo1 = create_test_todo 3 "Task" "Desc" false in
  let todo2 = create_test_todo 7 "Task" "Desc" false in
  let todos = [ todo1; todo2 ] in
  assert_equal ~printer:string_of_int 8 (next_id todos get_id);

  (* Sequential IDs *)
  let todo3 = create_test_todo 1 "Task" "Desc" false in
  let todo4 = create_test_todo 2 "Task" "Desc" false in
  let todos_seq = [ todo3; todo4 ] in
  assert_equal ~printer:string_of_int 3 (next_id todos_seq get_id);

  (* Single element *)
  let single = [ create_test_todo 5 "Task" "Desc" false ] in
  assert_equal ~printer:string_of_int 6 (next_id single get_id);

  (* Test with events, need a separate get_id function for events *)
  let event1 = create_test_event 2 "Event" "09:00" "10:00" "Room" [] [] None in
  let event2 = create_test_event 4 "Event" "11:00" "12:00" "Room" [] [] None in
  let events = [ event1; event2 ] in
  let get_event_id (e : event) = e.id in
  assert_equal ~printer:string_of_int 5 (next_id events get_event_id)

let test_time_to_minutes =
  "Test time_to_minutes" >:: fun _ ->
  assert_equal ~printer:string_of_int 0 (time_to_minutes "00:00");
  assert_equal ~printer:string_of_int 60 (time_to_minutes "01:00");
  assert_equal ~printer:string_of_int 90 (time_to_minutes "01:30");
  assert_equal ~printer:string_of_int 720 (time_to_minutes "12:00");
  assert_equal ~printer:string_of_int 1439 (time_to_minutes "23:59");

  (* Test leading zeros *)
  assert_equal ~printer:string_of_int 5 (time_to_minutes "00:05");
  assert_equal ~printer:string_of_int 305 (time_to_minutes "05:05");

  (* Invalid format should fail *)
  try
    let _ = time_to_minutes "invalid" in
    assert_failure "Should have failed with invalid format"
  with Failure msg -> (
    assert_equal ~printer:(fun x -> x) "Invalid time format. Use HH:MM" msg;

    try
      let _ = time_to_minutes "25:00" in
      assert_failure "Should have failed with invalid hours"
    with _ -> ())

let test_sort_events_by_time =
  "Test sort_events_by_time" >:: fun _ ->
  let event1 =
    create_test_event 1 "Late event" "14:00" "15:00" "Room" [] [] None
  in
  let event2 =
    create_test_event 2 "Early event" "09:00" "10:00" "Room" [] [] None
  in
  let event3 =
    create_test_event 3 "Mid event" "10:30" "11:30" "Room" [] [] None
  in

  let unsorted = [ event1; event2; event3 ] in
  let sorted = sort_events_by_time unsorted in

  assert_equal ~printer:string_of_int 3 (List.length sorted);
  assert_equal ~printer:string_of_int 2 (List.nth sorted 0).id;
  (* Early *)
  assert_equal ~printer:string_of_int 3 (List.nth sorted 1).id;
  (* Mid *)
  assert_equal ~printer:string_of_int 1 (List.nth sorted 2).id;

  (* Late *)

  (* Test with same start time, different end time *)
  let event4 =
    create_test_event 4 "Short event" "09:00" "09:30" "Room" [] [] None
  in
  let event5 =
    create_test_event 5 "Long event" "09:00" "10:00" "Room" [] [] None
  in
  let sorted2 = sort_events_by_time [ event5; event4 ] in
  assert_equal ~printer:string_of_int 4 (List.nth sorted2 0).id;
  assert_equal ~printer:string_of_int 5 (List.nth sorted2 1).id;

  (* Single event *)
  let single = [ event1 ] in
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    single
    (sort_events_by_time single);

  (* Empty list *)
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    [] (sort_events_by_time [])

let test_add_event =
  "Test add_event" >:: fun _ ->
  let day = create_test_day "2024-01-15" None [] [] in

  (* Add simple event *)
  let event_data =
    ( "Meeting",
      "09:00",
      "10:00",
      "Office",
      [ "Alice"; "Bob" ],
      [ "alice@test.com" ],
      None )
  in
  let day1 = add_event day event_data in

  assert_equal ~printer:string_of_int 1 (List.length day1.events);
  let event = List.hd day1.events in
  assert_equal ~printer:string_of_int 1 event.id;
  assert_equal ~printer:(fun x -> x) "Meeting" event.name;
  assert_equal ~printer:(fun x -> x) "09:00" event.start_time;
  assert_equal ~printer:(fun x -> x) "10:00" event.end_time;
  assert_equal ~printer:(fun x -> x) "Office" event.location;
  assert_equal
    ~printer:(fun lst -> String.concat "; " lst)
    [ "Alice"; "Bob" ] event.invited_people;
  assert_equal
    ~printer:(fun lst -> String.concat "; " lst)
    [ "alice@test.com" ] event.email_addresses;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    None event.picture_path;

  (* Add event with picture, use a path that's safe for tests *)
  let test_picture_path = "test_image.jpg" in
  let event_data2 =
    ("Party", "20:00", "23:00", "Home", [], [], Some test_picture_path)
  in
  let day2 = add_event day1 event_data2 in

  assert_equal ~printer:string_of_int 2 (List.length day2.events);
  let event2 = List.nth day2.events 1 in
  assert_equal ~printer:string_of_int 2 event2.id;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    (Some test_picture_path) event2.picture_path;

  (* Events should be sorted by time *)
  let first_event = List.hd day2.events in
  assert_equal ~printer:string_of_int 1 first_event.id;

  (* Meeting should come before Party *)

  (* Other fields should remain unchanged *)
  assert_equal ~printer:(fun x -> x) day.date day2.date;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    day.special_day_name day2.special_day_name;
  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.todos day2.todos

let test_update_event =
  "Test update_event" >:: fun _ ->
  let event =
    create_test_event 1 "Old Name" "09:00" "10:00" "Old Room" [ "Alice" ]
      [ "alice@test.com" ] None
  in
  let day = create_test_day "2024-01-15" None [] [ event ] in

  (* Update all fields *)
  let updated =
    update_event day 1 ~name:"New Name" ~start_time:"10:00" ~end_time:"11:00"
      ~location:"New Room"
  in

  assert_equal ~printer:string_of_int 1 (List.length updated.events);
  let updated_event = List.hd updated.events in

  assert_equal ~printer:(fun x -> x) "New Name" updated_event.name;
  assert_equal ~printer:(fun x -> x) "10:00" updated_event.start_time;
  assert_equal ~printer:(fun x -> x) "11:00" updated_event.end_time;
  assert_equal ~printer:(fun x -> x) "New Room" updated_event.location;

  (* Other fields should remain unchanged *)
  assert_equal
    ~printer:(fun lst -> String.concat "; " lst)
    event.invited_people updated_event.invited_people;
  assert_equal
    ~printer:(fun lst -> String.concat "; " lst)
    event.email_addresses updated_event.email_addresses;
  assert_equal
    ~printer:(fun opt ->
      match opt with
      | Some s -> s
      | None -> "None")
    event.picture_path updated_event.picture_path;

  (* Update non-existent event (should do nothing) *)
  let unchanged =
    update_event day 99 ~name:"New" ~start_time:"00:00" ~end_time:"01:00"
      ~location:"Room"
  in

  assert_equal
    ~printer:(fun lst -> string_of_int (List.length lst))
    day.events unchanged.events;

  (* Multiple events, update one *)
  let event1 =
    create_test_event 1 "Event 1" "09:00" "10:00" "Room 1" [] [] None
  in
  let event2 =
    create_test_event 2 "Event 2" "11:00" "12:00" "Room 2" [] [] None
  in
  let day_multi = create_test_day "2024-01-15" None [] [ event1; event2 ] in

  let updated_multi =
    update_event day_multi 2 ~name:"Updated" ~start_time:"13:00"
      ~end_time:"14:00" ~location:"Room 3"
  in

  let unchanged_event = List.find (fun e -> e.id = 1) updated_multi.events in
  let changed_event = List.find (fun e -> e.id = 2) updated_multi.events in

  assert_equal ~printer:(fun x -> x) "Event 1" unchanged_event.name;
  assert_equal ~printer:(fun x -> x) "Updated" changed_event.name;
  assert_equal ~printer:(fun x -> x) "13:00" changed_event.start_time

let test_delete_event =
  "Test delete_event" >:: fun _ ->
  let event1 =
    create_test_event 1 "Meeting" "09:00" "10:00" "Office" [] [] None
  in
  let event2 =
    create_test_event 2 "Lunch" "12:00" "13:00" "Cafeteria" [] [] None
  in
  let event3 = create_test_event 3 "Call" "15:00" "15:30" "Phone" [] [] None in
  let day = create_test_day "2024-01-15" None [] [ event1; event2; event3 ] in

  (* Delete middle event *)
  let after_delete = delete_event day 2 in
  assert_equal ~printer:string_of_int 2 (List.length after_delete.events);

  let remaining_ids = List.map (fun e -> e.id) after_delete.events in
  assert_bool "Should contain ID 1" (List.mem 1 remaining_ids);
  assert_bool "Should contain ID 3" (List.mem 3 remaining_ids);
  assert_bool "Should NOT contain ID 2" (not (List.mem 2 remaining_ids));

  (* Delete first event *)
  let after_delete2 = delete_event after_delete 1 in
  assert_equal ~printer:string_of_int 1 (List.length after_delete2.events);
  assert_equal ~printer:string_of_int 3 (List.hd after_delete2.events).id;

  (* Delete last event *)
  let after_delete3 = delete_event after_delete2 3 in
  assert_equal ~printer:string_of_int 0 (List.length after_delete3.events);

  (* Delete non-existent event (should do nothing) *)
  let unchanged = delete_event day 99 in
  assert_equal ~printer:string_of_int 3 (List.length unchanged.events)

let test_send_invitations =
  "Test send_invitations function" >:: fun _ ->
  (* Test without attachment *)
  let exit_code1 =
    send_invitations ~recipient:"test@example.com" ~subject:"Test"
      ~body:"Test body" ~attachment:None
  in

  (* Check that it returns an integer *)
  assert_bool "Should return an integer exit code" (exit_code1 >= 0);

  (* Test with attachment, create a temporary file for testing *)
  let temp_file = Filename.temp_file "test_attachment" ".txt" in
  let oc = open_out temp_file in
  output_string oc "Test attachment content";
  close_out oc;

  let exit_code2 =
    send_invitations ~recipient:"test@example.com"
      ~subject:"Test with attachment" ~body:"Test body"
      ~attachment:(Some temp_file)
  in

  (* Clean up temp file *)
  Sys.remove temp_file;

  assert_bool "Should return an integer exit code" (exit_code2 >= 0)

let test_send_event_invitations =
  "Test send_event_invitations" >:: fun _ ->
  (* Event with no emails *)
  let event_no_emails =
    create_test_event 1 "Event" "09:00" "10:00" "Room" [] [] None
  in
  let result1 = send_event_invitations event_no_emails in
  assert_equal ~printer:string_of_int 0 (List.length result1);

  (* Event with emails *)
  let event_with_emails =
    create_test_event 2 "Meeting" "09:00" "10:00" "Office" [ "Alice"; "Bob" ]
      [ "alice@test.com"; "bob@test.com" ]
      None
  in
  let result2 = send_event_invitations event_with_emails in

  (* Should return list of (email, success) pairs *)
  assert_equal ~printer:string_of_int 2 (List.length result2);

  (* Verify email addresses *)
  let emails = List.map fst result2 in
  assert_bool "Should contain alice@test.com" (List.mem "alice@test.com" emails);
  assert_bool "Should contain bob@test.com" (List.mem "bob@test.com" emails);

  (* Event with picture attachment, create a temporary file *)
  let temp_pic = Filename.temp_file "test_picture" ".jpg" in
  let oc = open_out temp_pic in
  output_string oc "fake image data";
  close_out oc;

  let event_with_pic =
    create_test_event 3 "Party" "20:00" "23:00" "Home" [] [ "host@test.com" ]
      (Some temp_pic)
  in
  let result3 = send_event_invitations event_with_pic in

  (* Clean up temp file *)
  Sys.remove temp_pic;

  (* Should return results for the single email *)
  assert_equal ~printer:string_of_int 1 (List.length result3);

  (* Verify the email address *)
  let emails3 = List.map fst result3 in
  assert_bool "Should contain host@test.com" (List.mem "host@test.com" emails3)

(* This test just ensures the function can be called without raising
   exceptions *)
let test_setup_signal_handlers =
  "Test setup_signal_handlers doesn't crash" >:: fun _ ->
  try
    setup_signal_handlers ();
    ()
  with exn ->
    assert_failure
      (Printf.sprintf "setup_signal_handlers raised exception: %s"
         (Printexc.to_string exn))

let test_get_week_dates =
  "Test get_week_dates function" >:: fun _ ->
  let week_dates = get_week_dates () in
  assert_equal ~printer:string_of_int 7 (List.length week_dates);

  (* All dates should be valid *)
  List.iter
    (fun (year, month, day) ->
      assert_bool "Year should be reasonable" (year >= 2000 && year <= 2100);
      assert_bool "Month should be 1-12" (month >= 1 && month <= 12);
      assert_bool "Day should be positive" (day > 0);
      assert_bool "Day should be valid for month"
        (day <= days_in_month year month))
    week_dates;

  (* Days should be consecutive *)
  let rec check_consecutive = function
    | [] | [ _ ] -> ()
    | (y1, m1, d1) :: (y2, m2, d2) :: rest ->
        (* Calculate next day from first date *)
        let next_day, next_month, next_year =
          if d1 < days_in_month y1 m1 then (d1 + 1, m1, y1)
          else if m1 < 12 then (1, m1 + 1, y1)
          else (1, 1, y1 + 1)
        in
        assert_equal ~printer:string_of_int next_year y2;
        assert_equal ~printer:string_of_int next_month m2;
        assert_equal ~printer:string_of_int next_day d2;
        check_consecutive ((y2, m2, d2) :: rest)
  in
  check_consecutive week_dates;

  (* First day should be Monday of current week *)
  let start_date, _ =
    let year, month, day = get_today () in
    get_week_range year month day
  in

  let first_year, first_month, first_day = List.hd week_dates in
  let expected_start_date =
    Printf.sprintf "%04d-%02d-%02d" first_year first_month first_day
  in

  assert_equal ~printer:(fun x -> x) start_date expected_start_date;

  (* All days should be in the same week or span at most 2 months *)
  let unique_months =
    week_dates
    |> List.map (fun (_, month, _) -> month)
    |> List.sort_uniq compare |> List.length
  in
  assert_bool "Days should span at most 2 months" (unique_months <= 2);

  (* All days should be in the same year or span at most 2 years (across
     Dec/Jan) *)
  let unique_years =
    week_dates
    |> List.map (fun (year, _, _) -> year)
    |> List.sort_uniq compare |> List.length
  in
  assert_bool "Days should span at most 2 years" (unique_years <= 2);

  (* If we're in the middle of the week, today should be in the list *)
  let today_year, today_month, today_day = get_today () in
  let is_today_in_list =
    List.exists
      (fun (y, m, d) -> y = today_year && m = today_month && d = today_day)
      week_dates
  in
  assert_bool "Today should be in the week dates" is_today_in_list

let test_get_month_range_simple =
  "Test get_month_range simple cases" >:: fun _ ->
  (* Test basic 3 months *)
  let months = get_month_range 2024 1 3 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (2024, 1); (2024, 2); (2024, 3) ]
    months;

  (* Test year boundary crossing *)
  let months2 = get_month_range 2024 12 3 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (2024, 12); (2025, 1); (2025, 2) ]
    months2;

  (* Test single month *)
  let months3 = get_month_range 2024 5 1 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (2024, 5) ]
    months3;

  (* Test zero months *)
  let months4 = get_month_range 2024 1 0 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [] months4

let test_get_month_range_edge_cases =
  "Test get_month_range edge cases" >:: fun _ ->
  (* Negative count (should return empty list) *)
  let months1 = get_month_range 2024 1 (-5) in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [] months1;

  (* Large count spanning multiple years *)
  let months2 = get_month_range 2024 1 25 in
  assert_equal ~printer:string_of_int 25 (List.length months2);
  assert_equal
    ~printer:(fun (y, m) -> Printf.sprintf "%d-%02d" y m)
    (2024, 1) (List.hd months2);
  assert_equal
    ~printer:(fun (y, m) -> Printf.sprintf "%d-%02d" y m)
    (2026, 1) (List.nth months2 24);

  (* Starting from middle of year *)
  let months3 = get_month_range 2024 6 8 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [
      (2024, 6);
      (2024, 7);
      (2024, 8);
      (2024, 9);
      (2024, 10);
      (2024, 11);
      (2024, 12);
      (2025, 1);
    ]
    months3;

  (* Verify all months are valid (1-12) *)
  let months4 = get_month_range 2024 1 48 in
  List.iter
    (fun (year, month) ->
      assert_bool "Month should be 1-12" (month >= 1 && month <= 12))
    months4;

  (* Verify years increase correctly *)
  let months5 = get_month_range 2023 11 6 in
  let years = List.map fst months5 in
  let unique_years = List.sort_uniq compare years in
  assert_equal ~printer:string_of_int 2 (List.length unique_years);
  assert_bool "Should contain 2023" (List.mem 2023 unique_years);
  assert_bool "Should contain 2024" (List.mem 2024 unique_years)

let test_get_month_range_sequential =
  "Test get_month_range sequential validation" >:: fun _ ->
  (* Test that months are sequential *)
  let months = get_month_range 2024 1 12 in

  let rec check_sequential prev = function
    | [] -> ()
    | (y, m) :: rest ->
        (match prev with
        | Some (prev_y, prev_m) ->
            let expected_m = if prev_m = 12 then 1 else prev_m + 1 in
            let expected_y = if prev_m = 12 then prev_y + 1 else prev_y in
            assert_equal ~printer:string_of_int expected_y y;
            assert_equal ~printer:string_of_int expected_m m
        | None -> ());
        check_sequential (Some (y, m)) rest
  in
  check_sequential None months

let test_get_month_range_specific_dates =
  "Test get_month_range specific date combinations" >:: fun _ ->
  (* Leap year test *)
  let leap_year = get_month_range 2024 2 3 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (2024, 2); (2024, 3); (2024, 4) ]
    leap_year;

  (* Century year not divisible by 400 *)
  let century_year = get_month_range 1900 12 2 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (1900, 12); (1901, 1) ]
    century_year;

  (* Year divisible by 400 *)
  let year_400 = get_month_range 2000 11 4 in
  assert_equal
    ~printer:(fun lst ->
      List.map (fun (y, m) -> Printf.sprintf "%d-%02d" y m) lst
      |> String.concat ", ")
    [ (2000, 11); (2000, 12); (2001, 1); (2001, 2) ]
    year_400

(* Authentication tests *)
let auth_tests =
  "Authentication Tests"
  >::: [
         test_signup_success;
         test_signup_duplicate;
         test_login_success;
         test_login_wrong_password;
         test_login_nonexistent_user;
         test_load_users;
       ]

(* File/data handling tests *)
let file_tests =
  "File Handling Tests"
  >::: [
         test_ensure_data_folder;
         test_save_and_load_day;
         test_load_nonexistent_day;
       ]

(* Calendar utility tests *)
let calendar_util_tests =
  "Calendar Utility Tests"
  >::: [
         test_month_name;
         test_days_in_month;
         test_first_weekday;
         test_is_holiday;
         test_is_today;
         test_is_past_day;
         test_get_today;
         test_get_month_range_simple;
       ]

(* Day operation tests *)
let day_op_tests =
  "Day Operation Tests"
  >::: [ test_set_special_day_name; test_get_todo_by_id; test_get_event_by_id ]

(* Todo operation tests *)
let todo_tests =
  "Todo Operation Tests"
  >::: [ test_add_todo; test_toggle_todo; test_delete_todo; test_next_id ]

(* Event operation tests *)
let event_tests =
  "Event Operation Tests"
  >::: [
         test_time_to_minutes;
         test_sort_events_by_time;
         test_add_event;
         test_update_event;
         test_delete_event;
         test_get_month_range_edge_cases;
       ]

(* Invitation tests *)
let invitation_tests =
  "Invitation Tests" >::: [ test_send_invitations; test_send_event_invitations ]

(* Edge case tests *)
let edge_case_tests =
  "Edge Case Tests" >::: [ test_setup_signal_handlers; test_get_week_dates ]

(* Main test suite *)
let test_suite =
  "Calendar Library Test Suite"
  >::: [
         auth_tests;
         file_tests;
         calendar_util_tests;
         day_op_tests;
         todo_tests;
         event_tests;
         invitation_tests;
         edge_case_tests;
       ]

(* Run all tests *)
let () =
  (* Setup before running tests *)
  if Sys.file_exists "data" then ignore (Sys.command "rm -rf data");
  if Sys.file_exists "user" then ignore (Sys.command "rm -rf user");

  run_test_tt_main test_suite
