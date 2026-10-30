# CamlCalendar

**Your whole schedule, one keystroke away, right in the terminal.**

## 💡 What is CamlCalendar?

Most calendar apps want you to leave the command line, open a browser, and click through menus just to add a 2 pm meeting. CamlCalendar is a terminal-based calendar written in OCaml that keeps you in your shell. It's for students, developers, and anyone who lives in a terminal and wants to plan their days without a GUI.

You sign up or log in, then see a color-coded monthly calendar. Today is red, holidays are blue, and past days are gray. Type a day number to open it. There you can set a special-day name, manage a to-do list, and create, edit, or delete timed events. You can also jump to the weekly or yearly views, or move between months with `n` and `p`. Everything is saved per user as JSON, so your schedule is there next time you log in. Events can also email invitations to guests.

It's technically interesting because the logic is separated from the interface. The event and to-do operations are pure functions that take a day record and return an updated one, with no hidden mutation. The interactive terminal code is a thin layer on top. The project also uses OCaml's module system, interface files, and a typed data model, and it has a large unit-test suite.

## ✨ Key Features

- **Monthly view.** A Monday-first grid with a color legend and month-to-month navigation.
- **Weekly and yearly views.** The weekly view shows the current week with its events and to-dos, and lets you pick a day to edit. The yearly view shows twelve months in rows of three and lets you jump to a date.
- **Events, to-dos, and special days.** Add, edit, and delete events (auto-sorted by time, with end-before-start rejected on creation). Check off to-dos, and label any day.
- **Accounts and JSON persistence.** Sign up, log in, and sign out. Each user's days are saved as `data/<username>/YYYY-MM-DD.json`.
- **Email invitations.** Events with email addresses send invitations through `mutt`, with the optional picture attached and per-recipient success reporting.

## 🛠️ Tech Stack

| Area | What's used |
|---|---|
| Language | OCaml |
| Build | Dune 3.20 |
| Serialization | `yojson` |
| Date and system | `unix` library, `Sys.command`, signal handling |
| Email | External `mutt` CLI |
| Testing | `ounit2`, with `bisect_ppx` instrumentation |
| Formatting | `ocamlformat` |
| UI | ANSI escape codes, with no TUI library |


## 👥 Team

- Yala Huang Feng (yh2293)
- Audrey Dequito (amd466)
- Daisy Lin (xl972)
- Zahin Tasnin (zt276)
- Sophia Zhen (xz876)
