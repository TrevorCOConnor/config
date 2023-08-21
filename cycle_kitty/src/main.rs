use std::{fs, thread::spawn, sync::{Arc, Mutex}};
use console::{Term, Key};

mod cycle;

use cycle::Cycle;
pub use cycle_kitty;
use cycle_kitty::set_theme_from_name;

fn main() {
    let themes = cycle_kitty::get_themes();
    let current_theme = fs::read_to_string(cycle_kitty::get_kitty_file());

    let cycle = {
        if let Ok(theme) = current_theme {
            Cycle::new_from_name(&theme, themes)
        } else {
            Cycle::new(0, themes)
        }
    };

    let arc = Arc::new(Mutex::new(cycle));
    let term = Term::stdout();
    term.clear_screen().unwrap();
    {
        arc.lock().unwrap().display_selection(&term);
    }

    loop {
        let key = term.read_key().expect("Invalid keystroke");
        match key {
            Key::Escape => {
                let og = arc
                    .lock()
                    .unwrap()
                    .original_theme()
                    .theme_name();
                set_theme_from_name(&og);
                break;
            },
            Key::Enter => {break;},
            _ => {
                let arc = Arc::clone(&arc);
                spawn(move || {
                    let name = {
                        let mut cycle = arc.lock().unwrap();
                        match key {
                            Key::ArrowUp => cycle.prev_wrap(),
                            Key::ArrowDown => cycle.next_wrap(),
                            Key::Backspace => cycle.query_del(),
                            Key::Char(c) => cycle.append_query(&c),
                            _ => {}
                        };
                        Term::stdout().clear_screen().unwrap();
                        cycle.display_selection(&Term::stdout());
                        cycle.current_theme().theme_name()
                    };
                    set_theme_from_name(&name);
                });
            },
        }
    }
}
