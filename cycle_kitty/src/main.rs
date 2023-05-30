use std::fs;
use console::{Term, Key};

mod cycle;
use cycle::Cycle;
pub use cycle_kitty;


fn main() {
    let themes = cycle_kitty::get_themes();
    let current_theme = fs::read_to_string(cycle_kitty::get_kitty_file());

    let term = Term::stdout();

    let mut cycle = {
        if let Ok(theme) = current_theme {
            Cycle::new_from_name(&theme, &themes)
        } else {
            Cycle::new(0, &themes)
        }
    };

    loop {
        term.clear_screen().unwrap();
        cycle.display_selection(&term);
        let key = term.read_key().expect("Invalid keystroke");
        match key {
            Key::Escape => {
                cycle.apply_original_theme();
                break;
            },
            Key::Enter => {break;},
            Key::ArrowUp => cycle.prev_wrap(),
            Key::ArrowDown => cycle.next_wrap(),
            Key::Backspace => cycle.query_del(),
            Key::Char(c) => cycle.append_query(c),
            _ => {}
        }
    }
}
