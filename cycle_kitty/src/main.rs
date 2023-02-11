use std::fs::{self, DirEntry, File};
use std::process::Command;
use std::env;
use console::Term;

#[derive(Debug)]
enum Direction {
    Up,
    Down
}

const KITTY_FILE: &str = ".config/kitty/current_theme.txt";
const KITTY_DIR: &str = ".config/kitty/kitty-themes/themes/";


fn get_kitty_dir() -> String {
    let home = env::var("HOME").expect("$HOME not set");
    format!("{}/{}", home, KITTY_DIR)
}

fn get_kitty_file() -> String {
    let home = env::var("HOME").expect("$HOME not set");
    format!("{}/{}", home, KITTY_FILE)
}

    fn get_current_kitty_theme(themes: &Vec<DirEntry>) -> String {
    let kitty_file = &get_kitty_file();
    let current_theme = fs::read_to_string(kitty_file);

    match current_theme {
        Ok(theme) => theme,
        _ => {
            File::create(kitty_file).expect("Failed to create theme file");
            get_default_theme(themes, Direction::Down).file_name().into_string().unwrap()
        },
    }
}

fn entry_shares_file_name(dir_entry: &DirEntry, file_name: &String) -> bool {
    &dir_entry.file_name().into_string().unwrap() == file_name
}

fn get_themes() -> Vec<DirEntry> {
    let kitty_dir = get_kitty_dir();
    fs::read_dir(kitty_dir)
        .unwrap()
        .map(|entry| entry.unwrap())
        .collect()
}

fn get_next_theme(themes: &Vec<DirEntry>, previous_theme: String, direction: Direction) -> &DirEntry {
    let next_theme = match direction {
        Direction::Up => {
            let new = themes.into_iter()
                            .take_while(|dir_entry| !entry_shares_file_name(dir_entry, &previous_theme));
            new.last()
        },
        Direction::Down => {
            themes.into_iter()
            .skip_while(|dir_entry| !entry_shares_file_name(dir_entry, &previous_theme))
            .nth(1)
        }
    };

    match next_theme {
        Some(dir_entry) => dir_entry,
        None => get_default_theme(themes, direction),
    }
}

fn get_default_theme(themes: &Vec<DirEntry>, direction: Direction) -> &DirEntry {
    match direction {
        Direction::Up => themes.into_iter().last().unwrap(),
        Direction::Down => themes.into_iter().next().unwrap(),
    }
}

fn set_theme(theme: &DirEntry) {
    let kitty_file = get_kitty_file();
    let file_name = theme.file_name();
    let file_name_str = file_name.to_str().unwrap();

    println!("{}", file_name_str);

    fs::write(
        kitty_file,
        file_name_str,
    ).expect("Failed to set theme");

    Command::new("kitty")
            .args([
                "@",
                "set-colors",
                "-a",
                format!("{}", theme.path().to_str().unwrap()).as_str()
            ])
            .output()
            .expect("Kitty command failed");
}

fn map_input_to_direction(input: char) -> Option<Direction> {
    match input {
        'j' => Some(Direction::Down),
        'k' => Some(Direction::Up),
        'n' => Some(Direction::Down),
        'e' => Some(Direction::Up),
        _ => None
    }
}

fn main() {
    let themes = &get_themes();
    let term = Term::stdout();
    let mut current_theme = get_current_kitty_theme(themes);
    loop {
        let next = term.read_char();
        if let Ok(c) = next {
            match c {
                'q' => {break;},
                _ => {
                    let direction = map_input_to_direction(c);
                    if let Some(dir) = direction {
                        let current_dir_entry = get_next_theme(
                            themes,
                            current_theme,
                            dir
                        );
                        current_theme = current_dir_entry.file_name().into_string().unwrap();
                        set_theme(current_dir_entry);
                    }
                }
            }
        }
    }
}
