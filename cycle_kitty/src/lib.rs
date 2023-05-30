use std::fs::{self, DirEntry};
use std::process::Command;
use std::env;

const KITTY_FILE: &str = ".config/kitty/current_theme.txt";
const KITTY_DIR: &str = ".config/kitty/kitty-themes/themes/";

pub struct Theme(pub DirEntry);

impl Theme {
    pub fn theme_name(&self) -> String {
        self.0.file_name()
            .into_string()
            .unwrap()
            .strip_suffix(".conf")
            .unwrap()
            .to_string()
    }
}

fn get_kitty_dir() -> String {
    let home = env::var("HOME").expect("$HOME not set");
    format!("{}/{}", home, KITTY_DIR)
}

pub fn get_kitty_file() -> String {
    let home = env::var("HOME").expect("$HOME not set");
    format!("{}/{}", home, KITTY_FILE)
}

pub fn get_themes() -> Vec<Theme> {
    let kitty_dir = get_kitty_dir();
    let dirs: Vec<DirEntry> = fs::read_dir(kitty_dir)
        .unwrap()
        .map(|entry| entry.unwrap())
        .collect();
    let mut themes: Vec<Theme> = dirs.into_iter().map(|d| Theme(d)).collect();
    themes.sort_by_key(|t| t.theme_name());
    themes
}

pub fn set_theme(theme: &Theme) {
    let kitty_file = get_kitty_file();

    fs::write(
        kitty_file,
        theme.theme_name(),
    ).expect("Failed to set theme");

    Command::new("kitty")
            .args([
                "@",
                "set-colors",
                "-a",
                format!("{}", theme.0.path().to_str().unwrap()).as_str()
            ])
            .output()
            .expect("Kitty command failed");
}
