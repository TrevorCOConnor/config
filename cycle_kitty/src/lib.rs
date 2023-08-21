use std::fs::{self, DirEntry};
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
        .trim_end_matches(".conf")
        .to_string()
}

pub fn get_themes() -> Vec<Theme> {
    let kitty_dir = get_kitty_dir();
    let dirs: Vec<DirEntry> = fs::read_dir(kitty_dir)
        .unwrap()
        .map(|entry| entry.unwrap())
        .collect();
    let mut themes: Vec<Theme> = dirs.into_iter().map(|d| Theme(d)).collect();
    themes.sort_by_key(|t| t.theme_name().to_lowercase());
    themes
}

fn hex_to_int(hex_str: &str) -> u32 {
    // Converts hex color to 24-bit int
    // red
    let red = &hex_str[1..=2];
    let red_val = u32::from_str_radix(red, 16).unwrap();

    // green
    let green = &hex_str[3..=4];
    let green_val = u32::from_str_radix(green, 16).unwrap();

    // blue
    let blue = &hex_str[5..=6];
    let blue_val = u32::from_str_radix(blue, 16).unwrap();

    // int
    (red_val << 16) | (green_val << 8) | blue_val
}

pub fn set_theme_from_name(file_name: &str) {
    // Update current kitty
    let name = format!("{}.conf", &file_name);
    let kitty_file = get_kitty_file();

    fs::write(
        kitty_file,
        &name
    ).expect("Failed to set theme");

    // Update colors
    let path = create_path(file_name);
    let contents = fs::read_to_string(path)
        .expect("Color file not found");
    let mut data = json::JsonValue::new_object();
    for line in contents.lines() {
        if !(line.is_empty() || line.starts_with("#")) {
            let (key, value) = line.split_once(" ").unwrap();
            data[key] = hex_to_int(value.trim()).into();
        }
    }
    let payload = json::object!{
        cmd: "set-colors",
        version: [0,21,2],
        no_response: true,
        payload: {
            colors: data,
            all: true,
        }
    };
    let cmd = format!("\x1bP@kitty-cmd{}\x1b\\", payload.dump());
    println!("{}", cmd);
}


fn create_path(name: &str) -> String {
    let kitty_dir = get_kitty_dir();
    format!("{kitty_dir}{name}.conf")
}
