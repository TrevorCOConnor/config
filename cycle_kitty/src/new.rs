use std::{fs, collections::hash_map};
use std::collections::HashMap;

use colors_transform::{Rgb, Color};
use cycle_kitty;

struct ColorBuilder(Rgb);

impl ColorBuilder {
    fn new() -> ColorBuilder {
        ColorBuilder(Rgb::new())
    }

    fn from_hex(hex: &str) -> ColorBuilder {
        ColorBuilder(
            Rgb::from_hex_str(&hex)
                .expect("Invalid hex string")
        )
    }

    fn to_hex(&self) -> String {
        self.0.to_css_hex_string()
    }

    fn apply_red(&mut self, r: f32) {
        self.0 = self.0.set_red(r)
    }

    fn apply_green(&mut self, g: f32) {
        self.0 = self.0.set_green(g)
    }

    fn apply_blue(&mut self, b: f32) {
        self.0 = self.0.set_blue(b)
    }
}

struct ThemeSchematic {
    name: String,
    background: ColorBuilder,
    foreground: ColorBuilder,
    cursor: ColorBuilder,
    selection_background: ColorBuilder,
    selection_foreground: ColorBuilder,
}

impl ThemeSchematic {
    fn new(name: String, theme: cycle_kitty::Theme) -> Result<ThemeSchematic, String> {
        Ok(
            ThemeSchematic {
                name,
                background: ColorBuilder::new(),
                foreground: ColorBuilder::new(),
                cursor: ColorBuilder::new(),
                selection_background: ColorBuilder::new(),
                selection_foreground: ColorBuilder::new()
            }
        )
    }

    fn read_from_file(fp: String) -> ThemeSchematic {
        let contents = fs::read_to_string(fp).unwrap();
        let lines: Vec<(&str, &str)> = contents
            .lines()
            .map(|x| {
                let (key, value) = x.trim().split_once(" ").expect("conf file has wrong format");
                (key, value.trim())
            })
            .collect();

        let map: HashMap<&str, &str> = lines.into_iter().collect();
        ThemeSchematic {
            name: String::from("new"),
            background: ColorBuilder::from_hex(map[&"background"]),
            foreground: ColorBuilder::from_hex(map[&"foreground"]),
            cursor: ColorBuilder::from_hex(map[&"cursor"]),
            selection_background: ColorBuilder::from_hex(map[&"selection_background"]),
            selection_foreground: ColorBuilder::from_hex(map[&"selection_foreground"]),
        }
    }

    fn export(&self) {
        let contents = format!(
            "background {}
            foreground {}
            cursor {self.cursor.to_hex()}
            selection_foreground {self.selection_foreground()}
            selection_background {self.selection_background()}
            ",
            self.background.to_hex()
        );
        let path = cycle_kitty::create_path(&self.name);
        fs::write(path, contents).unwrap()
    }
}
