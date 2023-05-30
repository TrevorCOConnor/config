use colored::Colorize;

use console::Term;
use cycle_kitty::{set_theme, Theme};


pub fn find_kitty_theme_in_list(theme_name: &str, themes: &[Theme]) -> usize {
    for i in 0..themes.len() {
        if themes[i].theme_name() == theme_name {
            return i
        }
    }
    0
}

pub struct Cycle<'a> {
    original: usize,
    current: usize,
    length: usize,
    themes: &'a Vec<Theme>,
    query: String,
    filtered_themes: Vec<&'a Theme>
}


impl<'a> Cycle<'a> {
    pub fn new(
        original: usize, themes: &Vec<Theme>
    ) -> Cycle {
        let ref_themes = themes.iter().collect();
        Cycle {
            original,
            current: original,
            length: themes.len(),
            themes,
            query: String::from(""),
            filtered_themes: ref_themes
        }
    }

    pub fn new_from_name(name: &str, themes: &'a Vec<Theme>) -> Cycle<'a> {
        let original = find_kitty_theme_in_list(name, &themes);
        Cycle::new(original, themes)
    }

    pub fn query_del(&mut self) {
        if !self.query.is_empty() {
            self.query.pop();
            self.update_themes();
            self.apply_current_theme()
        }
    }

    pub fn append_query(&mut self, c: char) {
        self.query.push(c);
        if self.update_themes() {
            self.apply_current_theme()
        } else {
            self.query.pop();
        }
    }

    fn update_themes(&mut self) -> bool {
        let new: Vec<&Theme> = self.themes.iter().filter(
            |t| t.theme_name().to_uppercase().starts_with(
                &self.query.to_uppercase()
            )
        ).collect();
        if new.is_empty() {
            false
        } else {
            self.filtered_themes = new;
            self.length = self.filtered_themes.len();
            self.current = 0;
            true
        }
    }

    pub fn next_wrap(&mut self) {
        if self.current + 1 >= self.length {
            self.current = 0;
        } else {
            self.current += 1;
        }

        self.apply_current_theme()
    }

    #[allow(dead_code)]
    pub fn next_max(&mut self) {
        if self.current + 1 < self.length {
            self.current += 1;
        }

        self.apply_current_theme()
    }

    pub fn prev_wrap(&mut self) {
        if self.current == 0 {
            self.current = self.length - 1;
        } else {
            self.current -= 1;
        }

        self.apply_current_theme()
    }

    #[allow(dead_code)]
    pub fn prev_max(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }

        self.apply_current_theme()
    }

    fn get_theme(&self, index: usize) -> &Theme {
        &self.filtered_themes.get(index)
            .expect(&format!("Index {} not found", self.current))
    }

    fn current_theme(&self) -> &Theme {
        self.get_theme(self.current)
    }

    fn original_theme(&self) -> &Theme {
        &self.themes.get(self.original)
            .expect(&format!("Index {} not found", self.current))
    }

    fn apply_current_theme(&self) {
        set_theme(&self.current_theme())
    }

    pub fn apply_original_theme(&self) {
        set_theme(&self.original_theme())
    }

    pub fn display_selection(&self, term: &Term) {
        let max_display = term.size().0 as usize;
        let buffer = max_display / 2;
        let min = self.current.checked_sub(buffer).unwrap_or(0);
        let max = self.length.min(max_display + min + 1);
        for i in min..self.current {
            println!("{}", self.get_theme(i).theme_name());
        }
        println!("{}", self.current_theme().theme_name().underline());
        for i in (self.current + 1)..max {
            println!("{}", self.get_theme(i).theme_name());
        }
        println!("{}", self.query);
        term.move_cursor_up(1).unwrap();
        term.move_cursor_right(self.query.len()).unwrap();
    }
}
