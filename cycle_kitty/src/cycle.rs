use colored::Colorize;

use console::Term;
use cycle_kitty::Theme;


fn find_kitty_theme_in_list(theme_name: &str, themes: &[Theme]) -> usize {
    let theme_name = theme_name.trim_end_matches(".conf");
    for i in 0..themes.len() {
        if themes[i].theme_name() == theme_name {
            return i
        }
    }
    0
}

pub struct Cycle {
    original: usize,
    current: usize,
    length: usize,
    themes: Vec<Theme>,
    query: String,
    filtered_themes: Vec<usize>
}

impl Cycle {
    pub fn new(
        original: usize, themes: Vec<Theme>
    ) -> Cycle {
        let ref_themes = (0..themes.len()).collect();
        Cycle {
            original,
            current: original,
            length: themes.len(),
            themes,
            query: String::from(""),
            filtered_themes: ref_themes
        }
    }

    pub fn new_from_name(name: &str, themes: Vec<Theme>) -> Cycle {
        let original = find_kitty_theme_in_list(name, &themes);
        Cycle::new(original, themes)
    }

    pub fn query_del(&mut self) {
        if !self.query.is_empty() {
            self.query.pop();
            self.update_themes();
        }
    }

    pub fn append_query(&mut self, c: &char) {
        self.query.push(c.to_owned());
        let updated = self.update_themes();
        if !updated {
            self.query.pop();
        }
    }

    fn update_themes(&mut self) -> bool {
        // Attempts to update themes.
        // Returns True if updates are non trivial
        let new: Vec<usize> = self.themes.iter().enumerate().filter(
            |(_, t)| t.theme_name().to_uppercase().starts_with(
                &self.query.to_uppercase()
            )
        ).map(|(e, _)| e).collect();
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
    }

    #[allow(dead_code)]
    pub fn next_max(&mut self) {
        if self.current + 1 < self.length {
            self.current += 1;
        }
    }

    pub fn prev_wrap(&mut self) {
        if self.current == 0 {
            self.current = self.length - 1;
        } else {
            self.current -= 1;
        }
    }

    #[allow(dead_code)]
    pub fn prev_max(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    fn get_theme(&self, index: usize) -> &Theme {
        let inner_index = self.filtered_themes.get(index)
            .expect(&format!("Index {} not found", self.current));
        self.themes.get(*inner_index)
            .expect(&format!("Inner index not found."))
    }

    pub fn current_theme(&self) -> &Theme {
        self.get_theme(self.current)
    }

    pub fn original_theme(&self) -> &Theme {
        &self.themes.get(self.original)
            .expect(&format!("Index {} not found", self.current))
    }

    pub fn display_selection(&self, term: &Term) {
        // reserve 1 for the current
        // reserve 1 for the cursor
        let max_display = (term.size().0 - 2) as usize;
        let buffer = max_display / 2;
        let min = self.current.checked_sub(buffer).unwrap_or(0);
        let max = self.length.min(max_display + min);
        for i in min..self.current {
            term.clear_line().unwrap();
            println!("{}", self.get_theme(i).theme_name());
        }
        term.clear_line().unwrap();
        println!("{}", self.current_theme().theme_name().black().on_color("#fffacd"));
        for i in (self.current + 1)..max {
            term.clear_line().unwrap();
            println!("{}", self.get_theme(i).theme_name());
        }
        term.clear_line().unwrap();
        println!("{}", self.query);
        term.move_cursor_up(2).unwrap();
    }
}
