use mdbook::book::Book;
use mdbook::errors::Error;
use mdbook::preprocess::{Preprocessor, PreprocessorContext};
use mdtranslation::pulldown_cmark::{self, Parser};
use mdtranslation::roundtrip::push_markdown;
use mdtranslation::translation::{translate_ext, TranslationOptions};

const PREPROCESSOR_NAME: &str = "mdbook-translation";

pub(crate) struct TranslationPreprocessor {}

impl TranslationPreprocessor {
    pub(crate) fn new() -> Self {
        TranslationPreprocessor {}
    }
}

impl Preprocessor for TranslationPreprocessor {
    fn name(&self) -> &str {
        PREPROCESSOR_NAME
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
        let source_dir = ctx.root.join(&ctx.config.book.src);
        let input_path = if let Ok(path) = std::env::var("MDTRANSLATION_INPUT") {
            source_dir.join(path)
        } else {
            eprintln!(
                "{}: MDTRANSLATION_INPUT is not set, skipping.",
                PREPROCESSOR_NAME
            );
            return Ok(book);
        };
        let lang = if let Ok(lang) = std::env::var("MDTRANSLATION_LANG") {
            lang
        } else {
            eprintln!(
                "{}: MDTRANSLATION_LANG is not set, skipping.",
                PREPROCESSOR_NAME
            );
            return Ok(book);
        };

        let input_data = match std::fs::read_to_string(&input_path) {
            Ok(data) => data,
            Err(e) => {
                eprintln!(
                    "{}: failed to open `{}`.",
                    PREPROCESSOR_NAME,
                    input_path.display()
                );
                return Err(e.into());
            }
        };
        let input_events = Parser::new(&input_data).collect::<Vec<_>>();
        let default_lang = if let Ok(lang) = std::env::var("MDTRANSLATION_DEFAULT_LANG") {
            lang
        } else {
            let lang = "en_US";
            eprintln!(
                "{}: MDTRANSLATION_DEFAULT_LANG is not set, defaulting to `{}`.",
                PREPROCESSOR_NAME, lang
            );
            lang.to_string()
        };

        let parse_options = pulldown_cmark::Options::empty();

        let translation_options = TranslationOptions {
            extract_link_contents: true,
            ignore_duplicate_items: true,
        };

        book.for_each_mut(|item| match item {
            mdbook::BookItem::Chapter(chapter) => {
                let content_events = Parser::new_ext(&chapter.content, parse_options);
                let mut new_contents = String::new();
                if translate_ext(
                    content_events,
                    input_events.iter().cloned(),
                    &lang,
                    Some(&default_lang),
                    translation_options.clone(),
                )
                .map(|translated| push_markdown(&mut new_contents, translated))
                .is_err()
                {
                    eprintln!("{}: Failed to process chapter.", PREPROCESSOR_NAME);
                } else {
                    chapter.content = new_contents;
                }

                let title_events = Parser::new_ext(&chapter.name, parse_options);
                let mut new_contents = String::new();
                if translate_ext(
                    title_events,
                    input_events.iter().cloned(),
                    &lang,
                    Some(&default_lang),
                    translation_options.clone(),
                )
                .map(|translated| push_markdown(&mut new_contents, translated))
                .is_err()
                {
                    eprintln!("{}: Failed to process chapter name.", PREPROCESSOR_NAME);
                } else {
                    chapter.name = new_contents;
                }
            }
            mdbook::BookItem::Separator => {}
            mdbook::BookItem::PartTitle(title) => {
                let title_events = Parser::new_ext(title, parse_options);
                let mut new_contents = String::new();
                if translate_ext(
                    title_events,
                    input_events.iter().cloned(),
                    &lang,
                    Some(&default_lang),
                    translation_options.clone(),
                )
                .map(|translated| push_markdown(&mut new_contents, translated))
                .is_err()
                {
                    eprintln!("{}: Failed to process chapter.", PREPROCESSOR_NAME);
                } else {
                    *title = new_contents;
                }
            }
        });

        Ok(book)
    }

    fn supports_renderer(&self, _renderer: &str) -> bool {
        true
    }
}
