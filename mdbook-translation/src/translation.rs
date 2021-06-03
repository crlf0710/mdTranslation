use mdbook::book::Book;
use mdbook::errors::Error;
use mdbook::preprocess::{Preprocessor, PreprocessorContext};
use mdtranslation::roundtrip::push_markdown;
use mdtranslation::translation::translate;
use pulldown_cmark::Parser;

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

        book.for_each_mut(|item| match item {
            mdbook::BookItem::Chapter(chapter) => {
                let content_events = Parser::new(&chapter.content);
                let mut new_contents = String::new();
                if translate(
                    content_events,
                    input_events.iter().cloned(),
                    &lang,
                    Some(&default_lang),
                )
                .map(|translated| push_markdown(&mut new_contents, translated))
                .is_err()
                {
                    eprintln!("{}: Failed to process chapter.", PREPROCESSOR_NAME);
                    return;
                }
                chapter.content = new_contents;
            }
            mdbook::BookItem::Separator => {}
            mdbook::BookItem::PartTitle(_) => {}
        });

        Ok(book)
    }

    fn supports_renderer(&self, _renderer: &str) -> bool {
        true
    }
}
