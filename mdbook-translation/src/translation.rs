use mdbook::book::Book;
use mdbook::errors::Error;
use mdbook::preprocess::{Preprocessor, PreprocessorContext};
use mdtranslation::roundtrip::push_markdown;
use mdtranslation::translation::translate;
use pulldown_cmark::Parser;

pub(crate) struct TranslationPreprocessor {}

impl TranslationPreprocessor {
    pub(crate) fn new() -> Self {
        TranslationPreprocessor {}
    }
}

impl Preprocessor for TranslationPreprocessor {
    fn name(&self) -> &str {
        "mdbook-translation"
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
        let source_dir = ctx.root.join(&ctx.config.book.src);
        let input_path = if let Some(path) = std::env::var("MDTRANSLATION_INPUT").ok() {
            source_dir.join(path)
        } else {
            eprintln!(
                "{}: MDTRANSLATION_INPUT is not set, skipping.",
                "mdbook-translation"
            );
            return Ok(book);
        };
        let lang = if let Some(lang) = std::env::var("MDTRANSLATION_LANG").ok() {
            lang
        } else {
            eprintln!(
                "{}: MDTRANSLATION_LANG is not set, skipping.",
                "mdbook-translation"
            );
            return Ok(book);
        };

        let input_data = std::fs::read_to_string(input_path)?;
        let input_events = Parser::new(&input_data).collect::<Vec<_>>();
        let default_lang = if let Some(lang) = std::env::var("MDTRANSLATION_DEFAULT_LANG").ok() {
            lang
        } else {
            let lang = "en_US";
            eprintln!(
                "{}: MDTRANSLATION_DEFAULT_LANG is not set, defaulting to `{}`.",
                "mdbook-translation", lang
            );
            lang.to_string()
        };

        book.for_each_mut(|item| match item {
            mdbook::BookItem::Chapter(chapter) => {
                let content_events = Parser::new(&chapter.content);
                let mut new_contents = String::new();
                if let Err(_) = translate(
                    content_events,
                    input_events.iter().cloned(),
                    &lang,
                    Some(&default_lang),
                )
                .and_then(|translated| {
                    push_markdown(&mut new_contents, translated);
                    Ok(())
                }) {
                    eprintln!("{}: Failed to process chapter.", "mdbook-translation");
                    return;
                }
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
