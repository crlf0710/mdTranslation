use mdtranslation::pulldown_cmark;
use mdtranslation::translation;
use structopt::StructOpt;

use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "mdtranslation_translate",
    about = "A tool to translate source document into localized document."
)]
struct Opt {
    /// Language
    #[structopt(short, long)]
    language: String,

    /// Default language
    #[structopt(short, long)]
    default_language: Option<String>,

    #[structopt(short, long)]
    translation: PathBuf,

    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output file, stdout if not present
    #[structopt(parse(from_os_str))]
    output: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();
    let input = std::fs::read_to_string(opt.input)?;
    let events = pulldown_cmark::Parser::new(&input);
    let translation = std::fs::read_to_string(opt.translation)?;
    let translation_events = pulldown_cmark::Parser::new(&translation);
    let output: Box<dyn std::io::Write> = match opt.output {
        Some(filename) => Box::new(std::fs::File::create(filename)?),
        _ => Box::new(std::io::stdout()),
    };
    let language = opt.language;
    let default_lang = opt.default_language;
    let iter = translation::translate(
        events,
        translation_events,
        language.as_ref(),
        default_lang.as_ref().map(|x| x.as_ref()),
    )?;
    pulldown_cmark::html::write_html(output, iter)?;
    Ok(())
}
