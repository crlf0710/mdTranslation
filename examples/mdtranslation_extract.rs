use mdtranslation::extraction;
use pulldown_cmark;
use structopt::StructOpt;

use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "mdtranslation_extract",
    about = "A tool to generate initial content for translation."
)]
struct Opt {
    /// Default language
    #[structopt(short, long)]
    language: Option<String>,

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
    let mut output: Box<dyn std::io::Write> = match opt.output {
        Some(filename) => Box::new(std::fs::File::create(filename)?),
        _ => Box::new(std::io::stdout()),
    };
    let language = opt.language;
    extraction::extract(events, &mut output, language.as_ref().map(|x| x.as_ref()))?;
    Ok(())
}
