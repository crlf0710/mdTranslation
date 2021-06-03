# mdTranslation

mdTranslation is a utility to prepare multi-lingual Markdown documents.

There's also a mdBook preprocessor called [`mdbook-translation`](./mdbook-translation) for working with mdBook books.

# How does it achieve this?

This is done by associating source contents and their translations together.
This association is expressed as a separate Markdown documents within a specific simple format.

This file looks like this:
````text
> Original content 1

* lang_ID1
Translated content written in lang_ID1
* lang_ID2
Translated content written in lang_ID2
****
...
````

This library can read and compare Markdown contents within each leaf node (paragraphs, list items, etc)
to all the original contents here, when there's a match, it will replace it with the corresponding translated content. This makes it possible to do translation without touching the original document.

## mdTranslation CLI tools

To use the CLI tools, just type this in the terminal:
```shell
cargo install mdtranslation-cli
```
This allows you to use two commands:

1. `mdtranslation_extract`

This tool help you to extract original contents and initial boilerplate into a Markdown file.

For example, if you want to provide a French translation for `a.md`, you can execute this:

```shell
mdtranslation_extract -l fr_FR a.md >> a.i18n.md
```

The `>>` here allows the newly generated content be appended to the tail of the file. You can use this method to collect all paragraphs from a book together.


2. `mdtranslation_translate`

This tool help you generate a translated and rendered html file for a Markdown file.

For example, if you want to provide a French translation for `a.md`, and you have the translation file ready at `a.i18n.md`, you can execute this:

```shell
mdtranslation_translate -l fr_FR -t a.i18n.md a.md > a.htm
```

If you didn't specify the language in a.i18n.md, you can pass an extra `-d fr_FR` parameter here to tell the program that the language is actually `fr_FR`. This is useful for single-translated language case, though we encourage you always spell out the actual language for each paragraph.

## mdBook-translation preprocessor
There is also a mdBook preprocessor that allows you to use mdTranslation together with mdBook.

* To install the mdBook preprocessor, just type this in the terminal:
```shell
cargo install mdbook-translation
```

* After that you can enable this preprocessor for a specific book, by adding this to `book.toml`:
```text
[preprocessor.translation]
```

* Now, before you run `mdbook build` as usual, you can set up environment variable to ask the preprocessor
to generate the book in a specific language. 

You need to do this if you're are using Windows cmd.exe:
```text
set MDTRANSLATION_INPUT=book.i18n.md
set MDTRANSLATION_LANG=fr_FR
set MDTRANSLATION_DEFAULT_LANG=fr_FR
```
Or if you're using a *nix shell:
```shell
export MDTRANSLATION_INPUT=book.i18n.md
export MDTRANSLATION_LANG=fr_FR
export MDTRANSLATION_DEFAULT_LANG=fr_FR
```

The contents of the book will be replaced by the translated contents.
