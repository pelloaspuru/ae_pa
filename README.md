# Applied Economics: course website

Source of the course website, built as a [Quarto](https://quarto.org/) book and served by GitHub Pages from the `docs/` folder.

## Layout

- `_quarto.yml`: site configuration and chapter order
- `index.qmd`: landing page
- `*.qmd`: one page per topic
- `slides/`, `code/`, `data/`: files the pages link to
- `docs/`: the rendered site (generated, do not edit by hand)

## Update the site

1. Edit the `.qmd` files, or add files to `slides/`, `code/` or `data/`.
2. Render: `quarto render` (or `quarto preview` to check it in the browser first).
3. Commit everything, including `docs/`, and push.

Quarto ships with RStudio. If `quarto` is not found in a terminal, use the Terminal tab inside RStudio or run
`"C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render`.
