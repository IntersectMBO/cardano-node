// Document template for the observability specification, in the ICAN Group design.
// Derived from basic-report 0.5.0 by Roland Schätzle (MIT, see LICENSE in this directory);
// trimmed to what the specification uses, with the brand palette and typefaces of brand.typ.

#import "@preview/hydra:0.6.2": hydra
#import "brand.typ": palette, fonts
#import "titlepage.typ": titlepage

#let basic-report(
  doc-category: none,
  doc-title: none,          // string
  title-accent: none,       // string appended to the title, set in the accent colour
  author: none,
  affiliation: none,
  language: "en",
  show-outline: true,
  datetime-fmt: "[year]-[month]-[day]",
  body,
) = {

  // ----- Global Parameters ------------------------

  let full-title = if title-accent == none { doc-title } else { doc-title + " " + title-accent }
  set document(title: full-title, author: author)
  set text(lang: language)

  let body-size = 10pt
  let info-size = 9pt       // heading font in this size for information blocks
  let label-size = 8pt      // heading font in this size for labels

  // are we inside or outside of the outline (for roman/arabic page numbers)?
  let in-outline = state("in-outline", true)

  // ----- Title Page ------------------------

  counter(page).update(0)   // so the TOC after the title page begins with page i
  titlepage(doc-category, doc-title, title-accent, author, affiliation, info-size, datetime-fmt)

  // ----- Basic Text- and Page-Setup ------------------------

  set text(font: fonts.sans, size: body-size, fill: palette.ink)

  set par(justify: true, leading: 0.75em, spacing: 1.5em, first-line-indent: 0em)

  // Page grid: horizontal 1.5cm-grid = 14u: 3u left margin, 9u text, 2u right margin;
  // vertical: 2u top margin (plus the header), 14u text, 2u bottom margin.
  set page(
    paper: "a4",
    margin: (top: 3.6cm, left: 4.5cm, right: 3cm, bottom: 3cm),
    // the header shows the main chapter heading on the left and the page number on the right,
    // above a thin rule in the brand red
    header: context {
      grid(
        columns: (1fr, 1fr),
        align: (left, right),
        row-gutter: 0.5em,
        text(font: fonts.display, size: label-size, fill: palette.muted,
          hydra(1, use-last: false, skip-starting: false)),
        text(font: fonts.display, size: label-size, fill: palette.muted, number-type: "lining",
          if in-outline.get() { counter(page).display("i") } else { counter(page).display("1") }),
        grid.cell(colspan: 2, line(length: 100%, stroke: 0.5pt + palette.red)),
      )
    },
    header-ascent: 1.5em,
  )

  // ----- Headings & Numbering ------------------------

  set heading(numbering: "1.")
  show heading: set text(font: fonts.display, weight: "bold", tracking: -0.01em, fill: palette.ink)
  show heading: set par(justify: false)

  // The number in red: ICAN Red only where large (level 1), Red 700 below.
  let heading-body(it) = {
    if it.numbering != none {
      let num-color = if it.level == 1 { palette.red } else { palette.red-700 }
      text(fill: num-color, counter(heading).display(it.numbering))
      h(0.5em)
    }
    it.body
  }
  show heading.where(level: 1): it => {
    v(3.2 * body-size, weak: true)
    block(text(size: 1.45em, heading-body(it)))
    v(0.3 * body-size)
  }
  show heading.where(level: 2): it => {
    v(1.2 * body-size)
    block(text(size: 1.2em, heading-body(it)))
    v(0.2 * body-size)
  }
  show heading.where(level: 3): it => {
    v(1.0 * body-size)
    block(text(size: 1.05em, heading-body(it)))
    v(0.1 * body-size)
  }

  // ----- Code, Links, Tables, References ------------------------

  show raw: set text(font: fonts.mono)
  set raw(theme: none)   // no syntax colours: the palette is one red plus neutrals
  show raw.where(block: true): set text(size: 7.5pt)
  show raw.where(block: true): block.with(fill: palette.paper-2, inset: 8pt, radius: 2pt, width: 100%)

  // Red as link text: Red 700, the only red legible at body size.
  show link: set text(fill: palette.red-700)

  set table(
    inset: (x: 5pt, y: 4pt),
    stroke: (x, y) => (bottom: 0.5pt + if y == 0 { palette.line-strong } else { palette.line }),
    fill: (x, y) => if y == 0 { palette.paper-3 },
  )
  show table: set text(size: 8.5pt)
  show table: set par(justify: false, leading: 0.55em)
  // Table headers in the brand's label style: mono, uppercase, tracked.
  show table.cell.where(y: 0): set text(font: fonts.mono, size: 7pt, weight: "medium",
    tracking: 0.08em, fill: palette.muted)
  show table.cell.where(y: 0): it => upper(it)

  // A reference to a heading reads "§5.2".
  show ref: it => {
    let el = it.element
    if el != none and el.func() == heading and el.numbering != none {
      let nums = numbering(el.numbering, ..counter(heading).at(el.location()))
      link(el.location(), [§#nums.trim(".", at: end)])
    } else {
      it
    }
  }

  // ----- Table of Contents ------------------------

  show outline: it => {
    in-outline.update(true)
    show link: set text(fill: palette.ink)   // TOC entries are links, but no accents
    it
    in-outline.update(false)
  }

  // top-level TOC entries in bold without filling
  show outline.entry.where(level: 1): it => {
    set block(above: 1.8 * body-size)
    set text(font: fonts.display, weight: "bold", size: info-size)
    link(it.element.location(),
      it.indented(it.prefix(), it.body() + box(width: 1fr) + strong(it.page())))
  }

  // other TOC entries in regular with dot leaders
  show outline.entry.where(level: 2).or(outline.entry.where(level: 3)): it => {
    set block(above: 0.8 * body-size)
    set text(font: fonts.display, size: info-size)
    link(it.element.location(),
      it.indented(it.prefix(),
        it.body() + "  " + box(width: 1fr, repeat([.], gap: 2pt)) + "  " + it.page()))
  }

  if show-outline {
    outline(indent: auto)
    counter(page).update(0)     // so the first chapter starts at page 1 (arabic)
  } else {
    in-outline.update(false)
    counter(page).update(1)
  }
  pagebreak()

  body
}
