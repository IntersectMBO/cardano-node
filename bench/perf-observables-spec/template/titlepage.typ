// Title page, in the ICAN Group design.
// Derived from basic-report 0.5.0 by Roland Schätzle (MIT, see LICENSE in this directory).

#import "brand.typ": palette, fonts

#let titlepage(
  doc-category,
  doc-title,
  title-accent,
  author,
  affiliation,
  info-size,
  datetime-fmt,
) = {
  set page(paper: "a4", margin: (top: 3cm, left: 4.5cm, right: 3cm, bottom: 4.5cm), header: none)

  v(6cm)

  // ----- Eyebrow: mono, uppercase, tracked, in red, with a short rule to its left ------
  if doc-category != none {
    block(below: 1.2em,
      box(baseline: -0.3em, line(length: 0.9cm, stroke: 1.5pt + palette.red))
      + h(0.9em)
      + text(font: fonts.mono, size: 8pt, tracking: 0.16em, fill: palette.red, upper(doc-category)))
  }

  // ----- Title: one accented phrase ------------------------
  block(
    par(justify: false, leading: 0.35em,
      text(font: fonts.display, weight: "bold", size: 32pt, tracking: -0.025em,
        fill: palette.ink, hyphenate: false,
        doc-title + if title-accent != none { " " + text(fill: palette.red, title-accent) })))

  // ----- Info block ------------------------
  place(bottom + left,
    par(leading: 0.9em, justify: false,
      text(font: fonts.mono, size: info-size - 1pt, fill: palette.muted,
        (datetime.today().display(datetime-fmt), author, affiliation)
          .filter(x => x != none and x != "")
          .join(linebreak()))))

  pagebreak()
}
