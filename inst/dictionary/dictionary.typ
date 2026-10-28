// inst/dictionary/dictionary.typ
//
// The PDF of a data dictionary (episerver_dictionary()). Its content is read
// from dictionary.json, which .dict_pdf() writes beside it with the logo
// (.dict_data() gives its layout). Text from the label tables arrives as
// strings, so it is never read as Typst markup. Written for Typst 0.13, the
// version in the Quarto that comes with RStudio. Colours and fonts are those
// of dev/reference/act-brand.md; fonts that are not installed fall back to
// the next in their list.

#let d = json("dictionary.json")

#let nightshade = rgb("#320557")
#let iris = rgb("#562C8C")
#let lavender = rgb("#DCC8FA")
#let tint = rgb("#EAE6F1")
#let rule = rgb("#C7C7C7")
#let body-font = ("Source Sans 3", "Source Sans Pro", "Calibri", "Arial")
#let heading-font = ("Montserrat", "Arial")
#let band = 20mm
#let side = 14mm

// Lines of text, one under another
#let lines(items) = items.map(l => [#l]).join(linebreak())
// A point for each item, each a list of lines
#let points(items) = list(..items.map(lines))

#set document(title: d.heading + " - " + d.title)
#set text(font: body-font, size: 9pt, lang: "en", region: "AU")
#set page(
  paper: "a4",
  flipped: true,
  margin: (top: band + 8mm, bottom: 14mm, x: side),
  background: {
    place(top + left, rect(width: 100%, height: band, fill: nightshade))
    place(top + left, dx: side, box(height: band, align(horizon, stack(
      spacing: 5pt,
      text(font: heading-font, size: 14pt, weight: "bold", fill: white, d.title),
      text(size: 9.5pt, fill: lavender, d.subtitle),
    ))))
    if d.logo != none {
      place(top + right, dx: -side, dy: 4mm, image(d.logo, height: band - 8mm))
    }
  },
  footer: context {
    set text(size: 7.5pt, fill: luma(80))
    grid(
      columns: (1fr, auto, 1fr),
      align: (left, center, right),
      d.name,
      d.source,
      [#d.date, page #counter(page).display("1 of 1", both: true)],
    )
  },
)

#let section(title) = block(above: 12pt, below: 6pt, text(
  font: heading-font, size: 12pt, weight: "bold", fill: nightshade, title,
))
#let subsection(title) = block(above: 10pt, below: 5pt, text(
  font: heading-font, size: 10.5pt, weight: "bold", fill: nightshade, title,
))
#let theme(title) = block(above: 8pt, below: 4pt, text(
  size: 9.5pt, weight: "bold", fill: iris, title,
))

#block(below: 8pt, text(font: heading-font, size: 17pt, weight: "bold",
                         fill: nightshade, d.heading))
#block(below: 3pt, text(size: 10pt, d.subheading))
#block(text(size: 9pt, fill: luma(60), d.source))

// The description and the notes, the notes under their themes' headings,
// then the columns on a page of their own
#section("Dataset information")
#subsection("Dataset description")
#if d.description.len() == 0 [
  No dataset description available for this collection.
] else { points(d.description) }
#subsection("Dataset notes")
#if d.note_groups.len() == 0 [
  No dataset notes available for this collection.
] else {
  for g in d.note_groups {
    if g.theme != none { theme(g.theme) }
    points(g.notes)
  }
}
#if d.links.len() > 0 {
  subsection("Linked tables")
  list(..d.links.map(l => [#l]))
}

#pagebreak(weak: true)
#section("Columns")
#let heads = ("#", "Column", "Description", "Type", "Value labels")
#let widths = (auto, auto, 1.3fr, auto, 1.5fr)
#if d.has_notes {
  heads.push("Notes")
  widths.push(1fr)
}
#table(
  columns: widths,
  inset: (x: 4pt, y: 3.5pt),
  stroke: (x, y) => if y > 0 { (bottom: 0.4pt + rule) },
  fill: (x, y) => if y == 0 { nightshade } else if calc.even(y) { tint },
  table.header(..heads.map(h => text(weight: "bold", fill: white, h))),
  ..d.columns.map(c => {
    let values = c.values.map(v => [#v])
    if c.more != none { values.push(emph(c.more)) }
    let cells = (
      str(c.position),
      c.column,
      c.description,
      c.type,
      values.join(linebreak()),
    )
    if d.has_notes { cells.push(lines(c.notes)) }
    cells
  }).flatten(),
)
