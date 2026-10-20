# Hex sticker reference

How the package's hex sticker is built, and how to make a related badge in the same style. Colours and fonts come from `act-brand.md`.

| File | Holds |
|---|---|
| `man/figures/logo.svg` | The master artwork, with the name converted to outlines so that it renders the same on any machine. Use it for print and wherever a scalable logo is needed. |
| `man/figures/logo.png` | The README image, a 240 × 278 px export of `logo.svg`. This is the size `usethis::use_logo()` produces; the README shows it at 139 px high so that it stays sharp on high-density screens. |
| `inst/www/logo-lockup.svg` | The lockup in the top bar of the package's Shiny apps: the pyramid and the name from `logo.svg`, side by side, without the hexagon. See "Lockup for the apps". |
| `dev/reference/hex-sticker-template.svg` | The same artwork with the name as live text, as the starting point for a related badge. |

## Frame

The sticker uses the common hex sticker format: a hexagon with a point at the top, 50.8 mm (2 in) from point to point and 43.99 mm across the flats. The SVG unit is the millimetre (`viewBox="0 0 43.994 50.8"`), so every coordinate in the files is a distance on the printed sticker, measured from the top left.

The 1.5 mm border is a Violet hexagon with a Nightshade hexagon drawn over it, inset 1.5 mm from each edge. It is not a stroke, because an SVG stroke is centred on its path and would extend beyond the cut line. The outer edge is the cut line, and the bars and name sit at least 2.5 mm inside it. If a printer asks for bleed, enlarge the `border` hexagon.

## Colours and type

| Element | SVG id | Colour | Hex |
|---|---|---|---|
| Border | `border` | Violet | `#6E46E0` |
| Background | `background` | Nightshade | `#320557` |
| Bars for males, left | `males` | Bluebell | `#A591FF` |
| Bars for females, right | `females` | Orchid | `#B464DC` |
| Name | `name` | White | `#FFFFFF` |

The two bar colours differ in lightness as well as hue, so the sides stay distinct in greyscale.

The name is set in Montserrat SemiBold (weight 600), the brand heading face, at a font size of 6.6 mm (18.7 pt). It is centred on the vertical centre line (x = 21.997) with its baseline at y = 41.6. The outlines in `logo.svg` were made from the `@fontsource/montserrat` 5.3.0 font files, shaped with HarfBuzz with kerning on. Rendered in a browser with the same font, the template's live text matches them to within 0.1 mm.

## Pyramid

The bars trace the outline of the ACT; they do not show population data. There are 18 bars, one for each five-year age group from 0–4 at the bottom to 85+ at the top, so the south of the territory is the young end of the pyramid.

The outline is the ACT polygon from the Natural Earth 1:10m admin-1 boundaries (public domain), projected to kilometres with an equirectangular projection at 35.52° S. Its area in that projection is 2,346 km², 0.5% less than the ABS ASGS 2021 figure of 2,358 km². The outline's north–south extent of 86.6 km is divided into 18 bands of equal height. Within each band, the male bar runs from the axis to the mean western edge of the outline and the female bar runs to the mean eastern edge.

The axis is a single meridian. The northern tip of the ACT lies east of its southern tail, so no meridian crosses every band. The axis is at the median of the 18 band midpoints, about 149.00° E, which keeps the two sides of comparable width. A meridian through both tips would keep the outline exact but leave the female bars as slivers for most of the height. In the two northernmost bands and the southernmost band, the outline does not reach 2.2 km west of the axis, so those male bars are drawn at that minimum length and every age group has a bar for both sexes.

The pyramid is drawn at 0.335 mm per km, 29 mm tall from y = 6 to y = 35, with the axis on the sticker's centre line. Each bar fills 80% of its band, and a 0.3 mm gap separates the two sides at the axis.

## Lockup for the apps

The Shiny apps show the sticker's motif in their 104 px top bar, on Nightshade, as a lockup rather than the whole hexagon (`.epi_logo()` in `R/epi_app.R`). At the bar's height the hexagon would be about 60 px tall, with the name set at about 8 px inside it; side by side, the name is set at about 26 px.

- **Parts.** The `pyramid` group and the outlined `name` path of `logo.svg`, copied unchanged: the same bars, colours and outlines. `test-epi_app.R` checks that they still match `logo.svg`.
- **Layout.** The pyramid at the sticker's scale, moved to the origin (`translate(-15.1,-6.16)`), 18.87 mm wide and 28.68 mm tall. The name at twice the sticker's scale, 4 mm to the right of the pyramid, with the middle of its x-height (y = 39.85 on the sticker) on the pyramid's vertical centre: `translate(2.19,-65.36) scale(2)`. The viewBox is `0 0 69.69 28.68`.
- **Use.** The name is white, so the lockup is for Nightshade and the darker purples only. The apps show it 56 px tall with 24 px above and below (`.navbar .epi-logo` in `.epi_app_style()`).

When the sticker changes, rebuild the lockup from the new `logo.svg` with the same layout; the test fails until it matches.

## Making a related badge

Start from `hex-sticker-template.svg`. Keep the frame, colours and type, and change only the name, and the motif if the badge needs its own. Each part is a group or element with an id (`frame`, `pyramid`, `males`, `females` and `name`), so it can be selected as a unit in Inkscape or Illustrator, or found by its id when the file is edited as text.

Edit the text of the `name` element. With Montserrat installed (Google Fonts, SIL Open Font Licence), the template shows the new name in the sticker's type; without it, the name is drawn in a fallback font. Keep the font size at 6.6 mm so that related badges share one type size, and reduce it only if the name would come within 1 mm of the border. At 6.6 mm, "actepir" spans 23.4 mm and already sits 1 mm from the border, so a longer name needs a smaller size.

A replacement motif uses the same palette and sits in the area the pyramid occupies, between y = 6 and y = 35, at least 1 mm from the border.

When the badge is final, convert the name to outlines (Inkscape: Path > Object to Path; Illustrator: Type > Create Outlines) and save the result as the package's `man/figures/logo.svg`. Export the README image from it at 240 × 278 px, for example with `rsvg::rsvg_png("man/figures/logo.svg", "man/figures/logo.png", width = 240, height = 278)`, and add it to the README's title line as this package does:

```markdown
# actepir <img src="man/figures/logo.png" align="right" height="139" alt="" />
```
