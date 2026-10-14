# ACT Government brand reference

Colour, type and logo rules for actepir themes, palettes and templates. Derived from the ACT Government Brand Guardian at-a-glance, version 1.0 (July 2025), an internal document. This file records only the values and rules needed for design decisions. Consult the source document, or the Creative team, for anything not covered here.

The brand is digital-first (RGB). Hex values are authoritative for HTML and screen output. CMYK equivalents exist but print more muted; they are not reproduced here.

## Primary palette

Six shades of purple. One or more must appear in any branded output. Other colours may be added to meet communication needs, chosen to complement the purples and meeting WCAG AA contrast.

| Name | Hex | RGB | Note |
|---|---|---|---|
| Nightshade | `#320557` | 50, 5, 87 | Darkest. Primary dark background and heading colour. |
| Iris | `#562C8C` | 86, 44, 140 | The original ACT Government purple. |
| Violet | `#6E46E0` | 110, 70, 224 | Vibrant mid-tone. |
| Orchid | `#B464DC` | 180, 100, 220 | |
| Bluebell | `#A591FF` | 165, 145, 255 | |
| Lavender | `#DCC8FA` | 220, 200, 250 | Lightest. Light background and tint. |

### Text on primary colours (WCAG AA)

AA means normal text, 10pt and above. AA Large means 14pt bold or 18pt regular and above. Pairings not listed were not shown as compliant in the source.

| Background | AA | AA Large only |
|---|---|---|
| Nightshade | White, Lavender | Orchid |
| Iris | White, Lavender, Bluebell | |
| Violet | White | Lavender, Black |
| Orchid | Black | Nightshade, White |
| Bluebell | Black, Nightshade | Iris |
| Lavender | Black, Nightshade, Iris | Violet |

Implications for the package: body text on Nightshade or Iris is white; body text on Lavender, Bluebell or Orchid is black or Nightshade. Violet carries white text only at body size. Lavender on Nightshade and Nightshade on Lavender are the two compliant purple-on-purple pairs at body size.

## Regional colours

For content about Canberra's five regions.

| Region | Hex | RGB | AA text | AA Large only |
|---|---|---|---|---|
| Inner North, Inner South, City | `#008FC5` | 0, 143, 197 | Black | White |
| Belconnen | `#077F7A` | 7, 128, 123 | White | Black |
| Gungahlin | `#90A630` | 144, 166, 48 | Black | |
| Tuggeranong | `#C6B30B` | 198, 179, 11 | Black | |
| Woden, Weston Creek, Molonglo Valley | `#F15F50` | 241, 95, 80 | Black | White |

## District colours

Subdivide certain regions. Inner North and City, and Woden, reuse their regional colour.

| District | Hex | RGB | AA text | AA Large only |
|---|---|---|---|---|
| Inner North, City | `#008FC5` | 0, 143, 197 | Black | White |
| Inner South | `#004868` | 0, 72, 104 | White | |
| East Canberra | `#0AB4BA` | 10, 180, 186 | Black | |
| Woden | `#F15F50` | 241, 95, 80 | Black | White |
| Weston Creek | `#F14C76` | 241, 76, 118 | Black | White |
| Molonglo Valley | `#93171B` | 147, 23, 27 | White | |

## Fonts

| Use | Primary (Google Fonts, open licence) | Alternative (when primary cannot be used, e.g. Word) |
|---|---|---|
| Headings and highlights | Montserrat: Light, Regular, Semibold, Bold, Extra Bold | Arial Regular, Arial Bold |
| Body copy | Source Sans Pro (Source Sans 3 on Google Fonts): Light, Regular, Semibold, Bold | Calibri Regular, Calibri Bold |

Minimum sizes: 6pt for primary fonts, 7pt for alternative fonts.

For HTML output, Montserrat and Source Sans 3 can be loaded from Google Fonts where the network allows, with Arial and Calibri as the fallback stack.

## Logo

- The ACT Government logo is used on all external communications. Directorate logos are for internal communications only and are never used externally.
- The logo appears in black or white only. Never recolour or redraw it; always use the supplied artwork.
- The reversed (white) version always shows the black swan on the left.
- Clear space around the logo on every side equals the height of the "ACT" lettering at the size used.
- Minimum size: the crest circle is at least 12 mm in print or 60 px on screen.
- Use of the logo on non-ACT Government material must be verified by the Creative team.

## Relationship to the current package

The package predates this palette. `i_acthd_colors.R` and the Quarto themes encode an older "bgs" scheme (blue `#002677`, turquoise `#78D5E1` and others) plus approximations of the purple (`#472d8c` in R, `#27015F` in `acthcsd.scss`). None of those purples matches a current brand value. When updating themes or palettes, use the values above and treat the older colours as legacy to be retired rather than extended.
