// ICAN Group corporate design: colour tokens and typefaces.
// Values from the ICAN Group colour palette (taken from icangroup.co.uk, 17 August 2026).

#let palette = (
  // Primary: the single brand colour. Rationed to accents (~5-10% of a layout).
  red:      rgb("#ee3532"),
  red-600:  rgb("#d8302d"),
  red-700:  rgb("#b81f1c"),   // red as small text or links on light ground
  red-tint: rgb("#fdeceb"),
  // Dark neutrals (cool)
  ink:      rgb("#15161a"),   // default text; never pure black
  ink-900:  rgb("#0b0c0f"),
  ink-800:  rgb("#16181d"),
  ink-700:  rgb("#2a2d34"),
  // Light neutrals (warm)
  paper:    rgb("#ffffff"),
  paper-2:  rgb("#f5f4f1"),
  paper-3:  rgb("#eceae5"),   // table header fills, inset panels
  silver:   rgb("#bfbfbf"),   // shapes only, never text
  // Text tones on light ground
  muted:    rgb("#585b62"),   // supporting text, captions (AA)
  muted-2:  rgb("#767980"),   // large text only
  // Lines
  line:        rgb(20, 22, 26, 31),   // rgba(20,22,26,.12)
  line-strong: rgb(20, 22, 26, 66),   // rgba(20,22,26,.26)
)

// Typefaces, each with fallbacks: the brand fonts are used where installed.
#let fonts = (
  display: ("Space Grotesk", "Inter", "DejaVu Sans"),
  sans:    ("Inter", "DejaVu Sans"),
  mono:    ("IBM Plex Mono", "DejaVu Sans Mono"),
)
