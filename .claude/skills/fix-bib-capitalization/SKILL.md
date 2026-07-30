---
name: fix-bib-capitalization
description: Find and fix proper nouns that BibTeX has lowercased in the bibliography (e.g. "venice" instead of "Venice"). Use whenever asked to check citation rendering, fix bibliography capitalization, or review .bbl/.bib output for lowercase proper nouns/acronyms.
---

# Fix BibTeX capitalization

BibTeX/BibLaTeX styles that use sentence case for titles (common with numeric/author-year
styles like `elsarticle-num-names`) lowercase every word in a `title` or `journal` field
except the first word, the word after a colon, and anything wrapped in `{...}`. This means
proper nouns, place names, nationalities/adjectives derived from proper nouns, acronyms, and
named institutions silently lose their capitals unless they are explicitly braced in the
`.bib` source.

This skill finds those cases by comparing the *rendered* bibliography (what BibTeX actually
produced) against the `.bib` source, and fixes the source.

## Procedure

1. **Find the compiled bibliography output.** Look for a `.bbl` file next to the main `.tex`/
   `.Rtex` file (e.g. `main.bbl`) — this is BibTeX's literal rendered output and is the
   ground truth for what appears in the PDF. If no `.bbl` exists, compile once
   (`pdflatex` then `bibtex`) to generate it, or read the rendered citation list directly
   from the PDF's references section.

2. **Locate the `.bib` file(s).** A paper may split references across multiple `.bib` files
   (check the `\bibliography{...}` command in the main `.tex`/`.Rtex` file for the exact list).

3. **Scan the `.bbl` for suspicious lowercase words**, focusing on `\bibinfo{title}{...}` and
   `\bibinfo{journal}{...}` fields (or the BibLaTeX equivalent). Look for:
   - Lowercased place names, demonyms, or nationalities (e.g. `venice`, `florentine`,
     `italian`, `brazilian`, `uk`)
   - Lowercased named institutions or formal bodies (e.g. `congress of the republic of peru`,
     `united states house of representatives`)
   - Lowercased acronyms (e.g. `uk`, `gis`)
   - Journal names that should be title case but aren't (e.g. `science` → `Science`,
     `physical review e` → `Physical Review E`, `american journal of sociology`)
   Ignore ordinary lowercase common words — sentence case is *correct* for those; only flag
   words that are proper nouns/adjectives or acronyms.

4. **For each flagged citation key**, grep the `.bib` file(s) for that key and find the
   offending `title=`/`journal=` field.

5. **Fix by wrapping only the proper noun/acronym in braces**, not the whole field (so the
   rest of the sentence-case behavior is preserved for ordinary words). Examples:
   - `title={The Venetian Constitution in Florentine Political Thought}`
     → `title={The {Venetian} Constitution in {Florentine} Political Thought}`
   - `journal={science}` → `journal={Science}`
   - `title={...the Congress of the Republic of Peru}`
     → `title={...the {Congress of the Republic of Peru}}`
   Multi-word proper names (e.g. "United States House of Representatives", "Republic of
   Venice") should be wrapped as a single braced group so BibTeX doesn't insert its own
   capitalization logic mid-phrase.

6. **Verify only entries that are actually cited** in the main file need fixing — skip
   unused `.bib` entries to avoid noisy diffs, but it's fine to also check for correctness
   if asked.

## Notes

- Never brace an entire title/journal field — that disables sentence casing for the whole
  string and creates a different, unwanted rendering (all-caps-preserving instead of just
  protecting the proper noun).
- Book/incollection `booktitle` fields are often preserved as-authored by many styles (not
  sentence-cased), so check the actual `.bbl` rendering before assuming they need braces —
  don't fix fields that already render correctly.
- If several `.bib` files exist, don't guess which one holds an entry — grep across all of
  them for the citation key before editing.
