---
name: double-blind-check
description: For double blind publications, check self-references as well change bibliography and/or text to comply with guidelines
version: 0.0.1
---

# Check and fix for double blind reviews

Many conferences and journals request double-blind submission, with papers
having no trace of the authors or anonymizing references to such when there is
no other alternative. The papers need to have no trace of who the authors are.

- Context: {{authors_byline}}

## Procedure

1. Identify self-references ini the paper, including, but not limited to:
   references to own papers, links to data or code repositories,
   acknosledgements of funds.
2. References to own papers will be papers authored by {{authors_byline}}
   present in the bibliography. There are two options with these papers:
   a) When the reference is just background or related literature, simply edit
   the text to make it a third-person reference.
   b) When it is a reference to a unpublished paper or work in progress, edit
   the bibliography file creating a referene with anonymous authors,
   journal/conference and title and have it use the key that results from
   appending `_anon` to the previous key so that it's easy to find and replace
   when the paper is accepted.
3. If funding information is present, comment it out and substitute it by a text
   that uses more or less the same space.
4. References to repositories or links are better moved to its own line and
   commented out, with its url substituted by another with the same site and
   anonymous-looking path.

## Notes

- Don't make any change if a reference is made in the third person.
- If there are many references to work by specific authors, suggest anonymizing
  some of them so that the authors are not identified by extensive
  self-references.
  
