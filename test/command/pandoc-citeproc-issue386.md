```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/advanced-optical-materials.csl'
references:
- DOI: 10.1038/lsa.2012.20
  ISSN: '2047-7538'
  author:
  - family: Ding
    given: K.
  - family: Ning
    given: C. Z.
  container-title: Light Sci. Appl.
  id: ding_metallic_2012
  issue: 7
  issued:
  - month: 7
    year: 2012
  page: 'e20-e20'
  title: 'Metallic subwavelength-cavity semiconductor nanolasers'
  type: 'article-journal'
  volume: 1
---

@ding_metallic_2012
^D
^\[1\]^

::: {#refs .references}
::: {#ref-ding_metallic_2012}
[\[1\]]{.csl-display-left-margin}[K. Ding, C. Z. Ning, *Light Sci.
Appl.* **2012**, *1*, e20.]{.csl-display-right-inline}
:::
:::
```
