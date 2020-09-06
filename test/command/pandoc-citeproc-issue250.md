```
% pandoc --citeproc -t markdown-citations
---
link-citations: true
references:
- author:
    family: Doe
  id: doe
  title: Title
---

[@doe]
^D
([Doe, n.d.](#ref-doe))

::: {#refs .references .hanging-indent}
::: {#ref-doe}
Doe. n.d. "Title."
:::
:::
```
