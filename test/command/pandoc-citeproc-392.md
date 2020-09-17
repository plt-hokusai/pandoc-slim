```
% pandoc --citeproc -t markdown-citations
---
csl: command/vancouver.csl
references:
- author:
  - family: James
    given: M.R.C.E.L.
  id: james
- author:
  - family: MacFarlane
    given: J. G.
  id: macfarlane
---

@james; @macfarlane
^D
(1); (2)

::: {#refs .references}
::: {#ref-james}
[1. ]{.csl-left-margin}[James MRCEL. ]{.csl-right-inline}
:::

::: {#ref-macfarlane}
[2. ]{.csl-left-margin}[MacFarlane JG. ]{.csl-right-inline}
:::
:::
```
