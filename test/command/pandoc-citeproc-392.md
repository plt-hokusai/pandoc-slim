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
[1. ]{.csl-display-left-margin}[James MRCEL.
]{.csl-display-right-inline}
:::

::: {#ref-macfarlane}
[2. ]{.csl-display-left-margin}[MacFarlane JG.
]{.csl-display-right-inline}
:::
:::
```
