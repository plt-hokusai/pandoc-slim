```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/sage-harvard.csl'
references:
- author:
    family: Doe
  id: doe
  issued:
    year: 2007
  type: article
- author:
    family: Zoe
  id: zoe
  issued:
  - year: 2009
  type: article
- author:
    family: Roe
  id: roe
  issued:
  - year: 2007
  type: article
---

[@zoe; @roe; see for comparison @doe, p. 3]
^D
(Roe, 2007; Zoe, 2009; see for comparison Doe, 2007: 3)

::: {#refs .references .hanging-indent}
::: {#ref-doe}
Doe (2007).
:::

::: {#ref-roe}
Roe (2007).
:::

::: {#ref-zoe}
Zoe (2009).
:::
:::
```
