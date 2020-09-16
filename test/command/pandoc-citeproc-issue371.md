```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Doe
    given: Jane
  id: item1
  status: in press
  title: Title one
  type: book
- author:
  - family: Doe
    given: Jane
  id: item2
  issued:
  - year: 2018
  title: Title two
  type: book
---

Foo [@item2; @item1].

References {#references .unnumbered}
==========
^D
Foo (Doe 2018, in press).

References {#references .unnumbered}
==========

::: {#refs .references .hanging-indent}
::: {#ref-item2}
Doe, Jane. 2018. *Title Two*.
:::

::: {#ref-item1}
---------. In press. *Title One*.
:::
:::
```
