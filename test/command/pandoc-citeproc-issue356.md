```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Alice
  id: foo
  issued:
  - year: 2042
  other-ids:
  - bar
  - doz
  type: book
---

[@bar]
^D
(Alice 2042)

::: {#refs .references .hanging-indent}
::: {#ref-foo}
Alice. 2042.
:::
:::
```
