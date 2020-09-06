```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Author
    given: Al
  id: item1
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: bazbaz foo'
  type: 'article-journal'
---

Foo [@item1].

References {#references .unnumbered}
==========
^D
Foo (Author 1998).

References {#references .unnumbered}
==========

::: {#refs .references .hanging-indent}
::: {#ref-item1}
Author, Al. 1998. "Foo Bar Baz: Bazbaz Foo."
:::
:::
```
