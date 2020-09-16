```
% pandoc --citeproc -t markdown-citations
---
bibliography:
- command/biblio.bib
nocite: '[@*]'
---

^D
::: {#refs .references .hanging-indent}
::: {#ref-item1}
Doe, John. 2005. *First Book*. Cambridge: Cambridge University Press.
:::

::: {#ref-item2}
---------. 2006. "Article." *Journal of Generic Studies* 6: 33--34.
:::

::: {#ref-пункт3}
Doe, John, and Jenny Roe. 2007. "Why Water Is Wet." In *Third Book*,
edited by Sam Smith. Oxford: Oxford University Press.
:::
:::
```
