```
% pandoc --citeproc -t markdown-citations
---
csl: command/apa.csl
references:
- author:
  - family: Benjamin
    given: A. S.
  id: itemA1
- author:
  - family: Yaakov
    given: D.
    non-dropping-particle: ben
  id: itemA2
- author:
  - family: Brown
    given: J. R.
  id: itemA3
- author:
  - family: Browning
    given: A. R.
  id: itemA4
- author:
  - family: Girard
    given: 'J.-B.'
  id: itemA5
- author:
  - family: 'Girard-Perregaux'
    given: A. S.
  id: itemA6
- author:
  - family: Ibn Abdulaziz
    given: T.
  id: itemA7
- author:
  - family: Ibn Nidal
    given: A. K. M.
  id: itemA8
- author:
  - family: López
    given: M. E.
  id: itemA9
- author:
  - family: López de Molina
    given: G.
  id: itemA10
- author:
  - family: Singh
    given: Y.
  id: itemA11
- author:
  - family: Singh Siddhu
    given: N.
  id: itemA12
- author:
  - family: Villafuerte
    given: S. A.
  id: itemA13
- author:
  - family: 'Villa-Lobos'
    given: J.
  id: itemA14
- author:
  - family: Macalister
    given: Donald
  id: itemB1
- author:
  - family: MacAlister
    given: Paul
  id: itemB2
- author:
  - family: Macauley
    given: Catharine
  id: itemB3
- author:
  - family: Macmillan
    given: Harold
  id: itemB4
- author:
  - family: Madison
    given: James
  id: itemB5
- author:
  - family: McAllister
    given: Ward
  id: itemB6
- author:
  - family: McAuley
    given: Catherine
  id: itemB7
- author:
  - family: McMillan
    given: Edwin M.
  id: itemB8
- author:
  - family: 'Sainte-Beuve'
    given: 'Charles-Augustin'
  id: itemC1
- author:
  - family: 'Saint-Gaudens'
    given: Augustus
  id: itemC2
- author:
  - family: 'Saint-Saëns'
    given: Camille
  id: itemC3
- author:
  - dropping-particle: de
    family: San Martin
    given: José
  id: itemC4
- author:
  - family: St. Denis
    given: Ruth
  id: itemC5
- author:
  - family: St. Laurent
    given: Louis Stephen
  id: itemC6
---

Foo
[@itemA1; @itemA2; @itemA3; @itemA4; @itemA5; @itemA6; @itemA7; @itemA8; @itemA9; @itemA10; @itemA11; @itemA12; @itemA13; @itemA14].

Foo
[@itemB1; @itemB2; @itemB3; @itemB4; @itemB5; @itemB6; @itemB7; @itemB8].

Foo [@itemC1; @itemC2; @itemC3; @itemC4; @itemC5; @itemC6].
^D
Foo (ben Yaakov, n.d.; Benjamin, n.d.; Brown, n.d.; Browning, n.d.;
Girard, n.d.; Girard-Perregaux, n.d.; Ibn Abdulaziz, n.d.; Ibn Nidal,
n.d.; López de Molina, n.d.; López, n.d.; Singh Siddhu, n.d.; Singh,
n.d.; Villafuerte, n.d.; Villa-Lobos, n.d.).

Foo (Macalister, n.d.; MacAlister, n.d.; Macauley, n.d.; Macmillan,
n.d.; Madison, n.d.; McAllister, n.d.; McAuley, n.d.; McMillan, n.d.).

Foo (Sainte-Beuve, n.d.; Saint-Gaudens, n.d.; Saint-Saëns, n.d.; San
Martin, n.d.; St. Denis, n.d.; St. Laurent, n.d.).

::: {#refs .references .hanging-indent}
::: {#ref-itemA2}
ben Yaakov, D. (n.d.).
:::

::: {#ref-itemA1}
Benjamin, A. S. (n.d.).
:::

::: {#ref-itemA3}
Brown, J. R. (n.d.).
:::

::: {#ref-itemA4}
Browning, A. R. (n.d.).
:::

::: {#ref-itemA5}
Girard, J.-B. (n.d.).
:::

::: {#ref-itemA6}
Girard-Perregaux, A. S. (n.d.).
:::

::: {#ref-itemA7}
Ibn Abdulaziz, T. (n.d.).
:::

::: {#ref-itemA8}
Ibn Nidal, A. K. M. (n.d.).
:::

::: {#ref-itemA10}
López de Molina, G. (n.d.).
:::

::: {#ref-itemA9}
López, M. E. (n.d.).
:::

::: {#ref-itemB1}
Macalister, D. (n.d.).
:::

::: {#ref-itemB2}
MacAlister, P. (n.d.).
:::

::: {#ref-itemB3}
Macauley, C. (n.d.).
:::

::: {#ref-itemB4}
Macmillan, H. (n.d.).
:::

::: {#ref-itemB5}
Madison, J. (n.d.).
:::

::: {#ref-itemB6}
McAllister, W. (n.d.).
:::

::: {#ref-itemB7}
McAuley, C. (n.d.).
:::

::: {#ref-itemB8}
McMillan, E. M. (n.d.).
:::

::: {#ref-itemC1}
Sainte-Beuve, C.-A. (n.d.).
:::

::: {#ref-itemC2}
Saint-Gaudens, A. (n.d.).
:::

::: {#ref-itemC3}
Saint-Saëns, C. (n.d.).
:::

::: {#ref-itemC4}
San Martin, J. de. (n.d.).
:::

::: {#ref-itemA12}
Singh Siddhu, N. (n.d.).
:::

::: {#ref-itemA11}
Singh, Y. (n.d.).
:::

::: {#ref-itemC5}
St. Denis, R. (n.d.).
:::

::: {#ref-itemC6}
St. Laurent, L. S. (n.d.).
:::

::: {#ref-itemA13}
Villafuerte, S. A. (n.d.).
:::

::: {#ref-itemA14}
Villa-Lobos, J. (n.d.).
:::
:::
```
