```
% pandoc --citeproc -t markdown-citations
---
references:
- id: test
  title: Essays presented to N.R. Ker (On Art)
- id: test2
  title: '*Test:* An experiment: An abridgement'
---

@test; @test2
^D
"Essays Presented to N.R. Ker (On Art)" (n.d.); "*Test:* An Experiment:
An Abridgement" (n.d.)

::: {#refs .references .hanging-indent}
::: {#ref-test}
"Essays Presented to N.R. Ker (On Art)." n.d.
:::

::: {#ref-test2}
"*Test:* An Experiment: An Abridgement." n.d.
:::
:::
```
