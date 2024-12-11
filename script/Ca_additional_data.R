library(tidyverse)

## Aurignacian retouch
Dataset %>%
  filter(Class == "Tool" | Class == "Core-Tool") %>%
  filter(Typology.macro == "Composite tool" | Typology.macro == "Blade retouched" | Typology.macro == "Flake retouched") %>%
  tabyl(Spit, Aurignacian.retouch) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lcccr") %>%
  row_spec(1, bold = T, hline_after = F) %>%
  kable_classic_2(lightable_options = "striped", full_width = F)


## Carinated frequency
Dataset_cores %>%
  filter(Laminar_y.n == "yes") %>%
  tabyl(Core.classification, Spit) %>%
  adorn_totals("row") %>%
  adorn_totals("col") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lccccr") %>%
  kable_classic_2(lightable_options = "striped", full_width = F)

## Carinated maintenance blanks frequency
Dataset %>%
  filter(Blank.technology == "Maintenance carinated") %>%
  tabyl(Spit, Blank.technology) %>%
  adorn_totals("row") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lcccr") %>%
  row_spec(1, bold = T, hline_after = F) %>%
  kable_classic_2(lightable_options = "striped", full_width = F)