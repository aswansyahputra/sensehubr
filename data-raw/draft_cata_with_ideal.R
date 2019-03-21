library(readxl)
library(tidyverse)
library(ggrepel)
library(purrr)
library(intubate)
library(broom)
library(FactoMineR)
source("/home/aswansyahputra/Projects/GitHub/sensehub/_helpers/check_status.R")

demoCATA <- read_xls("/home/aswansyahputra/Downloads/demoCATA.xls")
skimr::skim(demoCATA)
glimpse(demoCATA)

input <-
  left_join(
    demoCATA %>%
      select(-c(Consumer, Liking)) %>%
      group_by(Product) %>%
      summarise_at(vars(Firm:Astringent), ~sum(.)),
    demoCATA %>%
      select(-c(Consumer)) %>%
      group_by(Product) %>%
      mutate(
        Liking = replace_na(Liking, 0)
      ) %>%
      summarise_at(vars(Liking), ~mean(., na.rm = TRUE))
  ) %>%
  as.data.frame() %>%
  column_to_rownames("Product")
input
res_ca <- CA(input, quanti.sup = 16, graph = FALSE)
plot(res_ca, invisible = "col")
plot(res_ca, invisible = "row")
plot(res_ca)
plot(res_ca, choix = "quanti.sup")

liking <-
  demoCATA %>%
  select(Product, Consumer, Liking) %>%
  filter(Product != "Ideal")
liking

ideal <-
  demoCATA %>%
  filter(Product == "Ideal") %>%
  select(-c(Product, Liking)) %>%
  gather(key = "Atribut", value = "Nilai_ideal", - Consumer)
ideal

produk <-
  demoCATA %>%
  filter(Product != "Ideal") %>%
  select(-Liking) %>%
  gather(key = "Atribut", value = "Nilai_produk", -c(Consumer, Product)) %>%
  group_by(Product) %>%
  nest()
produk

produk %>%
  mutate(
    data2 = map(data, ~left_join(x = ., y = ideal))
  ) %>%
  unnest(data2) %>%
  mutate(Status = check_status(x = Nilai_produk, ref = Nilai_ideal)) %>%
  select(-c(Nilai_produk, Nilai_ideal)) %>%
  spread(key = "Atribut", value = "Status")

dat <-
  produk %>%
  mutate(
    data2 = map(data, ~left_join(x = ., y = ideal))
  ) %>%
  select(Product, data2) %>%
  unnest() %>%
  mutate(
    Status = check_status(x = Nilai_produk, ref = Nilai_ideal)
  ) %>%
  select(-c(Nilai_produk, Nilai_ideal)) %>%
  spread(key = "Atribut", value = "Status") %>%
  left_join(liking)
dat

dat %>%
gather(key = "Atribut", value ="Status", -c(Product, Consumer, Liking), convert = TRUE, factor_key = TRUE) %>%
  filter(Status == "P(No)|(Yes)" | Status == "P(Yes)|(Yes)")

res1 <-
  dat %>%
  gather(key = "Atribut", value ="Status", -c(Product, Consumer, Liking), convert = TRUE, factor_key = TRUE) %>%
  filter(Status == "P(No)|(Yes)" | Status == "P(Yes)|(Yes)") %>%
  mutate(
    Status = factor(Status, levels = c("P(Yes)|(Yes)", "P(No)|(Yes)"))
  ) %>%
  group_by(Atribut) %>%
  nest(.key = "Data") %>%
  mutate(Model = map(Data, ~ aov(Liking ~ Status, data = .)),
         ANOVA = map(Model, summary.lm),
         Tidy_ANOVA = map(ANOVA, tidy)) %>%
  select(Atribut, Tidy_ANOVA) %>%
  unnest() %>%
  select(Atribut, term, estimate, p.value) %>%
  filter(term != '(Intercept)') %>%
  mutate(estimate = -1*estimate) %>%
  mutate(
    term = str_remove(term, "Status")
  ) %>%
  rename(
    Liking_drop = estimate,
    P.value = p.value,
    Label = term
  )
res1

res2 <-
  dat %>%
  gather(key = "Atribut", value ="Status", -c(Product, Consumer, Liking)) %>%
  filter(Status == "P(Yes)|(No)" | Status == "P(No)|(No)") %>%
  mutate(
    Status = factor(Status, levels = c("P(No)|(No)", "P(Yes)|(No)"))
  ) %>%
  group_by(Atribut) %>%
  nest(.key = "Data") %>%
  mutate(Model = map(Data, ~ aov(Liking ~ Status, data = .)),
         ANOVA = map(Model, summary.lm),
         Tidy_ANOVA = map(ANOVA, tidy)) %>%
  select(Atribut, Tidy_ANOVA) %>%
  unnest() %>%
  select(Atribut, term, estimate, p.value) %>%
  filter(term != '(Intercept)') %>%
  mutate(estimate = 1*estimate) %>%
  mutate(
    term = str_remove(term, "Status")
  ) %>%
  rename(
    Liking_drop = estimate,
    P.value = p.value,
    Label = term
  )
res2

dat %>%
  gather(key = "Atribut", value ="Status", -c(Product, Consumer, Liking)) %>%
  count(Atribut, Status) %>%
  complete(Atribut, Status, fill = list(n = 0)) %>%
  add_count(Atribut, wt = n, name = "nn") %>%
  mutate(Freq = n/nn) %>%
  select(-c(n, nn))

freq <-
  dat %>%
  gather(key = "Atribut", value ="Status", -c(Product, Consumer, Liking)) %>%
  count(Atribut, Status) %>%
  complete(Atribut, Status, fill = list(n = 0)) %>%
  add_count(Atribut, wt = n, name = "nn") %>%
  mutate(Frequency = n/nn) %>%
  select(-n, -nn) %>%
  mutate(
    Status = str_c("Freq_", Status)
  ) %>%
  spread("Status", "Frequency") %>%
  select(Atribut, `Freq_P(No)|(Yes)`, `Freq_P(Yes)|(No)`) %>%
  gather("Label", "Frekuensi", -Atribut) %>%
  mutate(
    Label = str_remove(Label, "Freq_")
  )
freq

df <-
  bind_rows(res1, res2) %>%
  complete(Atribut, Label) %>%
  left_join(freq) %>%
  mutate(
    Atribut =  if_else(!is.na(P.value) & P.value <= 0.05, paste0(Atribut, "*"), Atribut)
  )
df

res_plot <-
  df %>%
  ggplot(aes(Frekuensi, Liking_drop, col = Label)) +
  geom_point() +
  geom_text_repel(aes(label = Atribut), show.legend = FALSE) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0.05, lty = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    x = "Frequency",
    y = "Mean liking drops",
    colour = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
res_plot

res1 %>%
  ggplot(aes(x = fct_rev(fct_reorder(Atribut, Liking_drop)), y = Liking_drop)) +
  geom_col() +
  coord_flip()


res2 %>%
  ggplot(aes(x = fct_rev(fct_reorder(Atribut, Liking_drop)), y = Liking_drop)) +
  geom_col() +
  coord_flip()

df %>%
  ggplot(aes(x = fct_rev(fct_reorder(Atribut, Liking_drop)), y = Liking_drop, fill = Label)) +
  geom_col() +
  coord_flip()
