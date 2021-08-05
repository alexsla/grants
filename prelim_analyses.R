library(tidyverse)

dat <- read_csv("Data/Research-Grants.csv", locale = locale(encoding = "Windows-1255"))

fields <- dat %>%
  mutate(Field = recode(Field,
                        "Bioinformatics & Computational & systems Biology" = "Bioinformatics",
                        "Computational & Systems Biology" = "Bioinformatics")) %>%
  group_by(Field) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  filter(count > 10,
         str_detect(Field, regex('medicin', ignore_case = T),
                    negate = T)) %>%
  select(Field)
fields <- fields$Field

pdf("Plots/field_by_time.pdf", useDingbats = F, height = 20, width = 5)
dat %>%
  mutate(Field = recode(Field,
                        "Bioinformatics & Computational & systems Biology" = "Bioinformatics",
                        "Computational & Systems Biology" = "Bioinformatics")) %>%
  group_by(Year, Field) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  drop_na() %>%
  filter(Field %in% fields) %>%
  ggplot(aes(x = as.numeric(Year),
             y = freq,
             fill = Field,
             colour = Field)) +
  geom_point() +
  geom_smooth(method = "loess",
              span = 1,
              se = F) +
  facet_grid(rows = vars(Field),
             scales = "free_y") +
  theme(legend.position = "none")
dev.off()

pdf("Plots/field_freq.pdf", useDingbats = F)
dat %>%
  mutate(Field = recode(Field,
                        "Bioinformatics & Computational & systems Biology" = "Bioinformatics",
                        "Computational & Systems Biology" = "Bioinformatics")) %>%
  group_by(Year, Field) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  drop_na() %>%
  filter(Field %in% fields) %>%
  ggplot(aes(x = Field,
             y = freq,
             fill = Field)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


pdf("Plots/field_funding_by_time.pdf", useDingbats = F)
dat %>%
  mutate(Field = recode(Field,
                        "Bioinformatics & Computational & systems Biology" = "Bioinformatics",
                        "Computational & Systems Biology" = "Bioinformatics")) %>%
  drop_na() %>%
  filter(Field %in% fields) %>%
  ggplot(aes(x = as.numeric(Year),
             y = Per_annum_budget,
             fill = Field,
             colour = Field)) +
  geom_point() +
  geom_smooth(method = "loess",
              span = 1,
              se = F) +
  theme(legend.position = "bottom")
dev.off()

pdf("Plots/field_funding.pdf", useDingbats = F)
dat %>%
  mutate(Field = recode(Field,
                        "Bioinformatics & Computational & systems Biology" = "Bioinformatics",
                        "Computational & Systems Biology" = "Bioinformatics")) %>%
  group_by(Year, Field) %>%
  drop_na() %>%
  filter(Field %in% fields) %>%
  ggplot(aes(x = Field,
             y = Per_annum_budget,
             fill = Field)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()