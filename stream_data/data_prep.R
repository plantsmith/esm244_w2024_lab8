library(tidyverse)
library(here)

stream_data_subset <- read_csv(here('stream_data', 'sbc_lter_registered_stream_chemistry.csv'),
                            na = '-999') %>%
  select(-contains(c('tss'))) %>%
  drop_na() %>%
  filter(nh4_uM > 0 & no3_uM > 0 & po4_uM > 0 & tdn_uM > 0 & tdp_uM > 0 & spec_cond_uSpercm > 0) %>%
  filter(tpp_uM > 0)

write_csv(stream_data_subset, here('data/stream_data_subset.csv'))

ggplot(stream_data_subset %>% pivot_longer(cols = where(is.numeric))) +
  geom_histogram(aes(x = value)) +
  facet_wrap(~name, scales = 'free')

stream_pca <- stream_data_subset %>%
  select(-site_code, -timestamp_local) %>%
  mutate(across(where(is.numeric), ~log(.x))) %>%
  drop_na() %>%
  prcomp(scale = TRUE)

autoplot(stream_pca, loadings = TRUE, loadings.label = TRUE)
