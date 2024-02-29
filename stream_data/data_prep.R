library(tidyverse)
library(here)
library(ggfortify)

set.seed(12345)
stream_data_raw <- read_csv(here('stream_data', 'sbc_lter_registered_stream_chemistry.csv'),
                            na = '-999') 

stream_data_subset <- stream_data_raw %>%
  select(-contains(c('tss'))) %>%
  pivot_longer(cols = c(-site_code, -timestamp_local)) %>%
  filter(value > 0) %>%
  group_by(site_code, timestamp_local, name) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider() %>%
  drop_na(tdp_uM, nh4_uM) %>%
  sample_frac(0.2)


summary(stream_data_subset)

write_csv(stream_data_subset, here('data/stream_data_subset.csv'))

stream_data_nas <- stream_data_subset %>%
  pivot_longer(where(is.numeric)) %>%
  mutate(year = year(timestamp_local)) %>%
  mutate(value = is.na(value)) %>%
  group_by(site_code, year) %>%
  summarize(n_na = sum(value),
            p_na = sum(value) / n())
ggplot(stream_data_nas, aes(x = year, y = p_na)) +
  geom_line() +
  facet_wrap(~site_code)


ggplot(stream_data_subset %>% pivot_longer(cols = where(is.numeric))) +
  geom_histogram(aes(x = value)) +
  facet_wrap(~name, scales = 'free')

stream_pca <- stream_data_subset %>%
  select(-site_code, -timestamp_local) %>%
  mutate(across(where(is.numeric), ~log(.x))) %>%
  drop_na() %>%
  prcomp(scale = TRUE)

autoplot(stream_pca, loadings = TRUE, loadings.label = TRUE)
screeplot(stream_pca, type = 'lines')
