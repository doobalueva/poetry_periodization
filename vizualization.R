library(tidyverse)


df <- read_tsv(file.choose())

#визуализация бинарных вариантов
df %>% 
  group_by(year) %>% 
  summarise(n_words = sum(nwords),
            n_texts = n()) %>% 
  mutate(period1_start = 1908,
         period2_start = 1913,
         period3_start = 1916,
         period4_start = 1921,
         period5_start = 1924) %>% 
  pivot_longer(names_to = "period_type", values_to = "pyear", period1_start:period5_start) %>% 
  separate(period_type, into = c("period", "start_end"), sep = "_") %>% 
  mutate(variant_name = case_when(
    .$period == "period1"~ "Вариант 1",
    .$period == "period2"~ "Вариант 2",
    .$period == "period3"~ "Вариант 3",
    .$period == "period4"~ "Вариант 4",
    .$period == "period5"~ "Вариант 5"
  )) %>% 
  pivot_wider(names_from = start_end, values_from = pyear) %>%
  ggplot(aes(year, n_words, label = n_texts))+
  geom_point()+
  ggrepel::geom_text_repel()+
  geom_vline(aes(xintercept = start), linetype = 2,size = 1.2)+
  #geom_vline(aes(xintercept = end), linetype = 2,size = 1.2)+
  facet_wrap(~variant_name, ncol = 1, scales = "free")+
  scale_x_continuous(breaks=c(seq(1899,1931,2))) +
  theme_bw()+
  labs(x = "", y = "Количество слов за год")+
  theme(axis.title = element_text(size = 13),
        axis.title.y = element_text(),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        strip.text = element_text(size = 13),
        strip.background = element_rect())

#визуализация тройных вариантов
df %>% 
  group_by(year) %>% 
  summarise(n_words = sum(nwords),
            n_texts = n()) %>% 
  mutate(period6_start = 1909,
         period6_end = 1911,
         period7_start = 1911,
         period7_end = 1927,
         period8_start = 1912,
         period8_end = 1927,
         period9_start = 1916,
         period9_end = 1918) %>% 
  pivot_longer(names_to = "period_type", values_to = "pyear", period6_start:period9_end) %>% 
  separate(period_type, into = c("period", "start_end"), sep = "_") %>% 
  mutate(variant_name = case_when(
    .$period == "period6"~ "Вариант 6",
    .$period == "period7"~ "Вариант 7",
    .$period == "period8"~ "Вариант 8",
    .$period == "period9"~ "Вариант 9"
  )) %>% 
  pivot_wider(names_from = start_end, values_from = pyear) %>%
  ggplot(aes(year, n_words, label = n_texts))+
  geom_point()+
  ggrepel::geom_text_repel()+
  geom_vline(aes(xintercept = start), linetype = 2,size = 1.2)+
  geom_vline(aes(xintercept = end), linetype = 2,size = 1.2)+
  facet_wrap(~variant_name, ncol = 1, scales = "free")+
  scale_x_continuous(breaks=c(seq(1899,1931,2))) +
  theme_bw()+
  labs(x = "", y = "Количество слов за год")+
  theme(axis.title = element_text(size = 13),
        axis.title.y = element_text(),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        strip.text = element_text(size = 13),
        strip.background = element_rect())

