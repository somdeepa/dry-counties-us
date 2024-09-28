
##----------------------------------------------------------------
##                        R-D style graphs                       -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/licenses-elections/RDpanel_quaterly_city.csv")

time_periods = c(-1,4)

bin = 1
temp = data %>% 
  filter(
    periods_from_election %in% time_periods
  ) %>% 
  #(!city %in% c("Winona", "Coffee City") ) %>% 
  #filter(licensepop<5) %>% 
  #filter(for_vote_share<.92) %>% 
  # binning vote shares
  mutate(for_vote_share = floor(for_vote_share*100/bin)*bin) %>% 
  #filter(for_vote_share==31)
  #filter(between(for_vote_share, 20, 80)) %>% 
  group_by(for_vote_share, periods_from_election) %>% 
    summarise(
      n = n(),
      mean_licensepop = mean(licensepop, na.rm = T),
      Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
      Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    )
  



ggplot(temp) +
  #geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(mapping = aes(x = for_vote_share, ymin = Lower, ymax = Upper),
  #             width = 0.3, linewidth =0.5) +
  geom_point(aes(x = for_vote_share, y = mean_licensepop), size = 1, shape = 21, fill = "grey") +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share < 50), 
              color = "black", se = T) +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share >= 50), 
              color = "black", se = T) +
  labs(x = "For vote percentage",
       y = "Licenses per 1000 population") +
  facet_wrap(~periods_from_election, ncol = 2,
             labeller = labeller(periods_from_election = function(x) {
               paste("Quarters from election =", x)
             })) +
  theme_bw(base_size = 16)

ggsave("results/weekly updates/11-9/rd_style_full.png", device = "png")


# on-premise

data = read.csv("data/clean/merged/licenses-elections/RDpanel_quaterly_city_on_premise.csv")

time_periods = c(-1,4)

bin = 1
temp = data %>% 
  filter(
    periods_from_election %in% time_periods
  ) %>% 
  # binning vote shares
  mutate(for_vote_share = floor(for_vote_share*100/bin)*bin) %>% 
  group_by(for_vote_share, periods_from_election) %>% 
  summarise(
    n = n(),
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )




ggplot(temp) +
  #geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(mapping = aes(x = for_vote_share, ymin = Lower, ymax = Upper),
  #             width = 0.3, linewidth =0.5) +
  geom_point(aes(x = for_vote_share, y = mean_licensepop), size = 1, shape = 21, fill = "grey") +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share < 50), 
              color = "black", se = T) +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share >= 50), 
              color = "black", se = T) +
  labs(x = "For vote percentage",
       y = "On-premise licenses per 1000 population") +
  facet_wrap(~periods_from_election, ncol = 1,
             labeller = labeller(periods_from_election = function(x) {
               paste("Quarters from election =", x)
             })) +
  scale_y_continuous(limits = c(-0.5,4.5)) +
  theme_bw(base_size = 16)

ggsave("results/weekly updates/25-9/rd_style_full_on_premise.png", device = "png")

# off-premise

data = read.csv("data/clean/merged/licenses-elections/RDpanel_quaterly_city_off_premise.csv")

time_periods = c(-1,4)

bin = 1
temp = data %>% 
  filter(
    periods_from_election %in% time_periods
  ) %>% 
  # binning vote shares
  mutate(for_vote_share = floor(for_vote_share*100/bin)*bin) %>% 
  filter(between(for_vote_share, 20, 80)) %>% 
  group_by(for_vote_share, periods_from_election) %>% 
  summarise(
    n = n(),
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(temp) +
  #geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(mapping = aes(x = for_vote_share, ymin = Lower, ymax = Upper),
  #             width = 0.3, linewidth =0.5) +
  geom_point(aes(x = for_vote_share, y = mean_licensepop), size = 1, shape = 21, fill = "grey") +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share < 50), 
              color = "black", se = T) +
  geom_smooth(aes(x = for_vote_share, y = mean_licensepop), 
              method = "lm", formula = y ~ x, 
              data = subset(temp, for_vote_share >= 50), 
              color = "black", se = T) +
  labs(x = "For vote percentage",
       y = "Off-premise licenses per 1000 population") +
  facet_wrap(~periods_from_election, ncol = 1,
             labeller = labeller(periods_from_election = function(x) {
               paste("Quarters from election =", x)
             })) +
  scale_y_continuous(limits = c(-0.5,4.5)) +
  theme_bw(base_size = 16)

ggsave("results/weekly updates/25-9/rd_style_full_off_premise.png", device = "png")

