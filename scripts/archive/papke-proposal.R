# Graphs for the Papke proposal 

# load data files

electiondata = read.csv("results/weekly updates/local-options-elections-1997-2020-cleaned.csv")


##---------------------------------------------------------------
##                  elections over time (graph)                 -
##---------------------------------------------------------------

summary_elections_overall = electiondata %>% 
  filter(!is.na(result)) %>% 
  group_by(fiscal_year, result) %>% 
  summarise(
    total = n()
  )%>% 
  mutate(
    group = factor(result, levels = c("passed","failed"))
  )

ggplot(summary_elections_overall, aes(x = fiscal_year, y = total, fill = result)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("passed" = "gray20", "failed" = "gray90")) +
  theme_minimal() +
  labs(
    x = "",
    y = "Number ofelections"
  ) +
  theme(legend.position = "bottom")

ggsave("results/papke-proposal/N_elections_overall.pdf", device = "pdf")


##----------------------------------------------------------------
##                status changes over time (graph)               -
##----------------------------------------------------------------

temp = electiondata %>% filter(result == "passed") %>% 
  group_by(fiscal_year) %>% 
  summarise(total = n_distinct(jurid))

ggplot(temp, aes(x = fiscal_year, y = total)) +
  geom_bar(stat = "identity", fill = "gray20") +
  theme_minimal() +
  labs(
    x = "",
    y = "Number of status changes"
  ) +
  theme(legend.position = "bottom")

ggsave("results/papke-proposal/status_changes.pdf", device = "pdf")


##----------------------------------------------------------------
##                    type of status changes                     -
##----------------------------------------------------------------

type = c("Dry to Wet", "Wet to More Wet", "Wet to Dry")

statuschangesonly = electiondata %>% filter(result=="passed") %>% 
  group_by(fiscal_year) %>% 
  distinct(jurid, .keep_all = T)

n = c(sum(statuschangesonly$status_before=="dry" & statuschangesonly$status_current == "wet", na.rm = T),
      sum(statuschangesonly$status_before=="wet" & statuschangesonly$status_current == "wet", na.rm = T),
      sum(statuschangesonly$status_before=="wet" & statuschangesonly$status_current == "dry", na.rm = T))

data.frame(type,n)


##---------------------------------------------------------------
##                          vote shares                         -
##---------------------------------------------------------------

temp = electiondata %>% 
  group_by(fiscal_year) %>% 
  filter(!is.na(result)) %>% 
  filter(prohibitory==0) %>% 
  mutate(total_votes = for_vote+against_vote,
         for_share = for_vote/total_votes,
         close1 = ifelse(for_share >= .40 & for_share<= .60, 1, 0),
         close2 = ifelse(for_share >= .45 & for_share<= .55, 1, 0),
         close3 = ifelse(for_share >= .48 & for_share<= .52, 1, 0),
  )

ggplot(temp, aes(x=for_share)) + 
  geom_histogram(fill = "gray80", color = "black") +
  #geom_vline(xintercept = .45, linetype = "dashed", color = "black", size = 1) +
  #geom_vline(xintercept = .55, linetype = "dashed", color = "black", size = 1) +
  labs(x= "Share of FOR votes", y = "Number of elections") +
  theme_minimal()

ggsave("results/papke-proposal/forvotes.pdf", device = "pdf")
