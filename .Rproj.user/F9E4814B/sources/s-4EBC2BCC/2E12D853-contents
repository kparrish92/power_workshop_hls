library(tidyverse)

sample_s = data.frame("score" = rnorm(n = 500, m = 70, sd = 25), 
                      "participant" = 1:1000)
sample_m = data.frame("score" = rnorm(n = 500, m = 77.5, sd = 25), 
                      "participant" = 1:1000)
sample_l = data.frame("score" = rnorm(n = 500, m = 85, sd = 25), 
                      "participant" = 1:1000)


draw_random_sample(sample_s, 10)

draw_random_sample(sample_m, 10)

draw_random_sample(sample_l, 10)


draw_random_sample = function(full_df, n)
{

slice = full_df %>% 
  sample_n(n)

sample_highlight = full_df %>% 
  mutate(sampled = case_when(
    participant %in% slice$participant ~ 1,
    !participant %in% slice$participant ~ 0))

sample_highlight_m = sample_highlight %>% 
  filter(sampled == "1")

mean(sample_highlight_m$score)
sd(sample_highlight_m$score)

p = ggplot(sample_highlight, aes(x = score, fill = as.factor(sampled))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 2, binpositions = "all") +
  annotate(geom = 'text', 
           label = paste0("Mean = ", 
                          round(mean(sample_highlight_m$score), 
                                digits = 2), " (", 
                          round(sd(sample_highlight_m$score), 
                                digits = 2), ")"), 
           x = -Inf, y = Inf, hjust = -.5, vjust = 2) +
  theme(legend.position = "none") 

return(p)
}




draw_random_sample(sample_s, 15)

