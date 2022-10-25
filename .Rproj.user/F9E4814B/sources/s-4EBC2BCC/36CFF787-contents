##### Simulate data set once? then sample. 


#calculate Cohen's d
mean1 = 38
mean2 = 30
sd1 = 20
sd2 = 20
n1 = 10  
n2 = 10
i = 1000
sample_sizes = c(10,20,30,40,50,60,70,80,90,100)

effect_size(mean1, mean2, sd1, sd2)

set.seed(1)
#### Rnorm resimulates the underlying distribution, I want to sample from it. 

s1 = rnorm(n = 10000, mean = mean1, sd = sd1)
s2 = rnorm(n = 10000, mean = mean2, sd = sd2)


df = eff_size_df_sample(s1, s2, 1000, 50, 50) %>% 
  as.data.frame() %>% 
  rename("es" = V1) %>% 
  rename("mean_1" = V2) %>% 
  rename("sd_1" = V3) %>% 
  rename("mean_2" = V4) %>% 
  rename("sd_2" = V5)


df %>% 
  ggplot(aes(x = mean_1, y = sd_1)) + geom_point() + 
  geom_point(x = mean1, y = sd1, color = "red") + ylim(0,100) +
  xlim(0,100)

df %>% 
  ggplot(aes(x = mean_2, y = sd_2)) + geom_point() + 
  geom_point(x = 30, y = 20, color = "red") + ylim(0,100) +
  xlim(0,100)


df %>% 
  ggplot(aes(x = mean_1)) + geom_boxplot() + 
  geom_vline(xintercept = mean1, color = "red") 

df %>% 
  ggplot(aes(x = mean_2)) + geom_boxplot() + 
  geom_vline(xintercept = mean2, color = "red") 

df %>% 
  ggplot(aes(x = sd_1)) + geom_boxplot() + 
  geom_vline(xintercept = sd1, color = "red") 

df %>% 
  ggplot(aes(x = sd_2)) + geom_boxplot() + 
  geom_vline(xintercept = sd2, color = "red") 




df %>% 
  ggplot(aes(x = sd_1)) + geom_boxplot() + 
  geom_vline(xintercept = sd1, color = "red") 


power_analysis_single(mean1, mean2, sd1, sd2, i, n1, n2)

plot_power_curve(mean1, sd1, mean2, sd2, sample_sizes)

plot_power_curve_ul(s1, s2, i, sample_sizes)


ten = data.frame("es" = 
                   eff_size_range_single_u(s1, s2, i, 10, 10)) %>% 
  mutate(sample_size = "10")

twenty = data.frame("es" =
                      eff_size_range_single_u(s1, s2, i, 20, 20)) %>% 
  mutate(sample_size = "20")

fourty = data.frame("es" =
                      eff_size_range_single_u(s1, s2, i, 40, 40)) %>% 
  mutate(sample_size = "40")

eighty = data.frame("es" =
                      eff_size_range_single_u(s1, s2, i, 80, 80)) %>% 
  mutate(sample_size = "80")

s_150 = data.frame("es" =
                     eff_size_range_single_u(s1, s2, i, 100, 100)) %>% 
  mutate(sample_size = "100")

all = rbind(ten, twenty, fourty, eighty, s_150)

all %>% 
  ggplot(aes(x = es, y = as.factor(sample_size))) + geom_boxplot() + 
  geom_vline(xintercept = .4)


mean(ten$es)
mean(s_150$es)




v = eff_size_range_single_u(s1, s2, i, n1, n2)

mean(v)
