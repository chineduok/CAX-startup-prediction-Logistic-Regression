train <- df%>%filter(Category=='Train')%>%select(-Category)
test <- df%>%filter(Category=='Test')%>%select(-Category)

# partitioning of test and train set for evaluation of models
# separating out 0 and 1 level
train_0 <- train[train$Dependent==0,]
train_1 <- train[train$Dependent==1,]


cnt_train <- train%>%select_if(is.numeric)

cnt_train$Dependent<-train$Dependent

glimpse(cnt_train)

# Plot Investment Distributions
inv.plt.1 <-cnt_train%>%
  ggplot(aes(Company_investor_count_seed, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) + 
  labs(title = 'Investor Distributions', subtitle = 'Seed Investors') 

inv.plt.2 <-cnt_train%>%
  ggplot(aes(Company_investor_count_Angel_VC, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Angel/VC Investors')

inv.plt.3 <-cnt_train%>%
  ggplot(aes(Company_repeat_investors_count, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Repeat Investors')

inv.dist.plts<- grid.arrange(inv.plt.1,inv.plt.2,inv.plt.3)
ggsave('figs/inv.dist.plts.png')


# Skills distribution

skill.score.plt1 <-cnt_train%>%
  ggplot(aes(Founders_skills_score, fill=Dependent)) +
  geom_density(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#d84242")) +
  labs(title= 'Skills Distribution',subtitle = 'Skills Score')

skill.score.plt2 <-cnt_train%>%
  ggplot(aes(Founders_Entrepreneurship_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Entrepenurship Skills of Founders')

skill.score.plt3 <-cnt_train%>%
  ggplot(aes(Founders_Operations_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Operations Skills of Founders')

skill.score.plt4 <- cnt_train%>%
  ggplot(aes(Founders_Engineering_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Engineering skills of Founders')

skill.score.plt5 <-cnt_train%>%
  ggplot(aes(Founders_Marketing_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Marketing skills of Founders')

skill.score.plt6 <-cnt_train%>%
  ggplot(aes(Founders_Leadership_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Leadership Skill of Founders')

skill.score.plt7 <- cnt_train%>%
  ggplot(aes(Founders_Data_Science_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Data Science skill of Founders')

skill.score.plt8 <-cnt_train%>%
  ggplot(aes(Founders_Business_Strategy_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Business Strategy skill of Founders')

skill.score.plt9 <- cnt_train%>%
  ggplot(aes(Founders_Product_Management_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Product Management Skill of Founders')

skill.score.plt10 <- cnt_train%>%
  ggplot(aes(Founders_Sales_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Sales skill of founders')

skill.score.plt11 <- cnt_train%>%
  ggplot(aes(Founders_Domain_skills_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Domain skill of founders')


grid.arrange(skill.score.plt1,skill.score.plt2,skill.score.plt3,skill.score.plt4,skill.score.plt5,
             skill.score.plt6,skill.score.plt7,skill.score.plt8,skill.score.plt9,skill.score.plt10, skill.score.plt11)


names(cnt_train)


other.plt1 <- cnt_train%>%
  ggplot(aes(Founder_university_quality, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(title = 'Other Founder factors',subtitle = 'University quality of founders')

other.plt2 <- cnt_train%>%
  ggplot(aes(Founders_Popularity, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Popularity of founders')

other.plt3 <- cnt_train%>%
  ggplot(aes(Founders_fortune1000_company_score, fill=Dependent)) +
  geom_histogram(alpha=0.5)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#7a2477")) +
  labs(subtitle = 'Fortune 100 experience')

unique(cnt_train$Founders_fortune1000_company_score)
