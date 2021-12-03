#str(HR)
psych::headTail(HR)
distinct(HR,company_size) #9
#distinct(HR,company_type) #7
#distinct(HR,city) #123
#distinct(HR,last_new_job) #7
#distinct(HR,experience) #23
#distinct(HR,major_discipline) #7
#distinct(HR,training_hours) #241
distinct(HR,education_level) #6
#distinct(HR_2v$education_level)

p1<-ggplot(HR, aes(x = training_hours)) +
  geom_histogram(bins = 20, color = "black", fill = "powderblue") +
  theme_light() +
  facet_wrap(vars(target)) +
  #theme(strip.background = element_rect(fill = "darkred")) +
  #theme(strip.text = element_text(colour = 'white', face = "bold")) +
  labs(
    title = "Training hours by Target ",
    subtitle = "0–Not looking for job change, 1–Looking for a job change",
    x = "Training hours",
    y = "Count"
  )

p2<-ggplot(HR, aes(x = city_development_index)) +
  geom_histogram(bins = 20, color = "black", fill = "thistle") +
  theme_light() +
  facet_wrap(vars(target)) +
  #theme(strip.background = element_rect(fill = "darkred")) +
  #theme(strip.text = element_text(colour = 'white', face = "bold")) +
  labs(title = "City Development Index by Target ",
       subtitle = "0–Not looking for job change, 1–Looking for a job change",
       x = "City Development Index",
       y = "Count") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5))


HR_edu = HR
HR_edu[HR_edu == ""] = NA 
HR_edu$target = as.factor(HR_edu$target)
p3<-HR_edu%>% 
  group_by(education_level,target) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count),
         label = percent(ratio %>% round(3))) %>% 
  mutate(education_level = factor(education_level)) %>% 
  mutate(target = factor(target)) %>%
  drop_na() %>%
  ggplot(aes(education_level, ratio, fill = target)) + 
  geom_bar(stat = "identity", color = 'black') +
  scale_fill_manual(values = c('darkolivegreen3', 'lightgoldenrod1'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5)) +
  labs(title = "Relationship between education level and job search")


p4<-HR_edu%>% 
  group_by(major_discipline,target) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count),
         label = percent(ratio %>% round(3))) %>% 
  mutate(major_discipline = factor(major_discipline)) %>% 
  mutate(target = factor(target)) %>%
  drop_na() %>%
  ggplot(aes(major_discipline, ratio, fill = target)) + 
  geom_bar(stat = "identity", color = 'black') +
  scale_fill_manual(values = c('darkolivegreen3', 'lightgoldenrod1'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5)) +
  labs(title = "Relationship between major discipline and job search")



p5<-HR_edu%>% 
  group_by(enrolled_university,target) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count),
         label = percent(ratio %>% round(3))) %>% 
  mutate(enrolled_university = factor(enrolled_university)) %>% 
  mutate(target = factor(target)) %>%
  drop_na() %>%
  ggplot(aes(enrolled_university, ratio, fill = target)) + 
  geom_bar(stat = "identity", color = 'black') +
  scale_fill_manual(values = c('darkolivegreen3', 'lightgoldenrod1'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30)) +
  labs(title = "Relationship between enrolled_university and job search")



p6<-HR_edu%>% 
  group_by(company_size,target) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count),
         label = percent(ratio %>% round(3))) %>% 
  mutate(company_size = factor(company_size)) %>% 
  mutate(target = factor(target)) %>%
  drop_na() %>%
  ggplot(aes(company_size, ratio, fill = target)) + 
  geom_bar(stat = "identity", color = 'black') +
  scale_fill_manual(values = c('darkolivegreen3', 'lightgoldenrod1'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30)) +
  labs(title = "Relationship between company size and job search")

p7<-HR_edu%>% 
  group_by(last_new_job,target) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / sum(count),
         label = percent(ratio %>% round(3))) %>% 
  mutate(last_new_job = factor(last_new_job)) %>% 
  mutate(target = factor(target)) %>%
  drop_na() %>%
  ggplot(aes(last_new_job, ratio, fill = target)) + 
  geom_bar(stat = "identity", color = 'black') +
  scale_fill_manual(values = c('darkolivegreen3', 'lightgoldenrod1'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5)) +
  labs(title = "Relationship between last new job and job search")

#ggarrange(p3,p4,p5,p6,p7, 
#          labels = c("education level", "major discipline", "enrolled university", "company type", "last new job"),
#          ncol = 2, nrow = 3,
#          common.legend = TRUE)
p1
p2
p3#education_level
p4
p5
p6
p7

HR <- read.csv("aug_train.csv", header = T)
HR$experience<-gsub(c(">","<"),"",as.character(HR$experience))
HR$experience<-gsub("<1","1",as.character(HR$experience))
HR$company_size<-gsub("/","-",as.character(HR$company_size))
HR$experience <- as.numeric(HR$experience)
#str(HR)

HR$education_level[HR$education_level == ""] <- "No_answer"
HR$company_size[HR$company_size == ""] <- "No_answer"

####Heatmap coef
#ggpairs(HR, c(3,9,10,13,14))
hr_num <-na.omit(HR[,c(3,9,13,14)])
hr_num$training_hours <- as.numeric(hr_num$training_hours)
cormat <- round(cor(hr_num),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

correlationmap<-
  ggheatmap+ 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#Intention of changing a job by education level and company size
p_2v<-
  HR%>% group_by(company_size, education_level) %>%
  summarize(mean = mean(target)) %>% ungroup() %>%
  ggplot() + 
  geom_point(aes(x = company_size,y = mean, color = company_size),size=4) + 
  theme_bw() + 
  xlab("company size") +   ylab("target") + 
  ggtitle("Target under different Family Status and company size") + 
  facet_wrap(~ education_level) +
  theme(axis.text.x = element_text(angle = 50, hjust = 0.5, vjust = 0.5))+
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5))


line1<-ggplot(data = HR)+
  aes(y = target, x = training_hours)+
  geom_point(aes(color = company_size),alpha = 0.3)+
  labs(title="Relationship between training hour & intention to leave\n by company size and education level",
       x="Training Hours", y = "Target")+
  geom_smooth(aes(color = company_size),method = "lm",se=F)+
  facet_grid(~education_level)

line2<-ggplot(data = HR)+
  aes(y = target, x = city_development_index)+
  geom_point(aes(color = company_size),alpha = 0.3)+
  labs(title="Relationship between city development index & intention to leave\n by company size and education level",
       x="City Development Index", y = "Target")+
  geom_smooth(aes(color = company_size),method = "lm",se=F)+
  theme(axis.text.x = element_text(angle = 50, hjust = 0.5, vjust = 0.5))+
  facet_grid(~education_level)
