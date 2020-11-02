train<- read.csv(file="data/CAX_Startup_Train.csv", header=TRUE,as.is=T)
test<- read.csv(file="data/CAX_Startup_Test.csv", header=TRUE,as.is=T)
train$Category<-c("Train")
test$Category<-c("Test")


df<-rbind(train,test)
nrow(df[!complete.cases(df),])

missmap(df,col = c('yellow','black'))

df2 <-df%>%select_if(~any(.=="No"))


#Encoding ordinal categorical variables
yes.no<-names(df2)
for(i in yes.no)
{
  df[,i]<- factor(df[,i],
                 levels = c("No","Yes"))
}

df$employee.count <- factor(df$Founders_previous_company_employee_count,
                           levels = c("Small","Medium","Large"))

# 
# 
df$Founders_previous_company_employee_count <- NULL
#d$Dependent<-factor(d$Dependent)

df<-df%>%mutate_if(is.character,factor)
glimpse(df)


# Categorical Variable data encoding
df$Company_Location<- factor(df$Company_Location,
                            levels = c("USA","Europe","Other"))                            

df$Company_business_model<- factor(df$Company_business_model,
                                  levels = c("B2B","B2C","Both"))

df$Founder_education <- factor(df$Founder_education,
                              levels = c("Bachelors","Masters","PhD"))

df$Founder_highest_degree_type<- factor(df$Founder_highest_degree_type,
                                       levels = c("Management","Science","Technology","Other"))

df$Company_Product_or_service <- factor(df$Company_Product_or_service,
                                       levels = c("Product","Service","Both"))

df$Company_difficulty_obtaining_workforce<- factor(df$Company_difficulty_obtaining_workforce,
                                                  levels = c("Low","Medium","High"))

df$Founders_Industry_exposure<- factor(df$Founders_Industry_exposure,
                                      levels = c("Low","Medium","High"))

df$Founders_experience <- factor(df$Founders_experience,
                                levels = c("Low","Medium","High"))

df$Company_Industry_count <-factor(df$Company_Industry_count,
                                  levels = c("single","Few","Many"))

df$Founders_publications <- factor(df$Founders_publications,
                                  levels = c("Few","Many","None"))

df$Founders_profile_similarity <- factor(df$Founders_profile_similarity,
                                        levels = c("None","Low","Medium","High"))
df$Dependent <-factor(df$Dependent)

df$Founder_university_quality <-factor(df$Founder_university_quality)

df$Founders_Popularity <- factor(df$Founders_Popularity)
