df <- read.csv("model_data.csv", header = TRUE)
df$city <- as.factor(df$city)
df$gender <- as.factor(df$gender)
df$name <- as.factor(df$name)
df$race_detailed <- as.factor(df$race_simplified)

attach(df)

#visualization of percen of gendered language by candidate

barplot(df$percen_gen~df$name, col=df$gender, border=df$city,
        main="Percentage of Gendered Language by Candidate",
        ylab="Percentage of Candidate-Specific Queries",
        xlab = '',
        las=2,
        cex.names = 0.6)



#remove outlier 
df.noout <- df[-17,]
#some visualizations
plot(log(percen_gen), log(avg_len))
pairs(df[c('num_queries','num_queries_unique','avg_len'
           ,'percen_gen')])


#MODEL FITTING: predict the percentage of gendered language 
glm <- glm(percen_gen~ num_queries + num_queries_unique  + avg_len +
               name,
          data=df.noout, family=binomial('logit'))

glm2 <- glm(percen_gen~num_queries+num_queries_unique+avg_len+
              gender,
            data=df.noout, family=binomial('logit'))

summary(glm)

summary(glm2)


plot(glm2)

BIC(glm)
BIC(glm2)

