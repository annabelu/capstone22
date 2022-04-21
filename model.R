library('lattice')
library(RColorBrewer)
df <- read.csv("../Untitled Folder/model_data.csv", header = TRUE)
df$city <- as.factor(df$city)
df$gender <- as.factor(df$gender)
df$name <- as.factor(df$name)
df$race_detailed <- as.factor(df$race_detailed)

attach(df)

#visualization of percen of gendered language by candidate

#brewer.pal(n = 3, name = "BrBG")
palette(brewer.pal(n = 3, name = "Pastel2"))

df1 <- df[order(df$percen_gen,decreasing=TRUE),]

barplot(df1$percen_gen#~df1$name, 
        ,col=df1$gender, #c('#0081A7','#FDFCDC','#F07167'), #, border=,
        main="Percentage of Gendered Language by Candidate",
        ylab="Percentage of Candidate-Specific Queries",
        xlab = '',
        las=2,
        cex.names = 0.6,
        names.arg = df1$name)

legend(20, 0.8, legend = c('Women','Men','Nonbinary'),
       fill = c('#B3E2CD','#FDCDAC','#CBD5E8'),
       col=c('#B3E2CD','#FDCDAC','#CBD5E8'),
       cex=0.8)
       
       #col=df1$gender)
               #c(palette(brewer.pal(n = 3, name = "BrBG"))))


##now ordered by gender

df2 <- df[order(df$gender),]

barplot(df2$percen_gen#~df1$name, 
        ,col=df2$gender, border=df2$gender,
        main="Percentage of Gendered Language by Candidate",
        ylab="Percentage of Candidate-Specific Queries",
        xlab = '',
        las=2,
        cex.names = 0.6,
        names.arg = df2$name)



#----------------------------------------------------
#
#barchart(percen_gen~name,#~name,
#         main="Percentage of Gendered Language by Candidate",
#         ylab="Percentage of Candidate-Specific Queries",
#         data = df1,
#         xlab = '',
#         las=2,
#         cex.names = 0.6,
#         groups=df$gender)


#remove outlier 
df.noout <- df[-17,]
#some visualizations
plot(log(percen_gen), log(avg_len))
pairs(df[c('num_queries','num_queries_unique','avg_len'
           ,'percen_gen')])



#MODEL FITTING: predict the percentage of gendered language 
#DO NOT USE THIS!
glm <- glm(percen_gen~ num_queries + num_queries_unique  + avg_len +
               name,
          data=df.noout, family=binomial('logit'))


glm2 <- glm(percen_gen~num_queries+num_queries_unique+avg_len+
              gender,
            data=df.noout, family=binomial('logit'))

summary(glm)

summary(glm2)


####FINAL MODEL HERE: BINOMIAL LOGISITIC REGRESSION
df.noout$not_gen <- df.noout$num_queries - df.noout$num_gen
row.names(df.noout) <- df.noout$name
#glm(as.matrix(df[,c("num_gen","not_gen")])~city+num_queries+num_queries_unique+avg_len+gender,
#    df=df.noout,family=binomial('logit')) #don't use this

#model with gender
glm.obj <- glm(cbind(df.noout$num_gen,df.noout$not_gen)~city+num_queries+
                   num_queries_unique+avg_len+gender,
    data=df.noout,family=binomial('logit'))
summary(glm.obj)

#model without gender
glm.obj2 <- glm(cbind(df.noout$num_gen,df.noout$not_gen) ~ city+num_queries+
                       num_queries_unique+avg_len,
               data=df.noout,family=binomial('logit'))
summary(glm.obj2)

plot(glm.obj,xlab='Predicted Values')

#######
AIC(glm.obj)
AIC(glm.obj2)
BIC(glm.obj)
BIC(glm.obj2)

palette(brewer.pal(n = 3, name = "Set1"))
plot(df.noout$num_queries,df.noout$percen_gen,col=df.noout$gender)
plot(df.noout$num_queries_unique,df.noout$percen_gen,col=df.noout$gender,
     main="Percentage of Gendered Language by Gender",
     xlab="Number of Unique Candidate Results",
     ylab="% of Gendered Candidate-Specific Results")

legend(380, 0.32, legend = c('Women','Men'),
       fill = c('#E41A1C','#377EB8'),
       col=c('#E41A1C','#377EB8'),
       cex=0.8)
