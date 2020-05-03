

EuroCrime  <- read.csv(file="EurostatCrime2017.csv", row.names=1,header=TRUE, sep=",")


dim(EuroCrime)


All_Theft_Data = EuroCrime[,c('Theft','Theft_of_a_motorized_land_vehicle','Burglary','Burglary_of_private_residential_premises')]
EuroCrime$All_Theft <- rowSums(All_Theft_Data,na.rm=TRUE)

drop_columns <- c('Theft','Theft_of_a_motorized_land_vehicle','Burglary','Burglary_of_private_residential_premises')
data<-EuroCrime[ , !(names(EuroCrime) %in% drop_columns)]




missing_countries = unique(unlist(lapply(data,function(data)which(is.na(data)))))
names=row.names(data)
names[missing_countries]

EuroCrime_new <- data[complete.cases(data),]
dim(EuroCrime_new)


Ireland_crimes = subset(EuroCrime_new,row.names(EuroCrime_new) == "Ireland")
colnames(sort(Ireland_crimes[,-1], decreasing = TRUE)[1:3])

EuroCrime_new$All_Offences <- rowSums(EuroCrime_new[,-1])
country_with_most_offense <- EuroCrime_new[order(-EuroCrime_new$All_Offences),][1,]
row.names(country_with_most_offense)



library("ggpubr")
ggscatter(EuroCrime_new,x="Kidnapping",y="Sexual.violence",
          add ="reg.line",conf.int=TRUE,
          cor.coef = TRUE, cor.method= "pearson")




ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
#