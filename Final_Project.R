attach(listings)

listings$superhost=ifelse(host_is_superhost=="t",1,0)
attach(listings)
table(superhost)

listings$profilepic=ifelse(host_has_profile_pic=="t",1,0)
attach(listings)
table(profilepic)

listings$verified_host=ifelse(host_identity_verified=="t",1,0)
attach(listings)
table(verified_host)

listings$book_instantly=ifelse(instant_bookable=="t",1,0)
attach(listings)
table(book_instantly)

listings$verify=ifelse(require_guest_phone_verification=="t",1,0)
attach(listings)
table(verify)

listings$has_TV=ifelse(TV=="TRUE",1,0)
attach(listings)
table(has_TV)

listings$has_Wifi=ifelse(Has.Wifi.=="TRUE",1,0)
attach(listings)
table(has_Wifi)

listings$host_greets=ifelse(Does.Host.Greet.=="TRUE",1,0)
attach(listings)
table(host_greets)

listings$cancellation_policy=as.factor(listings$cancellation_policy)
levels(cancellation_policy)
cancellation_policy=relevel(cancellation_policy, ref="flexible")

listings$host_response_time=as.factor(listings$host_response_time)
levels(host_response_time)
host_response_time=relevel(host_response_time, ref="within an hour")


listings$neighbourhood.1=as.factor(listings$neighbourhood.1)
levels(neighbourhood.1)
neighbourhood.1=relevel(neighbourhood.1, ref="I Arrondissement")

lm.fit=lm(review_scores_rating~host_response_time)
summary(lm.fit)

lm.fit1=lm(review_scores_rating~host_response_rate)
summary(lm.fit1)

lm.fit2=lm(review_scores_rating~host_is_superhost)
summary(lm.fit2)

lm.fit3=lm(review_scores_rating~minimum_nights)
summary(lm.fit3)

lm.fit4=lm(review_scores_rating~cleaning_fee+security_deposit+accommodates+minimum_nights+price+bedrooms+bathrooms+beds+host_listings_count+number_of_reviews)
summary(lm.fit4)

outlierTest(lm.fit4)

finallistings=listings[-c(18120,5191,1401,5251,7324,2820,1220,10973,2548,16451),]


finallistings <- listings

install.packages("randomForest")
library(randomForest)

listings <- na.omit(listings)

myforest=randomForest(review_scores_rating~host_response_rate+host_response_time+minimum_nights+price+bedrooms+beds+host_is_superhost+host_listings_count+number_of_reviews, ntree=500,data=listings,importance=TRUE)
myforest
importance(myforest)
varImpPlot(myforest)

myforest2=randomForest(review_scores_rating~host_has_profile_pic+host_identity_verified+instant_bookable+
                         require_guest_phone_verification, ntree=500,data=listings,importance=TRUE)
myforest2
importance(myforest2)
varImpPlot(myforest2)

myforest3=randomForest(host_is_superhost~price+beds+host_listings_count+minimum_nights+
                         number_of_reviews+bedrooms+host_response_time+host_response_rate+
                         security_deposit+cleaning_fee+bathrooms+cancellation_policy,ntree=500,data=listings,importance=TRUE)

myforest4=randomForest(host_is_superhost~price+beds+host_listings_count+minimum_nights+maximum_nights +
                         number_of_reviews+bedrooms+host_response_rate+
                         security_deposit+cleaning_fee+bathrooms,ntree=500,data=listings,importance=TRUE)
myforest4
importance(myforest4)
varImpPlot(myforest4)

myforest5=randomForest(host_is_superhost~availability_30 + availability_60 + availability_90 + 
                         availability_365 + review_scores_accuracy + review_scores_cleanliness + 
                         review_scores_checkin + review_scores_communication + review_scores_location + 
                         review_scores_value + reviews_per_month,ntree=500, data=listings,importance=TRUE)

myforest5
importance(myforest5)
varImpPlot(myforest5)

myforest6=randomForest(host_is_superhost~verified_host+book_instantly+host_listings_count+price+cleaning_fee
                       +accommodates+security_deposit+review_scores_rating + neighbourhood.1 + host_greets + has_Wifi + bedrooms
                       +reviews_per_month + number_of_reviews+minimum_nights+host_response_rate+host_response_time,ntree=500, do.trace=50, norm.votes=TRUE, data=listings,importance=TRUE)
myforest6
importance(myforest6)
varImpPlot(myforest6)

forest=data.frame(price=350,beds=2,host_listings_count=1,minimum_nights=15,maximum_nights=100,number_of_reviews=200,bedrooms=2,host_response_rate=1,security_deposit=250,cleaning_fee=250,bathrooms=2)
predict(myforest4, forest, type="prob")

logit=glm(superhost~host_response_rate, family="binomial")
summary(logit)

rate=0.9
exp(coef(logit)[1]+coef(logit)[2]*rate)/(1+exp(coef(logit)[1]+coef(logit)[2]*rate))

rate=1
exp(coef(logit)[1]+coef(logit)[2]*rate)/(1+exp(coef(logit)[1]+coef(logit)[2]*rate))

values = data.frame(host_response_rate = c(0.5,0.9,1))
predict(logit, values, type="response")

plot(host_response_rate, superhost, col="grey", ylim=c(0,1), ylab="Superhost", xlab="Host Reponse Rate")
curve(predict(logit,data.frame(host_response_rate=x),type="resp"),add=TRUE, col="red")

mlogit=glm(superhost~host_response_rate+host_response_time+profilepic+verified_host+book_instantly+verify+host_listings_count+security_deposit+cleaning_fee+price+guests_included+extra_people+number_of_reviews+cancellation_policy)
summary(mlogit)

install.packages("rms")
require(rms)

mlogit=lrm(superhost~number_of_reviews+host_listings_count+host_response_rate+price+security_deposit+minimum_nights+cleaning_fee+beds+bedrooms+maximum_nights+bathrooms)
mlogit

mlogit2=lrm(superhost~verified_host+book_instantly+host_listings_count+price+cleaning_fee
            +accommodates+security_deposit+review_scores_rating + neighbourhood.1 + host_greets + has_Wifi
            +reviews_per_month + number_of_reviews+minimum_nights+host_response_rate+host_response_time)

mlogit2

logit=glm(superhost~verified_host+book_instantly+host_listings_count+price+cleaning_fee
          +accommodates+security_deposit+review_scores_rating + neighbourhood.1 + host_greets + has_Wifi
          +reviews_per_month + number_of_reviews+minimum_nights+host_response_rate+host_response_time, family="binomial")
logit
value = data.frame(verified_host=1,book_instantly=0,host_listings_count=1,price=300,cleaning_fee=150,
                   accommodates=3,security_deposit=200,review_scores_rating=100,neighbourhood.1="II Arrondissement",
                   host_greets=1,has_Wifi=1,reviews_per_month=3,number_of_reviews=80,minimum_nights=15,host_response_rate=1, host_response_time="within an hour")
predict(logit,value,type="response")
predict(logit,value,type="prob")

library(boot)
cv.error=cv.glm(listings,logit,K=50)$delta[1]
cv.error
logit1=glm(superhost~verified_host+host_listings_count+price+review_scores_rating+number_of_reviews, family="binomial")
summary(logit1)

logit2=glm(superhost~verified_host+book_instantly+host_listings_count+price+number_of_reviews, family="binomial")
summary(logit2)

value_1 = data.frame(verified_host=1, host_listings_count=1, price=350, review_scores_rating= 95, number_of_reviews=200)
predict(logit1, value_1, type="response")

value_2 = data.frame(verified_host=1,book_instantly=0,host_listings_count=1,price=350,review_scores_rating=95,number_of_reviews=200)
predict(logit2, value_2, type="response")

labels4=listings[,c(11,13,69)]
listings_vars4=listings[,c(12,14,34,36,43,45,56,59,73)]

install.packages("car")
require(car)
scatterplotMatrix(listings_vars)

pca4=prcomp(listings_vars4,scale=TRUE)
pca4

pca3=prcomp(listings_vars3,scale=TRUE)
pca3

pca=prcomp(listings_vars1,scale=TRUE)
pca

install.packages("ggplot2")
install.packages("ggfortify")
require(ggplot2)
require(ggfortify)

autoplot(pca4, data = listings_vars4, loadings = TRUE, loadings.label = TRUE )

autoplot(pca4, loadings = TRUE, loadings.label = TRUE, data = listings_vars4, 
          col=ifelse(labels4$host_is_superhost== "t","#084594","#9ECAE1"))

autoplot(pca3, loadings = TRUE, loadings.label = TRUE, data = listings_vars3, 
         col=ifelse(labels4$host_is_superhost== "t","#084594","#9ECAE1"))

pve=(pca3$sdev^2)/sum(pca3$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
pve

ggplot(data=listings, aes(host_response_rate,review_scores_rating, col=host_is_superhost)) + geom_point(alpha = .5,stroke=0) +
  scale_color_manual(values=c("#6BAED6", "#084594")) + xlab("Host Response Rate") + ylab("Avg Ratings") + ggtitle("What does it take to be a Super Host")

qplot(host_response_rate, review_scores_rating, xlab="Reponse Rate", ylab="Rating", data=listings, col=host_is_superhost) + scale_color_manual(values=c("#6BAED6", "#084594"))

listings_vars1=listings[,c(12,14,35,36,37,38,44,45,46,47,48,49,60)]
listings_vars2=listings[,c(12,14,34,35,36,37,43,44,45,46,47,48,49,56,59)]
listings_vars3=listings[,c(12,14,34,43,44,45,48,54,56,59,73)]

autoplot(pca, data = listings_vars1, loadings = TRUE,
         col=ifelse(labels4$host_is_superhost=="t","blue","orange"), loadings.label = TRUE )

install.packages("psych")
require(psych)

pairs.panels(listings_vars4)
pairs.panels(listings_vars1)
pairs.panels(listings_vars2)

install.packages("olsrr")
require(olsrr)

mreg=lm(review_scores_rating~bedrooms+price+bathrooms+beds)
ols_coll_diag(logit)

install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal(n = 8, name = 'Blues')
brewer.pal(n = 8, name = 'Purples')
brewer.pal(n = 8, name = 'YlGnBu')

ggplot(data.frame(property_type), aes(x = property_type, fill=property_type)) + geom_bar(aes(x = forcats::fct_infreq(property_type), fill = property_type)) +
  scale_fill_manual("legend", values = c("Apartment" = "#4292C6", "Loft" = "#6BAED6", "Condominium" = "#9ECAE1", "House" = "#C6DBEF", "Townhouse" = "#DEEBF7", "Bed and breakfast" = "#F7FBFF"))+ 
  scale_x_discrete(limits=c("Apartment", "Loft", "Condominium", "House", "Townhouse", "Bed and Breakfast"))

boxplot(price, col="#C6DBEF", main="Airbnb Price per Night",ylab="Price")
ggplot(listings, aes(y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

ggplot(data = listings, aes(x = host_is_superhost, y = price)) + 
  geom_boxplot(aes(fill = host_is_superhost)) + scale_fill_manual(values=c("#4292C6", "#C6DBEF")) + 
  xlab("Superhost") + ylab("Price") + labs(fill="Superhost")

ggplot(listings, aes(x=cancellation_policy, y=price)) + 
  geom_boxplot(varwidth = T, aes(fill = factor(cancellation_policy)))  + 
  scale_fill_manual(values=c("#4292C6","#6BAED6", "#C6DBEF")) + 
  xlab("Cancellation Policy") + ylab("Price") + labs(fill="Cancellation Policy")

ggplot(listings, aes(x = review_scores_rating, y = price)) + 
  geom_point(aes(col = cancellation_policy)) + xlab("Superhost") + ylab("Price") + 
  scale_color_manual(values=c("#084594","#6BAED6", "#C6DBEF")) + xlim(65,100)

install.packages("tree")
install.packages("rpart.plot")
library(tree)
library(rpart.plot)
library(rpart)

rpart.plot(mytreehouse1, box.palette = "Blues")
rpart.plot(mytreehouse2, box.palette = "Blues")


mytreehouse1=rpart(neighbourhood.1~price+number_of_reviews+host_is_superhost+review_scores_rating, cp=0.001)
rpart.plot(mytreehouse1)

mytreehouse2=rpart(neighbourhood.1~price+review_scores_rating, cp=0.001)
rpart.plot(mytreehouse2)

install.packages("sunburstR")
library(sunburstR)

install.packages("plotly")
library(plotly)
plot_ly(cancellation_policy,type="sunburst")
sunburst(listings)

install.packages("scales")
library(scales)
library(ggplot2)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

ggplot(listings, aes(x="", y=cancellation_policy, fill=cancellation_policy))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)
ggplot(listings, aes(x="", y=cancellation_policy, fill=cancellation_policy)) +
  geom_col(aes(x = 1, y = "", fill = cancellation_policy), position = "fill") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cancellation_policy)
