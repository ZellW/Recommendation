#Packages to be used:
#recommenderlab
#ggplot
#dplyr



# -----------------------------------Question 1[Start]------------------

library("recommenderlab")	 	 
# Loading to pre-computed affinity data	 
#Loading Beer data from csv file
beer.data<-read.csv("beer_data.csv")
nrow(beer.data)  #475984
ncol(beer.data)  #3

str(beer.data)
# 'data.frame':	475984 obs. of  3 variables:
#   $ beer_beerid       : int  48215 52159 52159 52159 52159 58046 58046 58046 58046 58046 ...
# $ review_profilename: Factor w/ 22498 levels "","0110x011",..: 19293 15474 675 16851 16798 13843 5629 2680 1473 15351 ...
# $ review_overall    : num  3 3 3 4 3.5 4.5 4 4.5 4.5 4 ...

##There are three variable beer_beerid,review_profilename,review_overall
#View(beer.data)

head(beer.data)

length(unique(beer.data$beer_beerid)) #40308
unique(beer.data$review_overall) ##10 different type of rating.
#[1] 3.0 4.0 3.5 4.5 2.5 5.0 2.0 1.0 1.5 0.0
length(unique(beer.data$review_overall))#10

##Check for NA in dataset
sum(is.na(beer.data$beer_beerid)) ## 0  all beer ids are present

sum(is.na(beer.data$review_profilename)) ## 0  all review_profilename are present

sum(is.na(beer.data$review_overall)) ## 0  all review_overall are present


##beer.data$beer_beerid = as.factor(beer.data$beer_beerid)
##beer.data$review_overall = as.factor(beer.data$review_overall)

str(beer.data)

sum(duplicated(beer.data))
sum(duplicated(x))

##There are duplicate

beer.data.unique<-unique.data.frame(beer.data)
str(beer.data.unique)



library(dplyr)
beerid.count<- beer.data.unique %>% 
  count(beer_beerid)

str(beerid.count)
nrow(beerid.count) ## unique beers 40308


###
x<-subset(beerid.count,beerid.count$n>1)
nrow(x) ##22242

##so there are many beers in dataset having just 1 review, its going to be a cold start problem here for these beers.
## we should remove some

beerid.count.subset<-subset(beerid.count,beerid.count$n>=50)
nrow(beerid.count.subset) ##2066
str(beerid.count.subset)

sum(beerid.count.subset$n)

## choosing dataset with N>=50 we get 2066 beers to work with

beer.final.data<-subset(beer.data.unique,beer.data.unique$beer_beerid %in% beerid.count.subset$beer_beerid)
nrow(beer.final.data) ##296781

## with N=50, we get around 296781 recrods to work with from 475404 which more than 62% of overall recrods


###
# 
# beerReviewer.count<- beer.data.unique %>% 
#   count(review_profilename)
# 
# str(beerReviewer.count)


#View(beerid.count)
beer.final.data$beer_beerid <- as.factor(beer.final.data$beer_beerid) 

str(beer.final.data)
##
# review_profilename: Factor w/ 22498 levels "","0110x011",..: 3160 4344 10167 540 1766 10157 4587 1989 15661 365 ...
# $ beer_beerid       : Factor w/ 2066 levels "5","6","7","10",..: 187 1045 1045 1045 1045 1045 1045 1045 1045 1045 ...
# $ review_overall 
#

# -----------------------------------Question 1[End]------------------

# -----------------------------------Question 2[Start]------------------

beer.final.data <- beer.final.data[,c(2,1,3)]
str(beer.final.data)

##first user then item then rating



beer.matrix<- as(beer.final.data,"realRatingMatrix")

dimnames(beer.matrix)
rowCounts(beer.matrix)
colCounts(beer.matrix)
rowMeans(beer.matrix)

##2.1

#looking at similarity of the first ten users with each other
similar_users <- similarity(beer.matrix[1:10, ],
                            method = "cosine",
                            which = "review_profilename")


#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

## from this visulization, we see that there is some relation between 0tt0 and 0110x011 user (dark yellow) and some dark yellow are there

##user is similar to himself/herself

##2.2
similar_items <- similarity(beer.matrix[1:20, ],
                            method = "cosine",
                            which = "beer_beerid")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

##Items are similar to itself, there are very view similar items in the top 10,
## in top 20 we get the 0tt0,100floods good among them

##2.3
unique(beer.final.data$review_overall) ##3.0 4.5 4.0 5.0 3.5 2.0 2.5 1.5 1.0
length(unique(beer.final.data$review_overall)) ##9 unique ratings



##2.4
# Visualizing ratings
library(ggplot2)
qplot(getRatings(beer.matrix), binwidth = 1, 
      main = "Histogram of ratings", xlab = "review_overall")

##review_overall are majorly among (3,4,5) and mainly 5

summary(getRatings(beer.matrix)) # Skewed to the right

#MeanOf Overall User Ratings 3.89

qplot(getRatings(normalize(beer.matrix, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "review_overall") 

summary(getRatings(normalize(beer.matrix, method = "Z-score"))) # seems better
#Mean zero

qplot(rowCounts(beer.matrix), binwidth = 10, 
      main = "Beer Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
##Most users rate less number of beers. data graph is quite skewed, till around first 200 number of users we have high amount ot ratings

unique(beer.final.data$review_profilename)

# beer.final.data.item <- beer.final.data[,c(2,1,3)]
# str(beer.final.data)
# str(beer.final.data.item)

##first user then item then rating


##first item then user then rating

##For avg Beer ratings, item is first
##beer.matrix.item<- as(beer.final.data.item,"realRatingMatrix")



##avg beer rating for each beer
library(dplyr)
grouped.beerid <- group_by(beer.final.data,beer_beerid)
beerid.avg <- summarise(grouped.beerid, mean=mean(review_overall))

##OR by using colMeans() from matrix
colMeans(beer.matrix)

##Avg Beer Ratings
mean(beer.final.data$review_overall) ##3.881937


##avg for each user ratings
grouped.review_profilename <- group_by(beer.final.data,review_profilename)
review_profilename.avg <- summarise(grouped.review_profilename, mean=mean(review_overall))

##OR by using rowMeans we get the avg user rating
rowMeans(beer.matrix)


##Avg number of rating per beer
beerid.final.rating.count<- beer.final.data %>% 
  count(beer_beerid)

##Avg number of rating per user
review_profilename.final.rating.count<- beer.final.data %>% 
  count(review_profilename)



# -----------------------------------Question2 [end]------------------

# -----------------------------------Question3 [Start]------------------

#--------------------------Recommendation models ----------------#

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models

scheme <- evaluationScheme(beer.matrix, method = "split", train = .9,
                           k = 1, given = 1, goodRating = 4)
?evaluationScheme

##cross-validation.scheme <- evaluationScheme(beer.matrix, method="cross-validation",k=10, given = -1, goodRating = 4)
##Error in cross - validation.scheme <- evaluationScheme(beer.matrix, method = "cross-validation",  : 
##object 'cross' not found
##


#goodRating=4 means all items with user rating for a beer greater or equal 4 are considered positives in the evaluation process
scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms,type="topNList", n=c(1, 3, 5, 10, 15, 20),keepModel=TRUE)
class(results)

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
##In graph User based is above the item based, we see that for say next 15 beers we find TPR is 0.020 and FPR is 0.005
#instead for Item vased is TPR 0.002 and FPR 0.001

##item basedhas less TPR and FPR wheareas user based with number of 


?evaluate
##

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")



##cross-validation scheme
###New, Running this below one taking a lot of time on my machine
scheme.2 <- evaluationScheme(beer.matrix, method = "split", train = .9,k = 10, given = 1, goodRating = 4)

# run algorithms, predict next n movies
results.2 <- evaluate(scheme.2, algorithms,type="topNList", n=c(1, 3, 5, 10, 15, 20),keepModel=TRUE)
class(results.2)

# Draw ROC curve
plot(results.2, annotate = 1:4, legend="topleft")

##scheme & scheme.2(from cross validation) are not varying much using scheme with k=1
##


Rec.UBCF.model<-Recommender(getData(scheme, "train"), method = "UBCF", param=list(normalize = "Z-score",
                                                                                  method="Cosine",
                                                                                  nn=30, minRating=3))
Rec.IBCF.model<-Recommender(getData(scheme, "train"), method = "IBCF", param=list(normalize = "Z-score"))

results <- evaluate(scheme, algorithms,type="topNList", n=c(1, 3, 5, 10, 15, 20),keepModel=TRUE)
class(results)

##Recommendations for "cokes", "genog" & "giblet"

recommended.items.cokes <- predict(Rec.UBCF.model, beer.matrix["cokes",], n=5)
as(recommended.items.cokes, "list")
# $cokes
#[1] "7971"  "571"   "45086" "4083"  "50570"
recommended.items.genog <- predict(Rec.UBCF.model, beer.matrix["genog",], n=5)
as(recommended.items.genog, "list")
# $genog
#[1] "104"   "1005"  "9873"  "10331" "4083"
recommended.items.giblet <- predict(Rec.UBCF.model, beer.matrix["giblet",], n=5)
as(recommended.items.giblet, "list")
# $giblet
#[1] "2347"  "61"    "1010"  "2872"  "22787"



#--------------------------Recommendation models ----------------#
# -----------------------------------Question3 [End]------------------



####

## Running this taking a lot of time ##

# scheme.2 <- evaluationScheme(beer.matrix, method = "split", train = .9,k = 2, given = 1, goodRating = 4)
# 
# 
# 
# # run algorithms, predict next n movies
# results.2 <- evaluate(scheme.2, algorithms,type="topNList", n=c(1, 3, 5, 10, 15, 20),keepModel=TRUE)
# class(results.2)
# 
# # Draw ROC curve
# plot(results.2, annotate = 1:4, legend="topleft")

# p
# ## compute error metrics averaged per user and then averaged over all
# ## recommendations
# calcPredictionAccuracy(p, getData(e, "unknown"))


## create a user-based CF recommender using training data
# e <- evaluationScheme(beer.matrix, method="split", train=0.9,
#                       k=1, given=1)
# e
# ## create a user-based CF recommender using training data
# r <- Recommender(getData(e, "train"), "UBCF")
# ## create predictions for the test data using known ratings (see given above)
# p <- predict(r, getData(e, "known"), type="ratings")
# p
# ## compute error metrics averaged per user and then averaged over all
# ## recommendations
# calcPredictionAccuracy(p, getData(e, "unknown"))
# head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))
# ## evaluate topNLists instead (you need to specify given and goodRating!)
# p <- predict(r, getData(e, "known"), type="topNList")
# p
# calcPredictionAccuracy(p, getData(e, "unknown"), given=15, goodRating=5)
# 

####







