install.packages("tidyverse")
library(tidyverse)
LA_zone <- read_csv("/Users/yeyuan/Downloads/LAozone.csv")
summary(LA_zone)
LAsmall <- LA_zone[,c("ozone","temp")]
apply(LAsmall,2,mean)
cov(LAsmall)
qt(0.995,328)
ozonemodel <- lm(ozone~temp, data = LAsmall)
summary(ozonemodel)

install.packages("data.table")
install.packages("magrittr")
library(magrittr)
library(data.table)
ratings_dt <- fread(file= "/Users/yeyuan/Downloads/extdata/BX-Book-Ratings.csv")
book_dt <- fread(file = "/Users/yeyuan/Downloads/extdata/BX-Books.csv")
users_dt <- fread(file = "/Users/yeyuan/Downloads/extdata/BX-Users.csv")
str(ratings_dt)
class(ratings_dt)
sapply(users_dt, class)
sapply(users_dt, class)
as.integer(users_dt$Age)
users_dt[,Age := as.numeric(Age)] #operator used to define new column
summary(book_dt)
head(ratings_dt)
tail(ratings_dt)
colnames(book_dt) <- gsub("-",'_',colnames(book_dt))
colnames(users_dt) <- gsub("-",'_',colnames(users_dt))
colnames(ratings_dt) <- gsub("-",'_',colnames(ratings_dt))
colnames(users_dt)
book_dt[ ,"Image_URL_S" := NULL]
book_dt[,c("Image_URL_S","Image_URL_M","Image_URL_L") := NULL]
#book_dt_2 <- book_dt[i =Yearofpubliction %in% 1900:2019]
book_dt_2 <- book_dt[Year_Of_Publication > 1900 & Year_Of_Publication <2019 ]
str(book_dt)
NumberOfr <- book_dt[Year_Of_Publication > 2000 & Year_Of_Publication <= 2010,j =Book_Author, .N]
book_dt[,uniqueN(Book_Author)]
book_dt[i = Year_Of_Publication %in% 2000:2010, j = uniqueN(Book_Author), by = Year_Of_Publication][,sum(V1)]

users_dt[is.na(Age),.N]
ratings_dt[,max(Book_Rating, na.rm = TRUE)]
ratings_dt[Book_Rating >0,.N,by = Book_Rating][N==max(N)]
ratings_dt[Book_Rating %in% ratings_dt[,max(Book_Rating, na.rm = TRUE)], ISBN]
ratings_dt[Book_Rating == max(Book_Rating, na.rm == TRUE), "ISBN"]

ratings_dt <- ratings_dt[i = order(-Book_Rating)]
setorder(ratings_dt, -Book_Rating)

library(data.table) 
library(magrittr) 
library(tidyr)

