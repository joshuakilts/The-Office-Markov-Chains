# Natural Language Processing Using Markov Chains
## Predicting Lines from The Office
![The Office Logo](/Images/theOffice.png)
The Office (US version), is an american adaptation of the british sitcom The Office. The Office follows employees of Dunder Mifflin, a paper supply company located in Scranton, PA. The Office ran for 9 seasons and features many unique and distinct characters, making an analysis of scripts from the show very insteresting as the different characters are analyzed.

I got the idea to build some Markov chains after spending some time on Reddit, in a community named [r/SubredditSimulator](https://www.reddit.com/r/SubredditSimulator/). This community is one of the most unique subreddits, as humans are not allowed to post or comment; all posts and comments are writen by bots. Comments and posts are sometimes gibberish, almost always silly, and sometimes even scarily accurate. Most, if not all, use Markov chains to predict text from a given training set. (see [this article](https://medium.com/ymedialabs-innovation/next-word-prediction-using-markov-model-570fc0475f96) for more information on Markov chains)

I wanted to do something similar, but train the model with lines from The Office.

### Data  
I found the dataset from a [post](https://www.reddit.com/r/datasets/comments/6yt3og/every_line_from_every_episode_of_the_office_us/) in [r/datasets](https://www.reddit.com/r/datasets/) (a great resource if you are looking for intereseting datasets).

Though the data includes season, episode, and scene numbers, I am only intersted in the speaker and line text.
The data can be found at [this link](https://docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0/edit?usp=sharing) attatched [here](/OfficeLines.csv) in .csv format.

The data contains two columns (line text and speaker), and just under 60,000 lines.
Reading in data from .csv format:
```
office <- read.csv('.../OfficeLines.csv')
```

R reads each line text as a different factor, so we need to convert the factors to characters.
```
   office$line_text <- as.character(office$line_text)
```

### Required packages
This analysis was preformed in R (stay tuned for the Python version).
Two packages were used:
```
library(qdapRegex) 
# Used for editing/removing/replacing text

library(markovchain)
# Builds the Markov chain and predicts new lines
```


### Data preparation
The first subset I create is the scene descriptions. These are embedded in the lines, but are always surrounded by square brackets []
This is where the qdapRegex package comes into play. We use this package save anything between square brackets as it's own string.
```
sceneDescriptions <- rm_between(office$line_text, "[", "]", extract=T)
```

Not every line has scene descriptions. rm_between will store a value for each line in the dataframe, so if there isn't any text found, an NA is stored.
We get rid of the NAs here for convenience.
```
sceneDescriptions <- sceneDescriptions[!is.na(sceneDescriptions)]
```

We have saved our scene descriptions, so we now need to remove them from the line text
```
for (i in 1:length(office$line_text)){
  office[i,1] <-rm_between(office[i,1], "[", "]", extract=F, replacement="")
}
```

We now make a subset of each character we want (20 were selected for this project). We will also make a dataframe of these 20 characters.
```
michael <- subset(office,office$speaker=="Michael")
dwight <- subset(office,office$speaker=="Dwight")
jim <- subset(office,office$speaker=="Jim")
pam <- subset(office,office$speaker=="Pam")
andy <- subset(office,office$speaker=="Andy")
ryan <- subset(office,office$speaker=="Ryan")
darryl <- subset(office,office$speaker=="Darryl")
meredith <- subset(office,office$speaker=="Meredith")
angela <- subset(office,office$speaker=="Angela")
kelly <- subset(office,office$speaker=="Kelly")
kevin <- subset(office,office$speaker=="Kevin")
oscar <- subset(office,office$speaker=="Oscar")
erin <- subset(office,office$speaker=="Erin")
nellie <- subset(office,office$speaker=="Nellie")
phyllis <- subset(office,office$speaker=="Phyllis")
toby <- subset(office,office$speaker=="Toby")
gabe <- subset(office,office$speaker=="Gabe")
creed <- subset(office,office$speaker=="Creed")
jan <- subset(office,office$speaker=="Jan")
stanley <- subset(office,office$speaker=="Stanley")

officeSub <- rbind(michael,dwight,jim,pam,andy,ryan,darryl,meredith,
                   angela,kelly,kevin,oscar,erin,nellie,phyllis,toby,
                   gabe,creed,jan,stanley)
```

### Analysis
This function takes one of the character subscripts and returns a markov chain model 
(warning: this takes some time with characters that have a lot of lines)
```
script <- function(df){
 character <- as.vector(df[,1])
 character <- character[nchar(character) > 0]
 character <- gsub('.', ' .', character, fixed = TRUE)
 character <- gsub(',', ' ,', character, fixed = TRUE)
 character <- gsub('!', ' !', character, fixed = TRUE)
 character <- gsub('(', '( ', character, fixed = TRUE)
 character <- gsub(')', ' )', character, fixed = TRUE)


 terms <- unlist(strsplit(character, ' '))

 fit.df <- markovchainFit(data = terms)
 return(fit.df)
}


out.michael <- script(michael)
out.dwight <- script(dwight)
out.jim <- script(jim)
out.pam <- script(pam)
out.andy <- script(andy)
out.ryan <- script(ryan)
out.darryl <- script(darryl)
out.meredith <- script(meredith)
out.angela <- script(angela)
out.kelly <- script(kelly)
out.kevin <- script(kevin)
out.oscar <- script(oscar)
out.erin <- script(erin)
out.nellie <- script(nellie)
out.phyllis <- script(phyllis)
out.toby <- script(toby)
out.gabe <- script(gabe)
out.creed <- script(creed)
out.jan <- script(jan)
out.stanley <- script(stanley)
```


We are also going to predict scene descriptions, so we build a Markov chain model for the descriptions we saved earlier:
```
character <- as.vector(sceneDescriptions)
character <- character[nchar(character) > 0]
character <- gsub('.', ' .', character, fixed = TRUE)
character <- gsub(',', ' ,', character, fixed = TRUE)
character <- gsub('!', ' !', character, fixed = TRUE)
character <- gsub('(', '( ', character, fixed = TRUE)
character <- gsub(')', ' )', character, fixed = TRUE)
terms <- unlist(strsplit(character, ' '))
out.sceneDescriptions <- markovchainFit(data = terms)
```

### Prediction
Now we will put everything together.
Fist, we make lists of the character dataframes as well as the Markov chain fit model for each character.
```
characters <- list(michael,dwight,jim,pam,andy,ryan,darryl,meredith,angela,kelly,
                kevin,oscar,erin,nellie,phyllis,toby,gabe,creed,jan,dWallace,stanley)

character.out <- list(out.michael,out.dwight,out.jim,out.pam,out.andy,out.ryan,
                  out.darryl,out.meredith,out.angela,out.kelly,out.kevin,out.oscar,
                  out.erin,out.nellie,out.phyllis,out.toby,out.gabe,out.creed,out.jan,
                  out.dWallace,out.stanley)
```

This function picks a character based on a distributio of who is most likely to speak, and returns a line. The length of the line is based off a distribution of that character's line lengths. There is also a 1 in 5 chance of genereating a scene description alongside the produced line.
```
getLine <- function(){
 rand <- as.character(officeSub[sample(1:length(officeSub$speaker),1),2])
 if (rand == "Michael"){
     person <- 1
 } else if (rand == "Dwight") {
     person <- 2
 } else if (rand == "Jim") {
     person <- 3
 } else if (rand == "Pam") {
     person <- 4
 } else if (rand == "Andy") {
     person <- 5
 } else if (rand == "Ryan") {
     person <- 6
 } else if (rand == "Darryl") {
     person <- 7
 } else if (rand == "Meredith") {
     person <- 8
 } else if (rand == "Angela") {
     person <- 9
 } else if (rand == "Kelly") {
     person <- 10
 } else if (rand == "Kevin") {
     person <- 11
 } else if (rand == "Oscar") {
     person <- 12
 } else if (rand == "Erin") {
     person <- 13
 } else if (rand == "Nellie") {
     person <- 14
 } else if (rand == "Phyllis") {
     person <- 15
 } else if (rand == "Toby") {
     person <- 16
 } else if (rand == "Gabe") {
     person <- 17
 } else if (rand == "Creed") {
     person <- 18
 } else if (rand == "Jan") {
     person <- 19
 } else if (rand == "David") {
     person <- 20
 } else if (rand == "Stanley") {
     person <- 21
 }
 
 df <- as.data.frame(characters[person]) 
 
 if (person == 1){
     model <- out.michael
 } else if (person == 2) {
     model <- out.dwight
 } else if (person == 3) {
     model <- out.jim
 } else if (person == 4) {
     model <- out.pam
 } else if (person == 5) {
     model <- out.andy
 } else if (person == 6) {
     model <- out.ryan
 } else if (person == 7) {
     model <- out.darryl
 } else if (person == 8) {
     model <- out.meredith
 } else if (person == 9) {
     model <- out.angela
 } else if (person == 10) {
     model <- out.kelly
 } else if (person == 11) {
     model <- out.kevin
 } else if (person == 12) {
     model <- out.oscar
 } else if (person == 13) {
     model <- out.erin
 } else if (person == 14) {
     model <- out.nellie
 } else if (person == 15) {
     model <- out.phyllis
 } else if (person == 16) {
     model <- out.toby
 } else if (person == 17) {
     model <- out.gabe
 } else if (person == 18) {
     model <- out.creed
 } else if (person == 19) {
     model <- out.jan
 } else if (person == 20) {
     model <- out.dWallace
 } else if (person == 21) {
     model <- out.stanley
 }
 numLines <- length(df[,1])
 x <- sample(1:numLines,1)
 words <- round(nchar(df[x,1])/4.5,0)


 if (sample(1:5,1) == 5){
   y <- sample(1:length(actions),1)
   actionLength <-round(nchar(actions[y])/4.5,0)
   sampleAction <- paste(markovchainSequence(n=actionLength, markovchain=out.actions$estimate), collapse=" ") 
   actionForLine <- paste("[",sampleAction,"]",":",sep="")
 } else {
   actionForLine <- ""
 }



 who <- paste(rand,": ")
 sentance <- paste(markovchainSequence(n=words, markovchain=model$estimate), collapse=' ')
 line <- paste(who, actionForLine, sentance)
 return(line)
}
