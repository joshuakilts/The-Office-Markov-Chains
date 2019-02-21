# Natural Language Processing Using Markov Chains
## Predicting Lines from The Office
![The Office Logo](/Images/theOffice.png)
The Office (US version), is an american adaptation of the british sitcom The Office. The Office follows employees of Dunder Mifflin, a paper supply company located in Scranton, PA. The Office ran for 9 seasons and features many unique and distinct characters, making an analysis of scripts from the show very insteresting as the different characters are analyzed.

I got the idea to build some Markov chains after spending some time on Reddit, in a community named [r/SubredditSimulator](https://www.reddit.com/r/SubredditSimulator/). This community is one of the most unique subreddits, as humans are not allowed to post or comment; all posts and comments are writen by bots. Most, if not all, use Markov chains to predict text from a given training set. [This article](https://medium.com/ymedialabs-innovation/next-word-prediction-using-markov-model-570fc0475f96) provides a great walkthrough of the inner-workings of Markov chain text prediction.


I wanted to do something similar, but train the model with lines from the office

I found the dataset from a [post](https://www.reddit.com/r/datasets/comments/6yt3og/every_line_from_every_episode_of_the_office_us/) in [r/datasets](https://www.reddit.com/r/datasets/)
I am only intersted in the speaker and line_text, not the seaon or episode number.

The data can be found attatched in this folder or here as a .csv format




This analysis was preformed in R, stay tuned for the Python version, coming soon.
 Two packages were used:
```
library(qdapRegex) good for removing lines
library(markovchain) builds the markov chain
```

the data conains two columns, the line and the speaker
59911 lines
reading in data:
## Read in data and data prep #####################################
```
office <- read.csv('C:/users/joshua/desktop/OfficeLines.csv')
```
r reads the line text in as a factor of  59911 different levels, convert to character
```
   office$line_text <- as.character(office$line_text)
```

The first subsset I want to create is the scene actions. These are embedded in the lines, but are always surrounded by square brackets []
This is where the qdapRegex comes in to play. We use this package save anything between square brackets as it's own character string
# scene actions (any text between [] brackets)
```
actions <- rm_between(office$line_text, "[", "]", extract=T)
```

not every line has scene actions. rm_between will store it as an NA, so we need to get rid of the NAs
```
actions <- actions[!is.na(actions)]
```

We have saved our actions, so we now need to remove them from the line text
# removes actions (all brackets and text between brackets)
```
for (i in 1:length(office$line_text)){
  office[i,1] <-rm_between(office[i,1], "[", "]", extract=F, replacement="")
  if(i %% 1000 == 0){print(i)}
}
```

We now make a subset of each character we want, the top 20 were selected:
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
dWallace <- subset(office,office$speaker=="David")
stanley <- subset(office,office$speaker=="Stanley")
```



this function takes one of the character subscripts and returns a markov chain model 
(warning, this takes some time with characters with a lot of lines)
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
out.dWallace <- script(dWallace)
out.stanley <- script(stanley)
```

```
#plot(fit$estimate)

paste(markovchainSequence(n=3, markovchain=out.dWallace$estimate), collapse=' ')
```
