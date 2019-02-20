# The-Office

![test](photo url here)

r/SubredditSimulator uses markov chains, the only posts are made by bots, as are the comments.
I wanted to do something similar, but train the model with lines from the office

I found the dataset from a post in r/datasets from u/
I am only intersted in the speaker and line_text, not the seaon or episode number.

The data can be found attatched in this folder or here as a .csv format




This analysis was preformed in R, stay tuned for the Python version, coming soon.
 Two packages were used:
 
library(qdapRegex) good for removing lines
library(markovchain) builds the markov chain

the data conains two columns, the line and the speaker
59911 lines
reading in data:
## Read in data and data prep #####################################
office <- read.csv('C:/users/joshua/desktop/OfficeLines.csv')

r reads the line text in as a factor of  59911 different levels, convert to character
office$line_text <- as.character(office$line_text)

The first subsset I want to create is the scene actions. These are embedded in the lines, but are always surrounded by square brackets []
This is where the qdapRegex comes in to play. We use this package save anything between square brackets as it's own character string
# scene actions (any text between [] brackets)
actions <- rm_between(office$line_text, "[", "]", extract=T)

not every line has scene actions. rm_between will store it as an NA, so we need to get rid of the NAs
actions <- actions[!is.na(actions)]

We have saved our actions, so we now need to remove them from the line text
# removes actions (all brackets and text between brackets)
for (i in 1:length(office$line_text)){
  office[i,1] <-rm_between(office[i,1], "[", "]", extract=F, replacement="")
  if(i %% 1000 == 0){print(i)}
}


We now make a subset of each character we want, the top 20 were selected:
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

this function takes one of the character subscripts and returns a markov chain model 
(warning, this takes some time with characters with a lot of lines)

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

#plot(fit$estimate)

paste(markovchainSequence(n=3, markovchain=out.dWallace$estimate), collapse=' ')
