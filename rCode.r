library(qdapRegex)
library(markovchain)

## Read in data and data prep #####################################
office <- read.csv('C:/Users/Administrator/Downloads/OfficeLines.csv')
office$line_text <- as.character(office$line_text)

# scene actions (any text between [] brackets)
actions <- rm_between(office$line_text, "[", "]", extract=T)
actions <- actions[!is.na(actions)]

# removes actions (all brackets and text between brackets)
for (i in 1:length(office$line_text)){
  office[i,1] <-rm_between(office[i,1], "[", "]", extract=F, replacement="")
  if(i %% 1000 == 0){print(i)}
}



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

officeSub <- rbind(michael,dwight,jim,pam,andy,ryan,darryl,meredith,
                   angela,kelly,kevin,oscar,erin,nellie,phyllis,toby,
                   gabe,creed,jan,dWallace,stanley)




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

# predict actions
character <- as.vector(actions)
character <- character[nchar(character) > 0]
character <- gsub('.', ' .', character, fixed = TRUE)
character <- gsub(',', ' ,', character, fixed = TRUE)
character <- gsub('!', ' !', character, fixed = TRUE)
character <- gsub('(', '( ', character, fixed = TRUE)
character <- gsub(')', ' )', character, fixed = TRUE)
terms <- unlist(strsplit(character, ' '))
out.actions <- markovchainFit(data = terms)


characters <- list(michael,dwight,jim,pam,andy,ryan,darryl,meredith,angela,kelly,
                kevin,oscar,erin,nellie,phyllis,toby,gabe,creed,jan,dWallace,stanley)

character.out <- list(out.michael,out.dwight,out.jim,out.pam,out.andy,out.ryan,
                  out.darryl,out.meredith,out.angela,out.kelly,out.kevin,out.oscar,
                  out.erin,out.nellie,out.phyllis,out.toby,out.gabe,out.creed,out.jan,
                  out.dWallace,out.stanley)

for (i in 1:length(erin)){
  df$length[i] <- round(nchar(erin[i,1]),0)
}
## Producing script
# Length of lines
  # Get distributions from each character, rand num generator based on distribution
    michael$length[i] <- length(michael$line[i])
  # OR OR OR just make vector of line lengths and pick one (sounds easier)
# Who speaks most
  # Same as above, but based of number of lines
  # OR OR OR just pick random person fromoffice$speaker
  # will need to get rid of unused speakers
# Who usually speaks after x person
  # Get row numbers of x person, add one to each
if (sample(1:5,1) == 5){
  print("yeet")
}
    
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

getLine()

newLine <- NULL
newLine <- as.array(newLine)
for (i in 1:10000){
  newLine[i] <- getLine()
}
write.csv(newLine,"lines.csv")
