# text-snippets


This is just to store a few snippets of text processing code written in R that were part of the discussion in the Coursera/ University of Illinois "Text Mining and Analytics" course.

I have used Alice in Wonderland as an example as I understand it to be public domain text.

### part 1 making bigrams and calculating frequencies

```
aliceInWonderland <- "CHAPTER I. Down the Rabbit-Hole Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, 'and what is the use of a book,' thought Alice 'without pictures or conversations?' So she was considering in her own mind (as well as she could, for the hot day made her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly a White Rabbit with pink eyes ran close by her. There was nothing so VERY remarkable in that; nor did Alice think it so VERY much out of the way to hear the Rabbit say to itself, 'Oh dear! Oh dear! I shall be late!' (when she thought it over afterwards, it occurred to her that she ought to have wondered at this, but at the time it all seemed quite natural); but when the Rabbit actually TOOK A WATCH OUT OF ITS WAISTCOAT-POCKET, and looked at it, and then hurried on, Alice started to her feet, for it flashed across her mind that she had never before seen a rabbit with either a waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran across the field after it, and fortunately was just in time to see it pop down a large rabbit-hole under the hedge. In another moment down went Alice after it, never once considering how in the world she was to get out again. The rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well. Either the well was very deep, or she fell very slowly, for she had plenty of time as she went down to look about her and to wonder what was going to happen next. First, she tried to look down and make out what she was coming to, but it was too dark to see anything; then she looked at the sides of the well, and noticed that they were filled with cupboards and book-shelves; here and there she saw maps and pictures hung upon pegs. She took down a jar from one of the shelves as she passed; it was labelled 'ORANGE MARMALADE', but to her great disappointment it was empty: she did not like to drop the jar for fear of killing somebody, so managed to put it into one of the cupboards as she fell past it. 'Well!' thought Alice to herself, 'after such a fall as this, I shall think nothing of tumbling down stairs! How brave they'll all think me at home! Why, I wouldn't say anything about it, even if I fell off the top of the house!' (Which was very likely true.) Down, down, down. Would the fall NEVER come to an end! 'I wonder how many miles I've fallen by this time?' she said aloud. 'I must be getting somewhere near the centre of the earth. Let me see: that would be four thousand miles down, I think--' (for, you see, Alice had learnt several things of this sort in her lessons in the schoolroom, and though this was not a VERY good opportunity for showing off her knowledge, as there was no one to listen to her, still it was good practice to say it over) '--yes, that's about the right distance--but then I wonder what Latitude or Longitude I've got to?' (Alice had no idea what Latitude was, or Longitude either, but thought they were nice grand words to say.) Presently she began again. 'I wonder if I shall fall right THROUGH the earth! How funny it'll seem to come out among the people that walk with their heads downward! The Antipathies, I think--' (she was rather glad there WAS no one listening, this time, as it didn't sound at all the right word) '--but I shall have to ask them what the name of the country is, you know. Please, Ma'am, is this New Zealand or Australia?' (and she tried to curtsey as she spoke--fancy CURTSEYING as you're falling through the air! Do you think you could manage it?) 'And what an ignorant little girl she'll think me for asking! No, it'll never do to ask: perhaps I shall see it written up somewhere.' Down, down, down. There was nothing else to do, so Alice soon began talking again. 'Dinah'll miss me very much to-night, I should think!' (Dinah was the cat.) 'I hope they'll remember her saucer of milk at tea-time. Dinah my dear! I wish you were down here with me! There are no mice in the air, I'm afraid, but you might catch a bat, and that's very like a mouse, you know. But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way, 'Do cats eat bats? Do cats eat bats?' and sometimes, 'Do bats eat cats?' for, you see, as she couldn't answer either question, it didn't much matter which way she put it. She felt that she was dozing off, and had just begun to dream that she was walking hand in hand with Dinah, and saying to her very earnestly, 'Now, Dinah, tell me the truth: did you ever eat a bat?' when suddenly, thump! thump! down she came upon a heap of sticks and dry leaves, and the fall was over. Alice was not a bit hurt, and she jumped up on to her feet in a moment: she looked up, but it was all dark overhead; before her was another long passage, and the White Rabbit was still in sight, hurrying down it. There was not a moment to be lost: away went Alice like the wind, and was just in time to hear it say, as it turned a corner, 'Oh my ears and whiskers, how late it's getting!' She was close behind it when she turned the corner, but the Rabbit was no longer to be seen: she found herself in a long, low hall, which was lit up by a row of lamps hanging from the roof."   

sentences <- tolower(unlist(strsplit(aliceInWonderland, "\\.|!|\\?| '|' |\\(|\\)")))
ascii <- gsub("[^[:alnum:] ]", " ",sentences)
ascii <- gsub(" +", " ",ascii)
n1 <- unlist(lapply(ascii, function(x){c(strsplit(x, " "), NA)}))
n1[n1==""] <- NA
n2 <- c(n1[2:length(n1)], NA)
bigrams <- data.frame(n1, n2)
completegrams <- bigrams[complete.cases(bigrams),]
completegrams$totup <- 1 #not critical but convenient
##aggregate unigram frequencies
n1f <- aggregate(totup ~ n1, data=completegrams, FUN=sum)
n2f <- aggregate(totup ~ n2, data=completegrams, FUN=sum)
names(n1f)[2] <- "n1f"
names(n2f)[2] <- "n2f"
n1f$n1f <- n1f$n1f / sum(n1f$n1f)
n2f$n2f <- n2f$n2f / sum(n2f$n2f)
#bigram frequencies
bf  <- aggregate(totup ~ n1 + n2, data=completegrams, FUN=sum)
bf$totup <- bf$totup / sum(bf$totup)
names(bf)[3] <- "actual"
bf2 <- merge(bf,n1f, by="n1")
bf3 <- merge(bf2,n2f, by="n2")
bf3$ifIndependent <- bf3$n1f * bf3$n2f
bf3$difference <- bf3$actual - bf3$ifIndependent
head(bf3[order(bf3$difference, decreasing = TRUE), c("n1", "n2", "actual", "difference")], 20)

```

The main note on this is that I would not use the aggregate function at large scale- I would swap out to dplyr or data.table.

### A slightly more generalised way of making ngrams quickly

The above example was making so bigrams, this can be generalised to making ngrams by bumping vectors along

```
ngrams <- 4
n1 <- unlist(lapply(ascii, function(x){c(strsplit(x, " "), rep(NA,(ngrams-1)))}))
n1[n1==""] <- NA
for (i in 2:ngrams){
  nnew <- c(n1[i:length(n1)], rep(NA,(i-1)))
  assign(paste("n", as.character(i), sep=""), nnew)
}
```

### very basic paradigmatic similarity

This is a very, very naive way of measuring the similarity of context of two terms (so no weighting or exclusions of terms etc). But it is a fast initial implementation that can be built on.

```
##Assume the existance of the completegrams data frame from the alice example
#gettin the total for each n2
n2f <- aggregate(totup ~ n2, data=completegrams, FUN=sum)
names(n2f)[2] <- "n2f"
#bigram frequencies
bf  <- aggregate(totup ~ n1 + n2, data=completegrams, FUN=sum)
#combine n2 totals
bfprop <- merge(bf,n2f, by="n2")
#express each observation as a proportion of the number of entries of n2 (so the number for that context)
bfprop$totup <- bfprop$totup / bfprop$n2f
bfprop$n2f <- NULL
#they say many to many joins are bad, they say self joins are bad
crosset <- merge(bfprop, bfprop, by="n1")
#when you think about it when are only going to sum when both entries have an n1 (otherwise the frequency is 0, so the product is 0) so joining on the n1 actually works
#not interested where both n2s are the same
crosset <- crosset[crosset$n2.x != crosset$n2.y,]
#multiple the adjusted frequencies
crosset$sim <- crosset$totup.x * crosset$totup.y
#sum the entries for each combination of contexts
simindex <- aggregate(sim ~ n2.x + n2.y, data=crosset, sum)
```
