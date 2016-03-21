## The source() function finds a file and runs all of its contents
## through R, just like you had "fed" each line to R manually. In this
## case, I have a file that checks if you have the GSS downloaded
## already in a subdirectory "data". If not, it downloads it from
## NORC's website and unzips it. This should work on all systems, but
## Windows may be a pain. If it doesn't work, just download and unzip
## the GSS file from NORC's website yourself.

source("./check-gss-and-maybe-download.R")

## Next we need to load the GSS dataset into R. In this case, we've
## downloaded the stata version (.dta file). R has two packages that
## both work well. foreign::read.dta() is older, but a little more
## stable. It does NOT read new (Stata 13+) files. haven::read_dta()
## is newer and less stable, but can read the newest Stata file
## format.

GSS <- foreign::read.dta("./data/GSS7214_R4.DTA",
                         convert.factors=FALSE)

## The dplyr package is super useful for data management, so let's
## load it up. We'll also load magrittr, which provides "pipes" for R:

library(dplyr)

## It contains four verbs that let us do most of the things we want to
## do: select(), which subsets by column;2 filter(), which subsets by
## row; mutate(), which creates variables (or changes existing
## variables); and group_by(), which lets us compute things by levels
## of another variable. More on that later, though.


## Let's say we want the correlation between two variables: nateduc
## and natsoc. These measure whether the respondent things we're
## spending too little (1) about the right amount (2) or too much (3)
## on education and social security, respectively.

cor(GSS$nateduc,
    GSS$natsoc,
    use="complete.obs")

## We might also be interested in this for subgroups of the
## population. This is where "piping" really shows its strengths. The
## piping operator in R is in the dplyr package and looks like this
## %>%

## You can read it as "then."

## First, let's create an indicator for whether a respondent is black
## or not. the race variable is coded 2 is a respondent is black, 1
## for white and 3 for others. I like to check to make sure that I'm
## not going to overwrite a variable first if I name my new variable
## black:

table(GSS$race)
table(GSS$black)

# base R: GSS$black <- with(GSS, ifelse(race == 2, TRUE, FALSE))
# with pipes and dplyr:

GSS <- GSS %>%
  mutate(black=ifelse(race == 2, TRUE, FALSE))

table(GSS$black)

cor(GSS$nateduc[GSS$black == TRUE],
    GSS$natsoc[GSS$black == TRUE],
    use="complete.obs")

## We can use piping to make this a little more straightforward, but
## note that we have to use the summarize() (or summarise() function
## is British English is your thing) in order to return the summary
## statistic that we're interested in (the correlation in this case
## but it could be the mean or whatever)

GSS %>%
  filter(black == TRUE) %>%
  summarize(
    cor(nateduc, natsoc,
      use="complete.obs"))

## the dplyr package, which we loaded previously, provides a very
## powerful way to look at data. Let's find the correlation between
## those two variables among four different subsets of the data. The
## group_by() function from dplyr is our friend here.

table(GSS$sex)
GSS$sex <- factor(GSS$sex,
                  levels=c(1, 2),
                  labels=c("M", "F"))
table(GSS$sex)

## Now we take the GSS, group it by sex and black, then summarize it,
## keeping the correlation between those two variables as well as the
## number of people in each of our four categories

GSS %>%
  group_by(sex, black) %>%
  summarize(thecor=cor(nateduc, natsoc,
                       use="complete.obs"),
            n=n())

## What if we're interested in how supportive people are of increased
## spending by year? One way is to look at the means by year:

gss_yearly <- GSS %>%
  group_by(year) %>%
  summarize(educ=mean(nateduc, na.rm=TRUE),
            soc=mean(natsoc, na.rm=TRUE))

gss_yearly

summary(gss_yearly)

## Note that we have no information for 1972, so let's drop that year
## from our data:

gss_yearly <- gss_yearly %>%
  filter(year != 1972)

## let's merge in some economic data. the ggplot2 package comes with
## monthly economic data for the US, starting in July 1967. For a look at what it contains, we can do ?economics

library(ggplot2)
economics
?economics

## We need to make an unemployment rate, which we'll just define as
## unemploy/pop

economics <- economics %>%
  mutate(unemp_rate=unemploy / pop)

## since this is monthly data, we'll need to aggregate it to yearly
## data in order to merge it into our gss_yearly object

economics_yearly <- economics %>%
  mutate(year=format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(unemp=mean(unemp_rate))

## now we have two dataframe objects - gss_yearly and economics_yearly
## - that we want to join together. Again, dplyr provides a really
## easy way of doing this. We call these "joins" (the jargon comes
## from SQL), but you probably refer to them as doing a "merge." Since
## we more years in the economics dataset than in the gss, we'll tell
## dplyr that we're only interested in the years that are in the gss
## dataset. If you wanted to keep everything, you can do a full_join()
## instead:

gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by="year")

## Error! Turns out that year in the gss is an integer and year in the
## economics is character. The error message hints at that and we can
## verify. That's easy to fix:

class(gss_yearly$year)
class(economics_yearly$year)

economics_yearly$year <- as.integer(economics_yearly$year)

gss_yearly <- left_join(gss_yearly,
                        economics_yearly,
                        by="year")
