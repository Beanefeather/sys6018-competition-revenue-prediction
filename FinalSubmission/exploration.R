# Initialize libraries
library(plotrix)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Read in data
train = read.csv("train_clean.csv")
test = read.csv("test_clean.csv")

# Look at first few observations
head(train)

# Store distribution for false and true values of device_isMobile
mobile.tib =  train %>%
  group_by(device_isMobile) %>%
  tally()

# Store in vectors for passing into pie function
tf.vec = pull(mobile.tib, n)
name.vec = pull(mobile.tib, device_isMobile)

# Create pie chart
pie(tf.vec, name.vec, main = "Distribution of Mobile Visitors", col = rainbow(length(name.vec)))

# We see that a majority of users aren't on mobile


### Repeat this process with continent ###

cont.tib = train %>%
  group_by(geoNetwork_continent) %>%
  tally()

contnum.vec = pull(cont.tib, n)
cont.vec = pull(cont.tib, geoNetwork_continent)

pie(contnum.vec, cont.vec, main = "Visitors by Continent", col = rainbow(length(cont.vec)))

# We see the plurality of users are in the Americas with Asia and Europe
# contributing roughly equal numbers. This disparity could be explained by
# multiple continents being included in "Americas", so we'll see how that's
# distributed.

amer.tib = train[train$geoNetwork_continent == 'Americas',] %>%
  group_by(geoNetwork_subContinent) %>%
  tally()

amernum.vec = pull(amer.tib, n)
amer.vec = pull(amer.tib, geoNetwork_subContinent)

pie(amernum.vec, amer.vec, main = "Visitors in Americas by Continents", col = rainbow(length(amer.vec)))

nor.amer.perc = max(round(100*amernum.vec/sum(amernum.vec), 1))
# We see that though Americas were rolled into one continent, North America
# still dominates the numbers at about 86.7% of the Americas' numbers


### By operating system ###

os.tib = train %>%
  group_by(device_operatingSystem) %>%
  tally()

# Screen any percentages lower than 2%
os.tib = os.tib[os.tib$n > 0.02*sum(os.tib$n),]

osnum.vec = pull(os.tib, n)
os.vec = pull(os.tib, device_operatingSystem)

pie(osnum.vec, os.vec, main = "Visitors by OS", col = rainbow(length(os.vec)))

# We see some expected values and distributions based on previous results,
# but the main takeaway is that Macintosh and Windows have roughly equal numbers,
# with the same being true for iOS and Android but in smaller numbers.


### Look at date distribution ###

dates = table(as.Date(as.character(train$date), format = "%Y%m%d"))
#barplot(dates, main = "Visits by Date", ylab = "Visitors", xlab = "Date")
p = plot_ly(
  x = names(dates),
  y = as.vector(dates),
  type = 'bar'
) %>%
layout(
  title = 'Visits by Date',
  xaxis = list(
    type = 'Category',
    title = 'Date'
  ),
  yaxis = list(
    title = 'Number of Visits'
  )
)  
  
p

# We see that there's a relative spike in the early winter, which we can 
# likely attribute to the holiday season. Evidence for this is the very 
# steep dropoff immediately after 12/25, but further analysis is necessary.


### Revenue ###

train$totals_transactionRevenue[is.na(train$totals_transactionRevenue)]= 0

revbydate = train %>%
  group_by(date) %>%
  summarize(TotalRev = sum(totals_transactionRevenue))


revdate.plot = plot_ly(
  x = as.Date(as.character(revbydate$date), format = '%Y%m%d'),
  y = revbydate$TotalRev,
  type = 'bar'
) %>%
  layout(
    title = 'Revenue by Date',
    xaxis = list(
      type = 'Category',
      title = 'Date'
    ),
    yaxis = list(
      title = 'Total Revenue'
    )
  )  

revdate.plot
