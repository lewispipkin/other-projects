bowling
================
Lewis Pipkin
January 12, 2019

Last night, my good friend and I went bowling- $36 for 100 minutes of bowling (which would be a fantastic deal if it included shoe rental) and a prime opportunity for data collection. I lost 5 of 6 games, but most of the games were tantalizingly close. So, I ran a Monte Carlo simulation to see what my record would be after 100k matchups (not taking alcohol consumption into account- I'm a teetotaler but my friend bought some Dos Equis at the bowling alley's concession stand). It wasn't pretty:

``` r
Lewis <- c(90,79,90,102,94,109)
Erik <- c(139,97,104,110,99,100)

#mean(Lewis) #94
#sd(Lewis) #10.44988

#mean(Erik) #108.1667
#sd(Erik) #15.79135

lew <- c()
er <- c()
for(i in 1:1e5){
  lew[i] <- rnorm(1,mean(Lewis),sd(Lewis))
  er[i] <- rnorm(1,mean(Erik),sd(Erik))
}
df <- data.frame(lew,er)
df$winner <- ifelse(df$lew > df$er, "Lewis", "Erik")
sum(df$winner=="Lewis")
```

    ## [1] 22814

``` r
sum(df$winner=="Erik")
```

    ## [1] 77186

Sheesh. Have you ever lost 77000 of something? It doesn't feel too good. But, how did we get here? Let's dive into the data:

``` r
bowling <- read_csv("~/Documents/bowling.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Name = col_character(),
    ##   Game = col_double(),
    ##   Frame = col_double(),
    ##   Roll = col_double(),
    ##   Is_spare = col_double(),
    ##   Is_strike = col_double(),
    ##   pins = col_double(),
    ##   cumul_pins = col_double(),
    ##   score = col_double(),
    ##   cumul_score = col_double()
    ## )

``` r
head(bowling)
```

    ## # A tibble: 6 x 10
    ##   Name   Game Frame  Roll Is_spare Is_strike  pins cumul_pins score
    ##   <chr> <dbl> <dbl> <dbl>    <dbl>     <dbl> <dbl>      <dbl> <dbl>
    ## 1 Lewis     1     1     1        0         0     1          1     9
    ## 2 Lewis     1     1     2        0         0     8          9     9
    ## 3 Lewis     1     2     1        0         0     7          7     7
    ## 4 Lewis     1     2     2        0         0     0          7     7
    ## 5 Lewis     1     3     1        0         0     6          6     7
    ## 6 Lewis     1     3     2        0         0     1          7     7
    ## # ... with 1 more variable: cumul_score <dbl>

``` r
bowling %>% group_by(Name) %>% summarize(avg_pins=mean(pins))
```

    ## # A tibble: 2 x 2
    ##   Name  avg_pins
    ##   <chr>    <dbl>
    ## 1 Erik      4.46
    ## 2 Lewis     4.05

So, I had a lower pins-per-roll, naturally. Here is the density:

``` r
ggplot(bowling,aes(x=pins, fill=Name)) + geom_density(alpha=0.25)
```

![](bowlingjan12_files/figure-markdown_github/unnamed-chunk-2-1.png)

And we ended up being pretty even in terms of spares and strikes.

``` r
bowling %>% group_by(Name) %>% summarize(tot_spare=sum(Is_spare),
                                         tot_strike=sum(Is_strike))
```

    ## # A tibble: 2 x 3
    ##   Name  tot_spare tot_strike
    ##   <chr>     <dbl>      <dbl>
    ## 1 Erik         13          6
    ## 2 Lewis        12          4

Let's see how many pins were left standing going into each of these 25 total spares:

``` r
table(10-bowling$pins[which(bowling$Is_spare==1)-1])
```

    ## 
    ##  1  2  3  4  5  6  7  9 10 
    ##  6  2  5  4  2  1  1  1  3

So, 3 of these were essentially strikes, but the bulk of them were with 3 or fewer pins left going into the roll. Makes sense.

But now, we'll check out how confidence and repetion played into these matchups. After a spare or a strike, average number of pins shot up (compare this to the first table):

``` r
bowling_ <- bowling[which(bowling$Is_spare==1 | bowling$Is_strike==1)+1,]
bowling_ %>% group_by(Name) %>% summarize(avg_pins=mean(pins))
```

    ## # A tibble: 2 x 2
    ##   Name  avg_pins
    ##   <chr>    <dbl>
    ## 1 Erik      6.68
    ## 2 Lewis     5.56

And here is our average pins per frame over these 6 games:

``` r
b <- bowling %>% group_by(Name, Frame) %>% summarize(avg_pins=mean(pins))
ggplot(b,aes(x=Frame,y=avg_pins,col=factor(Name))) + geom_point() + geom_line()
```

![](bowlingjan12_files/figure-markdown_github/unnamed-chunk-6-1.png)

As we can see, I started off stronger, but soon enough I would begin to taper off. (Or, rather, Erik would begin to pull away.) We see that below, in terms of average score per frame, incorporating the post-spare or -strike bonuses:

``` r
b_ <- bowling %>% group_by(Name, Frame) %>% summarize(avg_score=mean(cumul_score))
ggplot(b_,aes(x=Frame,y=avg_score,col=factor(Name))) + geom_point() + geom_line()
```

![](bowlingjan12_files/figure-markdown_github/unnamed-chunk-7-1.png)

Oftentimes I was close behind if not leading halfway through, and then I wouldn't be able to put him away. I realize that since I was already owned extremely hard last night in 5 of the 6 games, I didn't have to write this and prove to the world that I am bad at bowling with tables and charts, but I did it anyway-- for the love of knowledge and discovery. I'll probably throw up the bumpers next time, though.
