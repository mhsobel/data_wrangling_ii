Reading data from the web
================
Marisa Sobel
10/11/2018

Load some packages.

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

## Scrape

Get the
HTML

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_xml = read_html(url)
```

Get tables from HTML

``` r
# extracted ALL the tables...only want the first one
drug_use_xml %>% 
  html_nodes(css = "table")
```

    ## {xml_nodeset (15)}
    ##  [1] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [2] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [3] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [4] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [5] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [6] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [7] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [8] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [9] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [10] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [11] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [12] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [13] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [14] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [15] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...

``` r
# trying to get the first object in the "list" --> name object and select first element in the list
drug_use_xml %>% 
  html_nodes(css = "table") %>% 
  .[[1]]
```

    ## {xml_node}
    ## <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100%" summary="This is a table containing 16 columns.">
    ## [1] <caption>Table 1 – <span class="boldital">Marijuana Use in the Past  ...
    ## [2] <thead><tr>\n<th class="left" scope="col">State</th>\r\n<th scope="c ...
    ## [3] <tfoot><tr>\n<td scope="row" colspan="16">NOTE: State and census reg ...
    ## [4] <tbody>\n<tr>\n<th scope="row">Total U.S.</th>\r\n<td width="6%" cla ...

``` r
# make table into something useful in R --> NOT THERE YET, looks terrible 
# the "note" at the bottom of the table in the webpage appears at the top of the table in every column
#drug_use_xml %>% 
#  html_nodes(css = "table") %>% 
#  .[[1]] %>% 
#  html_table()

# slice to access rows of a table by number (filter over slice for the most part)
# convert to tibble
drug_use_xml %>% 
  html_nodes(css = "table") %>% 
  .[[1]] %>% 
  html_table() %>% 
  slice(-1) %>% 
  as.tibble()
```

    ## # A tibble: 56 x 16
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-201…
    ##    <chr> <chr>            <chr>            <chr>          <chr>           
    ##  1 Tota… 12.90a           13.36            0.002          13.28b          
    ##  2 Nort… 13.88a           14.66            0.005          13.98           
    ##  3 Midw… 12.40b           12.76            0.082          12.45           
    ##  4 South 11.24a           11.64            0.029          12.02           
    ##  5 West  15.27            15.62            0.262          15.53a          
    ##  6 Alab… 9.98             9.60             0.426          9.90            
    ##  7 Alas… 19.60a           21.92            0.010          17.30           
    ##  8 Ariz… 13.69            13.12            0.364          15.12           
    ##  9 Arka… 11.37            11.59            0.678          12.79           
    ## 10 Cali… 14.49            15.25            0.103          15.03           
    ## # ... with 46 more rows, and 11 more variables: `12-17(2014-2015)` <chr>,
    ## #   `12-17(P Value)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>

Got it into R, but still can’t use it… \* not tidy \* characters where
numbers are (from superscript in original
table)

### Learning Assessment

``` r
NYC_cost_living_data = read_html("https://www.bestplaces.net/cost_of_living/city/new_york/new_york") %>% 
  html_nodes(css = "table") %>% 
  .[[1]] %>% 
  html_table(header = TRUE)
```

## Harry Potter

Get the data

``` r
hpsaga_html = read_html("https://www.imdb.com/list/ls000630791/")

titles = hpsaga_html %>% 
  html_nodes(css = ".lister-item-header a") %>% 
  html_text()

gross_rev = hpsaga_html %>% 
  html_nodes(css = ".text-small:nth-child(7) span:nth-child(5)") %>% 
  html_text

hpsaga_df = tibble(
  title = titles, 
  rev = gross_rev
)
```

### Learning assessment

``` r
dynamite_html = read_html("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1")

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

## Using an API

Get water data

``` r
# as CSV
nyc_water = GET("https://data.cityofnewyork.us/resource/waf7-5gvc.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_integer(),
    ##   year = col_integer()
    ## )

``` r
# as JSON
nyc_water = GET("https://data.cityofnewyork.us/resource/waf7-5gvc.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()
```

Get BRFSS

``` r
brfss_smart2010 = 
  GET("https://data.cdc.gov/api/views/acme-vg9e/rows.csv?accessType=DOWNLOAD") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   Year = col_integer(),
    ##   Sample_Size = col_integer(),
    ##   Data_value = col_double(),
    ##   Confidence_limit_Low = col_double(),
    ##   Confidence_limit_High = col_double(),
    ##   Display_order = col_integer()
    ## )

    ## See spec(...) for full column specifications.
