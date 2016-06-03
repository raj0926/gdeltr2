gdeltr2
================

`gdeltr2` — **R's modern GDELT Project interface**

#### <strong>What is the GDELT Project?</strong>

[The Global Database of Events, Language, and Tone](gdelt.org) **\[GDELT\]** is a non profit whose initiative is to: &gt; construct a catalog of human societal-scale behavior and beliefs across all countries of the world, connecting every person, organization, location, count, theme, news source, and event across the planet into a single massive network that captures what's happening around the world, what its context is and who's involved, and how the world is feeling about it, every single day.

GDELT was founded in 1994 and it's data commences in 1979. Over the last two years the GDELT's functionality and abilities have grown exponentially, for example in May 2014 GDELT processed 3,928,926 where as in May 2016 it processed 6,198,461. GDELT continues to evolve and integrate advanced machine learning tools including [Google Cloud Vision](https://cloud.google.com/vision/) \[Not integrated into the package but coming soon!!\]

#### <strong>The project contains three data stores</strong>

-   [The GDELT Events Database](http://gdeltproject.org/data.html#rawdatafiles) **\[EVENTS\]**: Global Events, 1979 to present.
-   [The GDELT Global Knowledge Graph](http://gdeltproject.org/data.html#rawdatafiles) **\[GKG\]** : GDELT's Knowledge Graph, April 2013 to present.
-   [The GDELT Free Text API](http://blog.gdeltproject.org/announcing-the-gdelt-full-text-search-api/) **[Free Text API](#free-text-api)**: Full text search for all monitored sources within a 24 hour window. Output includes raw data, sentiment, and word counts.

#### <strong>Why gdeltr2?</strong>

My main motivation for this building package is simple, **GDELT IS INCREDIBLE!!**

Accessing GDELT's data gold is doable but either difficult or costly.

Currently, anyone proficient in SQL can access the data via [Google Big Query](https://bigquery.cloud.google.com/dataset/gdelt-bq:gdeltv2?pli=1). The problem is that even if you want to us SQL users have to pay above a certain API call threshold and then you still need another layer of connectivity to explore the data in R.

Although R has two existing packages that allow users to interact with portions of GDELT's data outside of Big Query:

-   [gdeltr](https://github.com/ahalterman/gdeltr)
-   [GDELTtools](https://cran.r-project.org/web/packages/GDELTtools/)

These packages are old, incomplete and difficult to use and it is my hope that gdelt2r allows the R user easy access to GDELT's data allowing for faster and more exhilarating data visualizations and analysis!

#### <strong>Installation</strong>

``` r
devtools::install_github("abresler/gdeltr2")
```

#### <strong>Function Ontology</strong>

The package currently consists of two function families, **data acquisition** and **data tidying**.

The package data acquisition functions begin with `get_urls_` for acquiring data store log information, `get_codes_` for acquiring code books and `get_data_` for downloading and reading data.

The data tidying functions begin with `get_mentioned_` and they apply to a number of the features in the **gkg** data store that will get described in further detail farther below.

#### <strong>CAUTION</strong>

-   `gdeltr2` requires an internet connection for any data retrieval function
-   The package's `get_gkg_data` and `get_gdelt_event_` functions are extremely bandwidth intensive given the download sizes of these data stores.
-   The package is very memory intensive given the unzipped size of the `GDELT Event` and `Global Knowledge Graph` files.

#### <strong>Primary Functions</strong>

-   <strong>Free Text API</strong>
    -   `get_data_ft_api_domains()` - retrieves descriptive data from specified domains over the last 24 hours
    -   `get_data_ft_api_terms()` - retrieves descriptive data for specified terms over the last 24 hours
    -   `get_data_wordcloud_ft_api_domains()` - retrieves wordcloud data for specified domains over the last 24 hours
    -   `get_data_wordcloud_ft_api_terms()` - retrieves wordcloud data for specified terms over the last 24 hours
    -   `get_data_sentiment_ft_api_domains()` - retrieves sentiment data for specified domains over the last 24 hours
    -   `get_data_sentiment_ft_api_terms()` - retrieves sentiment data for specified terms over the last 24 hours
-   <strong>GDELT Events</strong>
    -   `get_urls_gdelt_event_log()` - retrieves descriptive data and urls for all available GDELT event downloads.
    -   `get_data_gdelt_period_event_totals` - retrieves summary event data for a given a period \[monthly, daily, yearly\]; this can be grouped by country.
    -   `get_data_gdelt_periods_event()` - retrieves GDELT event data for a specified periods. Periods are by 4 digit years from 1979 to 2005, 6 digit year month from January 2006 to March 2013, and 8 digit year month day code thereafter.
-   <strong>Global Knowledge Graph</strong>
    -   `get_urls_gkg_15_minute_log` - retrieves GKG 15 minute capture logs; data begins February 18th, 2015 for the three table types
        -   gkg: This is the full gkg data set and contains columns that may require further data tidying tying to a **GKG Record ID**
        -   export: This data replicates the output contained in the GDELT event table for processed documents tying to a **Global Event ID**
        -   mentions: This data contains information surrounding the processed events, including sources, tone, location within a document and this tying to a **Global Event ID**
    -   `get_urls_gkg_daily_summaries` - retrieves daily gkg capture logs; data begins in April of 2013.
    -   Each day contains a count file and the full gkg output.
    -   `get_data_gkg_day_summary()` retrieves GKG daily summary data for specified date(s), this captures *count files* by `is_count_file = T`
    -   `get_data_gkg_days_detailed()` - retrieves GKG data from the data cached every 15 minutes for specified date(s) for a given table. The table can be one of `c('gkg', 'export', 'mentions')`. This function may require significant bandwidth and memory given the potential file sizes.

#### <strong>Tidying Functions</strong>

Many of the columns in the GKG output are concatenated and require further parsing for proper analysis. These function tidy those concatenated columns, note given file sizes the functions may be time consuming.

-   `get_gkg_mentioned_names()` - parses mentioned names
-   `get_gkg_mentioned_people()` - parses mentioned people
-   `get_gkg_mentioned_organizations()` - parses mentioned organizations
-   `get_gkg_mentioned_numerics()` - parses mentioned numeric figures
-   `get_gkg_mentioned_themes()` - parses mentioned themes, ties to CAMEO Theme Codes
-   `get_gkg_mentioned_gcams()` - parses resolved GCAMs ties GCAM code book.
-   `get_gkg_mentioned_dates()` - parses mentioned dates according to the GKG scheme

#### <strong>Code Books</strong>

All these the GDELT and GKG datasets contain a whole host of codes that need resolution to be human readable. The package contains easy access to these code books to allow for that resolution. These functions provide access to the code books:

-   `get_codes_gcam()` - retrieves Global Content Analysis Measurement **\[GCAM\]** codes
-   `get_codes_cameo_country()` - retrieves Conflict and Mediation Event Observations **\[CAMEO\]** country codes
-   `get_codes_cameo_ethnic()` - retrieves cameo ethnic codes
-   `get_codes_cameo_events()` - retrieves cameo event codes
-   `get_codes_gkg_themes()` - retrieves gkg theme codes
-   `get_codes_cameo_type()` - retrieves cameo type codes
-   `get_codes_cameo_religion()` - retrieves cameo religion codes
-   `get_codes_cameo_known_groups()` - retrieves cameo known group codes

#### Coming Soon

-   Vignettes
-   Generic data visualization functions
-   Generic machine learning and data analysis functions
-   Integration with Google Big Query
-   Potential integration with 3rd party full database mirror

<strong>EXAMPLES</strong>
-------------------------

``` r
library(gdeltr2)
load_needed_packages(c('dplyr', 'magrittr'))
```

### Free Text API

``` r
test_terms <-
  c('"Brooklyn Nets"', 'Manhattan Condominium', '"Eddie Huang"', '"EB5"')

term_data <- 
  get_data_ft_api_terms(terms = test_terms, max_rows = 10000, only_english = T, domain = NA)

test_domains <- 
  c('realdeal.com', 'nytimes.com', 'curbed.com', 'pehub.com', 'wsj.com')
  
domain_data <- 
  get_data_ft_api_domains(term = NA, domains = test_domains)

domain_wordcloud <- 
  get_data_wordcloud_ft_api_domains(domains = test_domains, term = NA, tone_more_than = 1)

term_wordcloud <- 
  get_data_wordcloud_ft_api_terms(terms = test_terms, domain = NA)

term_sentiment <-
  get_data_sentiment_ft_api_terms(terms = test_terms, domain = NA )

domain_sentiment <-
  get_data_sentiment_ft_api_domains(domains = test_domains)
```

### GDELT Event Data

``` r
events_1981_1989 <- 
  get_data_gdelt_periods_event(
  periods = c(1983, 1989),
  file_directory = 'Desktop/gdelt_temp',
  is_count_file = F,
  remove_files = T,
  empty_trash = T,
  return_message = T
)
```

### GKG Data

``` r
gkg_summary_count_may_15_16_2014 <-  
  get_data_gkg_days_summary(
  dates = c('2014-05-15', '2014-05-16'),
  is_count_file = T,
  file_directory = 'Desktop/gkg_temp',
  remove_files = T,
  empty_trash = T,
  return_message = T
)

sources_example <- 
  gkg_summary_count_may_15_16_2014 %>% 
  slice(1:1000) %>% 
  get_mentioned_gkg_source_data(source_column = 'sources')

gkg_full_june_2_2016 <-
  get_data_gkg_days_detailed(
  dates = c("2016-06-02"),
  table_name = 'gkg',
  file_directory = 'Desktop/gkg_temp',
  remove_files = T,
  empty_trash = T,
  return_message = T
  )

gkg_mentions_may_12_2016 <-
  get_data_gkg_days_detailed(
  dates = c("2016-05-12"),
  table_name = 'mentions',
  file_directory = 'Desktop/gkg_temp',
  remove_files = T,
  empty_trash = T,
  return_message = T
  )
```
