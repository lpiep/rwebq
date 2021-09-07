# rwebq

## An R interface to the University of Washington [WebQ REST API](https://wiki.cac.washington.edu/display/aca/WebQ+REST+API) for Online Surveys

`rwebq` pulls data from the UW Catalyst WebQ survey API and formats it in easy-to-use R tibbles. 

This project was created for my own use is not maintained by UW IT. It therefore does not support all of the functionality of the WebQ REST API, only the features I needed for my own work. Feel free to reach out and/or submit a PR if you would like to collaborate on extending this package. 

**NOTE:** Catalyst WebQ is available for members of University of Washington only. 

### Setup

You can install this package from Github using `devtools::install_github('lpiep/rwebq')`.

Next, you must [generate a private key](https://catalyst.uw.edu/rest_user) using your UW login. Once you have done so, store your credentials using the `webq_config` function. This function stores your netid and API key as environment variables (`CATALYST_NETID`, `CATALYST_KEY`). 

```
webq_config(netid = 'sue', key = 'abcdefghijklmnopqrstuvwxyz123456789')
```

### Usage 

You'll need to find the `survey_id` for your survey in order to pull data from it. You can list all of the surveys you have access to with `webq_surveys()`. 

```
> webq_surveys()
# A tibble: 3 x 3
  name                                        survey_id response_count
  <chr>                                       <chr>              <dbl>
1 My Cool Survey                              123456               176
2 My Cooler Survey                            654321               117
3 My Less Cool Survey                         010101                86
```

You can also list the `participant_id`s, along with the times when they took the survey. The function also returns the `rest_id`, which is included in the API response, but does not appear to be populated (at least for my surveys). 

```
> webq_participants('123456')
# A tibble: 3 x 5
   survey_id participant_id rest_id   start_date          end_date           
   <chr>     <chr>          <list>    <chr>               <chr>              
 1 123456    20595146       <chr [0]> 2021-05-04 08:45:48 2021-05-04 09:02:15
 2 123456    19349250       <chr [0]> 2019-12-17 10:25:05 2019-12-17 10:31:18
 3 123456    18334462       <chr [0]> 2018-11-14 17:46:59 2018-11-14 23:56:27
```

And, of course, you can pull the responses to your survey. The API supports filtering responses by `participant_id`, and so you can provide a character vector of `participant_ids` to the `webq_responses` function. If you don't provide this vector, the function returns responses for all participants. The API does not support filtering by `question_id`, but you can filter the results afterwards. 

```
> webq_responses('123456') # We didn't provide `participant_ids`, so all participant's responses will be returend. 
# A tibble: 3 x 6
   question_id question_type            question_content             text_values           numeric_values participant_id
   <chr>       <chr>                    <chr>                        <chr>                 <chr>          <chr>         
 1 3657118     Multiple choice - multi… What is your favorite color? "Orange\nMagenta"     "3\n5"         20595146      
 2 3657118     Multiple choice - multi… What is your favorite color? "Pink\nGreen"         "1\n2"         19349250      
 3 3657118     Multiple choice - multi… What is your favorite color? "Blue"                "7"            18334462 
```

I've selected what I considered to be the most useful elements returned by the API to include in the result tibble. Each response contains both a text and numeric version of the participant's answer. Depending on the question type, one or both will be populated. The exact format of the values varies based on the question type. 
