###
# Call DeepL API to translate REDCap Metadata (Choices and Selections) from Deutsch to English
#
# Read metadata
# Set up request to DeepL API
# translate 50  texts per request
# extract translations and add to a dataframe
# create 5 column dataframe with english translations
###

library(httr2)
library(dplyr)
## Read in document
metadata_file = 'file/to/metadata/extract.csv'
metadata = readr::read_csv(metadata_file)

# remove all NAs
# remove all field_type == 'calc'
# get distinct selection of choices to avoid duplicate translations

metadata_tmp = metadata |> dplyr::filter(!is.na(select_choices_or_calculations) & field_type != 'calc')
metadata_distinct = metadata_tmp |> dplyr::distinct(select_choices_or_calculations)

#Setup base API request
api_url = 'https://api.deepl.com/v2/translate'
authkey = 'DeepL-Auth-Key [insert API key]' # do not hard-code this...

base_request = request(api_url)
base_request = base_request |> req_method('POST') |> req_headers('Content-Type' = 'application/json', 'Authorization' = authkey)

## 1. let's translate the 'select_choices_or_calculations'
translated_data = c()
parts = (nrow(metadata_distinct)/50) |> ceiling()

for(i in 2:parts)
{
  text_input = metadata_distinct |> slice((1+(i-1)*50):(i*50)) %>% `[[`('select_choices_or_calculations')
  
  request = base_request |> req_body_json(list(text = text_input,  target_lang = 'EN-GB', source_lang = 'DE'))
  response = req_perform(request)
  response_text = response |> resp_body_json() %>% `[[`('translations') |> lapply(`[[`, 2) |> unlist() 
  # Note the %>%, |> doesn't work for this particular situation
  
  translated_data = c(translated_data, response_text)
}

metadata_distinct = metadata_distinct |> mutate(choices_EN = translated_data)
# Translation Complete

# add choices to metadata
metadata = left_join(metadata, metadata_distinct, by = 'select_choices_or_calculations')

# write to file
outfile = 'file/to/write/translations.csv'
data.table::fwrite(metadata, outfile, sep = ',', quote = TRUE)

