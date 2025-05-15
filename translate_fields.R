###
# Call DeepL API to translate REDCap Metadata from Deutsch to English
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

#Setup base API request
api_url = 'https://api.deepl.com/v2/translate'
authkey = 'DeepL-Auth-Key [insert API key]' # do not hard-code this...

base_request = request(api_url)
base_request = base_request |> req_method('POST') |> req_headers('Content-Type' = 'application/json', 'Authorization' = auth_key)

## 1. let's translate the form_names
# There are 52 form names. Split into two requests
df_formnames = tibble(form_name = unique(metadata$form_name))
translated_data = c()
parts = (nrow(df_formnames)/50) |> ceiling()

for(i in 1:parts)
{
  text_input = df_formnames |> slice((1+(i-1)*50):(i*50)) %>% `[[`('form_name')
  
  request = base_request |> req_body_json(list(text = text_input,  target_lang = 'EN-GB', source_lang = 'DE'))
  response = req_perform(request)
  response_text = response |> resp_body_json() %>% `[[`('translations') |> lapply(`[[`, 2) |> unlist() 
  # Note the %>%, |> doesn't work for this particular situation
  
  translated_data = c(translated_data, response_text)
}

df_formnames = df_formnames |> mutate(form_name_EN = translated_data)
# Translation Complete

#2. Let's translate the field_label's
translated_data = c()
parts = (nrow(metadata)/50) |> ceiling()
metadata = metadata %>% mutate(field_label = ifelse(is.na(field_label), 'logo', field_label)) # minor fix to handle NAs

for(i in 11:parts)
{
  text_input = metadata |> slice((1+(i-1)*50):(i*50)) %>% `[[`('field_label')
  
  request = base_request |> req_body_json(list(text = text_input,  target_lang = 'EN-GB', source_lang = 'DE'))
  response = req_perform(request)
  response_text = response |> resp_body_json() %>% `[[`('translations') |> lapply(`[[`, 2) |> unlist() 
  # Note the %>%, |> doesn't work for this particular situation
  
  translated_data = c(translated_data, response_text)
}

# add to metadata
metadata = metadata |> mutate(field_label_EN = translated_data)

# add formnames to metadata
metadata = left_join(metadata, df_formnames, by = 'form_name')

# rearrange
metadata = metadata %>% select(field_name, form_name, form_name_EN, field_label, field_label_EN)

# write to file
outfile = '/file/to/write/translations.csv'
data.table::fwrite(metadata, outfile, sep = ',', quote = TRUE)



