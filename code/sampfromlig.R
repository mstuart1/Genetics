# find sample id from ligation id

sampfromlig <- function(x){
  source("../../myRcode/Laboratory/R/conlabor.R")
  labor <- conlabor()
  # connect ligation ids to digest ids
  dig <- labor %>% tbl("ligation") %>% filter(ligation_id %in% x$names) %>% select(ligation_id, digest_id) %>% collect()
  # connect digest ids to extraction ids
  extr <- labor %>% tbl("digest") %>% filter(digest_id %in% dig$digest_id) %>% select(digest_id, extraction_id) %>% collect()
  extr_id <- left_join(dig, extr, by = "digest_id")
  # connect extraction ids to sample ids
  samp <- labor %>% tbl("extraction") %>% filter(extraction_id %in% extr_id$extraction_id) %>% select(extraction_id, sample_id) %>% collect() 
  samp_id <- left_join(extr_id, samp, by = "extraction_id")
  # remove unnecessary columns
  samp_id <- samp_id[ , c("ligation_id", "sample_id")]
  return(samp_id)
}
