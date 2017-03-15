# a function to retrieve sample ids for ligation ids
sampforlig <- function(x){
  # select ligation ids of interest
  # Add sample IDs ----------------------------------------------------------
  labor <-  dplyr::src_mysql(dbname = "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  c1 <- labor %>% tbl("ligation") %>% filter(ligation_id %in% x) %>% select(ligation_id, digest_id) %>% collect()
  c2 <- labor %>% tbl("digest") %>% filter(digest_id %in% c1$digest_id)  %>% select(digest_id, extraction_id) %>% collect()
  c2 <- left_join(c1, c2, by = "digest_id")
  c3 <- labor %>% tbl("extraction") %>% filter(extraction_id %in% c2$extraction_id) %>% select(extraction_id, sample_id) %>% collect()
  c3 <- left_join(c2, c3, by = "extraction_id")
  c3 <- c3[ , c("ligation_id", "sample_id")]
  rm(labor)
  return(c3)
}