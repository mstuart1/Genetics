# write out a summary of issues


paste(unique(work$sample_id), " was extracted ", length(unique(work$extraction_id)), "times, ", unique(work$extraction_id), ",  was digested ", length(unique(work$digest_id)), "times",  unique(work$digest_id)," and ligated", length(unique(work$ligation_id)), "times",  unique(work$ligation_id), ".  ", iss$Ligation_ID[i], " brought up a red flag because ", iss$Issue[i],". ", sep = "")
for (j in 1:nrow(work)){
  if (is.na(work$ligation_id[j])){
    paste(Digest)
  }
}