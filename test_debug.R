devtools::load_all(".")
data(hazard)
si <- hazard[["ID"]][seq_len(ceiling(nrow(hazard) / 2))]
cat("subset_ids:", si, "\n")

ad_s <- ped2add(hazard, sparse = TRUE, keep_ids = si)
cat("ad_small dimnames:", rownames(ad_s), "\n")

cn_f <- ped2cn(hazard, sparse = TRUE)
cat("cn_full dimnames:", rownames(cn_f), "\n")

cn_s <- ped2cn(hazard, sparse = TRUE, keep_ids = si)
cat("cn_small dimnames:", rownames(cn_s), "\n")

# Check: does com2links produce same output for both?
mit_f <- ped2mit(hazard, sparse = TRUE)
r1 <- com2links(ad_ped_matrix = ad_s, mit_ped_matrix = mit_f, cn_ped_matrix = cn_f, writetodisk = FALSE)
cat("r1 rows:", nrow(r1), "\n")

mit_s <- ped2mit(hazard, sparse = TRUE, keep_ids = si)
r2 <- com2links(ad_ped_matrix = ad_s, mit_ped_matrix = mit_s, cn_ped_matrix = cn_s, writetodisk = FALSE)
cat("r2 rows:", nrow(r2), "\n")

# Show difference
diff_ids <- setdiff(paste(r1$ID1, r1$ID2), paste(r2$ID1, r2$ID2))
cat("Extra in r1:", diff_ids, "\n")
diff_ids2 <- setdiff(paste(r2$ID1, r2$ID2), paste(r1$ID1, r1$ID2))
cat("Extra in r2:", diff_ids2, "\n")
