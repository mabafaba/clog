#
#
# context("test-apply_cleaninglog")
# my_cleaning_log<-read.csv2(file = "./tests/testthat/test_cl.csv",sep = ",", stringsAsFactors = F)
# my_raw_data<-read.csv(file = "./tests/testthat/test_rawdata.csv", sep =",", stringsAsFactors = F)
#
#
#
# cl<-cleaninglog(ids = t_cl$uuid,
#                 variables = t_cl$Question,
#                 new_values = t_cl$New.Value,
#                 data_id_column_name = "X_uuid",
#                 name = "same",
#                 info = t_cl$Comments, enumerator_id = t_cl$Enumerator,issue_name = "some issue")
#
#
# cleaned_data <- raw %>% clog_clean(cl)
#
#
# data_with_issues <- clog_data_to_cleaninglog(data = raw,cl = cl,cl_item_columns = cl$issue_name)
#
#
#
#
# write.csv(dirty,"./tests/testthat/test_rawdata.csv")
#
# test_that("cleaning log changes are applied", {
#   expect_equal(2 * 2, 4)
# })
#
#
#
