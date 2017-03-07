context("sc-rgl")

library(rgl)
y <- subdivision3d(tetrahedron3d(col = "red"), depth = 3)


test_that("get rgl objects into shape", {
  PRIMITIVE(y) %>% expect_s3_class("PATH") %>% expect_s3_class("sc")

})
