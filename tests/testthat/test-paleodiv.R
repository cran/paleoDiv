test_that("divdistr_() works", {
testdf<-data.frame(taxon=c("A","B","C"), max=c(200,190,180),min=c(180,180,170))

divdistr_(c(195,185,175),table=testdf)->testres#make a taxon-range table manually with three taxa with ranges 200-180,190-180 and 180-170 ma

  expect_equal(testres[1], 1)
  expect_equal(testres[2], 2)
  expect_equal(testres[3], 1)
  
  expect_equal(divdistr_(c(180),table=testdf), 3)


  })
