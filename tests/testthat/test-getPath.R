context('getPath')

test_that('getPath handles the known versions', {
    expect_that(getPath("http://psidev.info/psi/pi/mzIdentML/1.1"), equals('/x:MzIdentML'))
    expect_that(getPath("http://psidev.info/psi/pi/mzIdentML/1.0"), equals('/x:mzIdentML'))
})