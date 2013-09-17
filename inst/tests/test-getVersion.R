context('getVersion')

test_that('getVersion handles the known versions', {
    expect_that(getVersion("http://psidev.info/psi/pi/mzIdentML/1.1"), equals('1.1'))
    expect_that(getVersion("http://psidev.info/psi/pi/mzIdentML/1.0"), equals('1.0'))
})

test_that('getVersion recognizes unknown versions and invalid xml files', {
    expect_that(getVersion("http://psidev.info/psi/pi/mzIdentML/0.9"), throws_error("Version 0.9 unknown: only 1.0 and 1.1 supported."))
    expect_that(getVersion("dummyNS"), throws_error("Version dummyNS unknown: only 1.0 and 1.1 supported."))
})