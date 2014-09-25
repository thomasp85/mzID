context('isRemote')

test_that('isRemote', {
    expect_true(mzID:::isRemote("http://www.google.com"))
    expect_true(mzID:::isRemote("https://www.google.com"))
    expect_true(mzID:::isRemote("ftp://kernel.org"))
    expect_true(mzID:::isRemote("ftps://kernel.org"))

    expect_false(mzID:::isRemote("/home/foo/bar.txt"))
    expect_false(mzID:::isRemote("C:\\Windows\\bar.txt"))
})

