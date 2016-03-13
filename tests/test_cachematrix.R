library('testthat')

source('../cachematrix.R')

context('cachedMatrix')

cm <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
expected_inverse <- matrix(c(-2, 1, 1.5, -0.5), nrow=2)

test_that('cached matrix starts with no cached results', {
    expect_that(cm$getinverse(), is_identical_to(NULL))
})

test_that('after cacheSolve is used, a result is cached', {
    expect_that(cm$getinverse(), equals(NULL))
    cacheSolve(cm)
    expect_that(cm$getinverse(), equals(expected_inverse))
})

test_that('in a second call to cacheSolve, the cached result is used', {
    cacheSolve(cm)
    expect_that(cacheSolve(cm), shows_message('returning cached result'))
})

test_that('after a call to set, the cache is erased', {
    cacheSolve(cm)
    expect_that(cm$getinverse(), equals(expected_inverse))
    cm$set(matrix())
    expect_that(cm$getinverse(), equals(NULL))
})


