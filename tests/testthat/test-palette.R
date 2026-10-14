test_that("metro_palette() matches dagviz, via Python's colorsys.hls_to_rgb", {
  # RGB values hard-coded from:
  #   python3 -c "import colorsys
  #   for h,l,s in cases: print(colorsys.hls_to_rgb(h,l,s))"
  # dagviz's palette() steps hue by 1/6 (7 steps, wrapping) at lightness
  # 0.55, 0.7, 0.4 in turn, saturation 0.5, and converts with floor(x * 256).
  cases <- list(
    list(i = 1, rgb = c(0.775, 0.325, 0.325)), # h = 0,   l = 0.55
    list(i = 2, rgb = c(0.775, 0.775, 0.325)), # h = 1/6, l = 0.55
    list(i = 3, rgb = c(0.325, 0.775, 0.325)), # h = 2/6, l = 0.55
    list(i = 4, rgb = c(0.325, 0.775, 0.775)), # h = 3/6, l = 0.55
    list(i = 5, rgb = c(0.325, 0.325, 0.775)), # h = 4/6, l = 0.55
    list(i = 6, rgb = c(0.775, 0.325, 0.775)), # h = 5/6, l = 0.55
    list(i = 7, rgb = c(0.85, 0.55, 0.55)), # h = 0,   l = 0.7
    list(i = 8, rgb = c(0.85, 0.85, 0.55)), # h = 1/6, l = 0.7
    list(i = 9, rgb = c(0.55, 0.85, 0.55)), # h = 2/6, l = 0.7
    list(i = 11, rgb = c(0.55, 0.55, 0.85)), # h = 4/6, l = 0.7
    list(i = 13, rgb = c(0.6, 0.2, 0.2)), # h = 0,   l = 0.4
    list(i = 14, rgb = c(0.6, 0.6, 0.2)), # h = 1/6, l = 0.4
    list(i = 16, rgb = c(0.2, 0.6, 0.6)), # h = 3/6, l = 0.4
    list(i = 18, rgb = c(0.6, 0.2, 0.6)) # h = 5/6, l = 0.4
  )
  pal <- metro_palette(18)
  for (case in cases) {
    v <- floor(case$rgb * 256)
    expect_identical(
      pal[case$i],
      sprintf("#%02x%02x%02x", v[1], v[2], v[3]),
      info = as.character(case$i)
    )
  }
})

test_that("metro_palette(0) returns an empty vector", {
  expect_equal(metro_palette(0), character(0))
})

test_that("metro_palette(n) equals the first n colors of metro_palette(n + k)", {
  big <- metro_palette(60)
  expect_equal(metro_palette(21), big[1:21])
  expect_equal(metro_palette(40), big[1:40])
})

test_that("metro_palette never repeats a color", {
  for (n in c(1, 18, 19, 21, 50, 200)) {
    p <- metro_palette(n)
    expect_length(p, n)
    expect_false(anyDuplicated(p) > 0, info = as.character(n))
    expect_true(all(grepl("^#[0-9a-f]{6}$", p)), info = as.character(n))
  }
})

test_that("metro_palette validates n", {
  expect_error(metro_palette(-1))
  expect_error(metro_palette(c(1, 2)))
  expect_error(metro_palette(NA))
})

test_that("metro_palette(n) is lowercase hex and keeps the 18 dagviz colors first", {
  p <- metro_palette(40)
  expect_identical(p[1:18], metro_palette(18))
  expect_identical(p, tolower(p))
  expect_identical(metro_palette(1.0), metro_palette(1L))
})

test_that("metro_palette() stops, rather than loop forever, when no extra color can be told apart", {
  # Using up the real palette takes about a thousand colors and hundreds of
  # thousands of slow Lab conversions. Make every color indistinguishable
  # instead, so no candidate is ever kept and the search hits its try limit.
  testthat::local_mocked_bindings(
    lab_of = function(hex) matrix(0, nrow = length(hex), ncol = 3)
  )
  expect_error(
    metro_palette(19),
    "Could not generate that many distinct colors"
  )
  # up to the 18 base colors, the search never runs
  expect_length(metro_palette(18), 18)
  expect_identical(metro_palette(0), character(0))
})
