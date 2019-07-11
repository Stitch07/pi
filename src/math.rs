pub fn factorial(n: i64) -> i64 {
    match n {
        0 | 1 => 1,
        _ => n * factorial(n - 1),
    }
}

#[test]
fn test_factorial() {
    macro_rules! fac_test {
        ($case:expr, $expected:expr) => {
            assert_eq!(factorial($case), $expected)
        };
    }

    fac_test!(0, 1);
    fac_test!(1, 1);
    fac_test!(5, 120);
}
