use test_common::DMTest;

fn main() {
    DMTest::new(
        "bench_gas_mixture",
        "benches/tmp",
        vec!["benches/testData", "tests/testData"]
    ).run_hook_and_assert_result(
        vec![
            "gas_mixture.dm",
            "bench_gas_mixture.dm"
        ]
    )
}
