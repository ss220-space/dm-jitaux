use base::run_hook_test;
use base::run_hook_and_assert_result;
use base::test_result;
mod base;

#[test]
fn test_total_moles_func() {
    run_hook_and_assert_result(vec!("total_moles.dm"));
}

#[test]
fn test_jz() {
    run_hook_and_assert_result(vec!("test_jz.dm"));
}

#[test]
fn test_args() {
    run_hook_and_assert_result(vec!("test_args.dm"));
}

#[test]
fn test_push_val() {
    run_hook_and_assert_result(vec!("push_val.dm"));
}

#[test]
fn test_tg_op() {
    run_hook_and_assert_result(vec!("test_tg_op.dm"));
}

#[test]
fn test_deopt() {
    run_hook_and_assert_result(vec!("test_deopt.dm"));
}