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
fn test_cmp() {
    run_hook_and_assert_result(vec!("test_cmp.dm"));
}

#[test]
fn test_deopt() {
    run_hook_and_assert_result(vec!("test_deopt.dm"));
}

#[test]
fn test_mul() {
    run_hook_and_assert_result(vec!("test_mul.dm"));
}

#[test]
fn test_add() {
    run_hook_and_assert_result(vec!("test_add.dm"));
}

#[test]
fn test_call_global() {
    run_hook_and_assert_result(vec!("test_call_global.dm"));
}

#[test]
fn test_call_static() {
    run_hook_and_assert_result(vec!("test_call_static.dm"));
}

#[test]
fn test_call_dynamic() {
    run_hook_and_assert_result(vec!("test_call_dynamic.dm"));
}

#[test]
fn test_abs() {
    run_hook_and_assert_result(vec!("test_abs.dm"));
}

#[test]
fn test_if_bool_ops() {
    run_hook_and_assert_result(vec!("test_if_bool_ops.dm"));
}