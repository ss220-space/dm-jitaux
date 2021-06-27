use base::run_hook_test;
use base::test_result;
mod base;

#[test]
fn test_total_moles_func() {
    run_hook_test(vec!("total_moles.dm"));
    assert_eq!(test_result().trim(), "63")
}

#[test]
fn test_jz() {
    run_hook_test(vec!("test_jz.dm"));
    assert_eq!(test_result().trim(), "1\n0")
}

#[test]
fn test_args() {
    run_hook_test(vec!("test_args.dm"));
    assert_eq!(test_result().trim(), "11\n15.5")
}

#[test]
fn test_push_val() {
    run_hook_test(vec!("push_val.dm"));
    assert_eq!(test_result().trim(), "Hello, world!")
}

#[test]
fn test_tg_op() {
    run_hook_test(vec!("test_tg_op.dm"));
    assert_eq!(test_result().trim(), "b\na\neq")
}

#[test]
fn test_deopt() {
    run_hook_test(vec!("test_deopt.dm"));
    assert_eq!(test_result().trim(), "/do_deopt\n2")
}