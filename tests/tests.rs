use test_common::DMTest;

const TEST_DATA_DIR: &str = "tests/testData";
const TEST_WORK_DIR: &str = "tests/tmp";

macro_rules! test_dm {
    ($file:ident $(+ $($additional:literal),+)?) => {
        #[test]
        fn $file() {
            DMTest::new(stringify!($file), TEST_WORK_DIR, vec![TEST_DATA_DIR])
                .run_hook_and_assert_result(
                    vec!(
                        $($($additional),+,)?
                        concat!(stringify!($file), ".dm")
                    )
                );
        }
    };
}

test_dm!(test_total_moles);
test_dm!(test_jz);
test_dm!(test_args);
test_dm!(push_val);
test_dm!(test_cmp);
test_dm!(test_deopt);
test_dm!(test_mul);
test_dm!(test_div);
test_dm!(test_add);
test_dm!(test_and_or);
test_dm!(test_call_global);
test_dm!(test_call_static);
test_dm!(test_call_dynamic);
test_dm!(test_abs);
test_dm!(test_if_bool_ops);
test_dm!(test_sub);
test_dm!(test_aug_op);
test_dm!(test_is + "test_is_turf.dmm");
test_dm!(test_ref_count);
test_dm!(test_local_var);
test_dm!(test_null_cmp);
test_dm!(test_call_args);
test_dm!(test_round);
test_dm!(test_gas_mixture_share + "gas_mixture.dm");
test_dm!(test_list_get_set);
test_dm!(test_list_binary_ops);
test_dm!(test_loops);
test_dm!(test_inc_dec);