/proc/do_test()
    compile_proc(/proc/create_vector_list)
    compile_proc(/proc/create_assoc_list)
    compile_proc(/proc/create_assoc_list_deopt)
    CHECK_INSTALL_COMPILED // RES: /create_vector_list, /create_assoc_list, /create_assoc_list_deopt

    var/list/l = create_vector_list(2, 10, 12)
    RES(json_encode(l)) // RES: "[2,10,12,2,10,12,2,10,12]"

    l = create_assoc_list(2, 10, 12)
    RES(json_encode(l)) // RES: "{\"aaa\":2,\"bbb\":10,\"ccc\":12,\"ddd\":2,\"eee\":10,\"fff\":12,\"ggg\":2,\"hhh\":10,\"iii\":12}"

    l = create_assoc_list_deopt(2, 10, 12)
    RES(json_encode(l)) // RES: "[2,10,12,0]"

/proc/create_vector_list(a, b, c)
    return list(a, b, c, a, b, c, a, b, c)

/proc/create_assoc_list(a, b, c)
    return list("aaa" = a, "bbb" = b, "ccc" = c, "ddd" = a, "eee" = b, "fff" = c, "ggg" = a, "hhh" = b, "iii" = c)

/proc/create_assoc_list_deopt(a, b, c)
    return list(1 = a, 2 = b, 3 = c) + dmjit_is_optimized()