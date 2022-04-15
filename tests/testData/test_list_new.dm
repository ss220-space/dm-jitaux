/proc/do_test()
    compile_proc(/proc/create_vector_list)
    compile_proc(/proc/create_assoc_vector_list)
    compile_proc(/proc/create_assoc_list)
    CHECK_INSTALL_COMPILED // RES: /create_vector_list, /create_assoc_vector_list, /create_assoc_list

    var/list/l = create_vector_list(2, 10, 12)

    RES(l[1]) // RES: 2
    RES(l[2]) // RES: 10
    RES(l[3]) // RES: 12

    l = create_assoc_vector_list(2, 10, 12)

    RES(l[1]) // RES: 2
    RES(l[2]) // RES: 10
    RES(l[3]) // RES: 12

    l = create_assoc_list(2, 10, 12)

    RES(l["aaa"]) // RES: 2
    RES(l["bbb"]) // RES: 10
    RES(l["ccc"]) // RES: 12

/proc/create_vector_list(a, b, c)
    return list(a, b, c)

/proc/create_assoc_vector_list(a, b, c)
    return list(1 = a, 2 = b, 3 = c)

/proc/create_assoc_list(a, b, c)
    return list("aaa" = a, "bbb" = b, "ccc" = c)