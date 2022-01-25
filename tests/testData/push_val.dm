/proc/do_test()
    compile_proc(/proc/get)
    CHECK_INSTALL_COMPILED // RES: /get

    RES(get()) // RES: "Hello, world!"

/proc/get()
    return "Hello, world!"