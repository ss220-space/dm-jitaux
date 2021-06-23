/proc/do_test()
    compile_proc("/proc/get")
    install_compiled()

    file("result.txt") << get()

/proc/get()
    return "Hello, world!"