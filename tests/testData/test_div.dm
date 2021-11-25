/proc/do_test()
    compile_proc("/proc/div")
    CHECK_INSTALL_COMPILED // RES: /div

    RES(div(2, 10)) // RES: 0.2
    RES(div(2, list())) // RES:
    RES(div(2, "test")) // RES:
    RES(div(2, 0.4)) // RES: 5



/proc/div(a, b)
    return a / b