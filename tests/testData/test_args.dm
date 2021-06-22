/proc/do_test()
    compile_proc("/proc/sum")
    install_compiled()

    file("result.txt") << sum(1, 10)
    file("result.txt") << sum(15, 0.5)

/proc/sum(a, b)
    return a + b