/proc/do_test()
    compile_proc("/proc/greater")
    install_compiled()

    file("result.txt") << greater(1, 2)
    file("result.txt") << greater(2, 1)
    file("result.txt") << greater(2, 2)

/proc/greater(a, b)
    if (a > b)
        return "a"
    if (b > a)
        return "b"
    return "eq"