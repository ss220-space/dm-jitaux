/proc/do_test()
    compile_proc(/proc/do_call_global)
    compile_proc(/proc/call_me_compile)
    CHECK_INSTALL_COMPILED // RES: /do_call_global, /call_me_compile

    RES(do_call_global()) // RES: "COMPILED"

/proc/do_call_global()
    call_me()
    return call_me_compile()

/proc/call_me()
    RES("CALLED") // RES: "CALLED"

/proc/call_me_compile()
    return "COMPILED"