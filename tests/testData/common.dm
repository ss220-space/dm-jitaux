#define CHECK_INSTALL_COMPILED file("result.txt") << install_compiled()
#define RES(T) file("result.txt") << T

/world/New()
    hook_main_init()
    do_test()
    exit_test()

/proc/exit_test()
    CRASH("hook not loaded")