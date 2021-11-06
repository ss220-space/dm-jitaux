#define CHECK_INSTALL_COMPILED file("result.txt") << install_compiled()
#define WRITE_RESULT(T) file("result.txt") << T
#define RES(T) WRITE_RESULT(T)

/world/New()
    hook_main_init()
    world.log = file("world.txt")
    do_test()
    exit_test()

/proc/exit_test()
    CRASH("hook not loaded")