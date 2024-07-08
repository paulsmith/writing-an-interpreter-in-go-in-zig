#include <unistd.h>
#include <pwd.h>

const char *getusername()
{
    struct passwd *pw;
    pw = getpwuid(geteuid());
    return pw->pw_name;
}
