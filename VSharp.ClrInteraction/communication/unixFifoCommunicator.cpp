#include "communicator.h"
#include "../logging.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>

using namespace vsharp;

int fd;

bool reportError() {
    LOG_ERROR(tout << strerror(errno));
    return false;
}

bool Communicator::open() {
    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0)
        return reportError();
    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    std::string pipeEnvVar = "CONCOLIC_PIPE";
    auto pipeFile = getenv(pipeEnvVar.c_str());
    if (strlen(pipeFile) < 1) FAIL_LOUD("Invalid pipe environment variable!");
    strncpy(addr.sun_path, pipeFile, strlen(pipeFile));
    if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0)
        return reportError();
    return true;
}

int Communicator::read(char *buffer, int count) {
    int bytes = ::read(fd, buffer, count);
//    LOG(tout << "read " << count << " bytes: " << buffer);
    if (bytes < 0) reportError();
    return bytes;
}

int Communicator::write(char *message, int count) {
//    LOG(tout << "writing " << count << " bytes: " << message);
    int bytes = ::write(fd, message, count);
    if (bytes < 0) reportError();
    return bytes;
}

bool Communicator::close() {
    if (::close(fd)) 
        return reportError();
    return true;   
}
