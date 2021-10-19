#include "communication/communicator.h"
#include "logging.h"
#include <stdio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

using namespace icsharp;

#define FIFO_FILE "/tmp/concolic_fifo"

int fd;

bool reportError() {
    ERROR(tout << strerror(errno));
    return false;
}
bool Communicator::open() {
    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) 
        return reportError();
    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, FIFO_FILE, sizeof(addr.sun_path)-1);
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
