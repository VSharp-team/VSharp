#include "logging.h"

#ifdef _LOGGING

std::ofstream tout;

void open_log(const char *&logName) {
    tout.open(logName);
}

void close_log() {
    tout.close();
}

#endif
