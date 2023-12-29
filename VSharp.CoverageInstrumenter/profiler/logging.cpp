#include "logging.h"

#ifdef _LOGGING

std::ofstream tout;
std::recursive_mutex logMutex;

void open_log(const char *&logName) {
    tout.open(logName, std::ios_base::app);
}

void close_log() {
    tout.close();
}

#endif
