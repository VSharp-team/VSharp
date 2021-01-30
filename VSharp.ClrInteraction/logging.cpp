#include "logging.h"

#ifdef _LOGGING

#define LOG_FILE_NAME "lastrun.log"

std::ofstream tout;

void open_log() {
    tout.open(LOG_FILE_NAME);
}

void close_log() {
    tout.close();
}

#endif
