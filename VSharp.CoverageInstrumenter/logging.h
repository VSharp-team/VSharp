#ifndef LOGGING_H_
#define LOGGING_H_

#ifdef _CYGWIN
#undef max
#undef min
#endif
#include <fstream>

#define HEX(x) std::hex << "0x" << (x) << std::dec

#ifdef _LOGGING
extern std::ofstream tout; 
#define LOG_CODE(CODE) { CODE } ((void) 0)

void open_log(const char*& logName);
void close_log();

#else
#define LOG_CODE(CODE) ((void) 0)

static inline void open_log() {}
static inline void close_log() {}
#endif

#define LOG(CODE) LOG_CODE(CODE ; tout << "\n"; tout.flush();)
//#define LOG(CODE) LOG_CODE(tout << "---------------- " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush();)
//#define SLOG(CODE) LOG_CODE(CODE ; tout.flush();)
//#define CLOG(COND, CODE) LOG_CODE(if (COND) { tout << "---------------- " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush(); })
#define CLOG(COND, CODE) LOG_CODE(if (COND) { CODE ; tout << "\n"; tout.flush(); })
#define LOG_ERROR(CODE) LOG_CODE(tout << "-------- [LOG_ERROR] " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush();)
#define FAIL_LOUD(x) {LOG_ERROR(tout << (x)); throw std::logic_error(x);}

#endif // LOGGING_H_
