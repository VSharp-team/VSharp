#ifndef LOGGING_H_
#define LOGGING_H_

#ifdef _CYGWIN
#undef max
#undef min
#endif
#include <fstream>

#ifdef _LOGGING
extern std::ofstream tout; 
#define LOG_CODE(CODE) { CODE } ((void) 0)

void open_log();
void close_log();

#else
#define LOG_CODE(CODE) ((void) 0)

static inline void open_log() {}
static inline void close_log() {}
#endif

#define LOG(CODE) LOG_CODE(tout << "---------------- " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush();)
#define SLOG(CODE) LOG_CODE(CODE ; tout.flush();)
#define CLOG(COND, CODE) LOG_CODE(if (COND) { tout << "---------------- " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush(); })
#define ERROR(CODE) LOG_CODE(tout << "-------- [ERROR] " << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << " ---------\n"; CODE ; tout << "------------------------------------------------\n"; tout.flush();)
#define FAIL_LOUD(x) {ERROR(tout << x); throw std::logic_error(x);}
//#define FAIL_LOUD(x) {ERROR(tout << x); std::cout << std::endl << x << std::endl;}
//#define FAIL_LOUD(x) {ERROR(tout << x); std::cout << std::endl << x << std::endl; throw std::logic_error(x);}

#endif // LOGGING_H_
