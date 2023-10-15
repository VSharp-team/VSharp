#include "./profiler/os.h"

#include <windows.h>

std::string OS::unicodeToAnsi(const WCHAR *str) {
    std::wstring ws(str);
    return std::string{ws.begin(), ws.end()};
}

void OS::sleepSeconds(int seconds) {
    Sleep(seconds * 1000);
}