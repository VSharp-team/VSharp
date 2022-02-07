#include "communicator.h"
#include "../logging.h"
#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <tchar.h>
#include <strsafe.h>

using namespace icsharp;

HANDLE hPipe;

bool reportError() {
    ERROR(tout << strerror(errno));
    return false;
}

bool Communicator::open() {
    std::string pipeEnvVar = "CONCOLIC_PIPE";
    auto pipeFile = getenv(pipeEnvVar.c_str());
    hPipe = CreateFile(TEXT(pipeFile), GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    if (hPipe == INVALID_HANDLE_VALUE) reportError();
//    DWORD dwMode = PIPE_TYPE_MESSAGE;
//    BOOL fSuccess = SetNamedPipeHandleState(hPipe, &dwMode, NULL, NULL);
//    if (!fSuccess) reportError();
    return true;
}

int Communicator::read(char *buffer, int count) {
    DWORD cbRead;
    BOOL fSuccess = ReadFile(hPipe, buffer, count, &cbRead, NULL);

    if (!fSuccess || cbRead < 0) reportError();
    return cbRead;
}

int Communicator::write(char *message, int count) {
    DWORD cbWritten;
    BOOL fSuccess = WriteFile(hPipe, message, count, &cbWritten, NULL);

    if (!fSuccess || cbWritten < 0) reportError();
    return cbWritten;
}

bool Communicator::close() {
    CloseHandle(hPipe);
    return true;
}
