#include "communicator.h"
#include "../logging.h"
#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <tchar.h>
#include <strsafe.h>
#include <string>

using namespace vsharp;

HANDLE hPipe;

bool reportError() {
    LOG_ERROR(tout << "WinAPI error code = " << GetLastError());
    return false;
}

bool Communicator::open() {
    std::wstring pipeEnvVar = L"CONCOLIC_PIPE";
    const wchar_t *pipeFile = _wgetenv(pipeEnvVar.c_str());
    std::wstring pipe(pipeFile);
    if (pipe.size() < 1) FAIL_LOUD("Invalid pipe environment variable!");
    hPipe = CreateFile(pipeFile, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hPipe == INVALID_HANDLE_VALUE) return reportError();
    return true;
}

int Communicator::read(char *buffer, int count) {
    DWORD cbRead = -1;
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
