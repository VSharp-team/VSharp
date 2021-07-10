#include "communication/protocol.h"
#include "logging.h"
#include "probes.h"

#include <cstring>
#include <iostream>

using namespace icsharp;

char confirmationByte = 0x55;
char instrumentCommandByte = 0x56;
char executeCommandByte = 0x57;
char *confirmationMessage = new char[1] {confirmationByte};
char *instrumentationCommandMessage = new char[1] {instrumentCommandByte};

bool Protocol::readConfirmation() {
    char *buffer = new char[1];
    int bytesRead = m_communicator.read(buffer, 1);
    if (bytesRead != 1 || buffer[0] != confirmationByte) {
        ERROR(tout << "Communication with server: could not get the confirmation message. Instead read"
                   << bytesRead << " bytes with message [";
              for (int i = 0; i < bytesRead; ++i) tout << buffer[i] << " ";
              tout << "].");
        delete[] buffer;
        return false;
    }
    delete[] buffer;
    return true;
}

bool Protocol::writeConfirmation() {
    int bytesWritten = m_communicator.write(confirmationMessage, 1);
    if (bytesWritten != 1) {
        ERROR(tout << "Communication with server: could not send the confirmation message. Instead sent"
                   << bytesWritten << " bytes.");
        return false;
    }
    return true;
}

bool Protocol::readCount(int &count) {
    int countCount = m_communicator.read((char*)(&count), 4);
    if (countCount != 4) {
        ERROR(tout << "Communication with server: could not get the amount of bytes of the next message. Instead read " << countCount << " bytes");
        return false;
    }

    return true;
}

bool Protocol::writeCount(int count) {
    int bytesWritten = m_communicator.write((char*)(&count), 4);
    if (bytesWritten != 4) {
        ERROR(tout << "Communication with server: could not sent the amount of bytes of the next message. Instead sent " << bytesWritten << " bytes");
        return false;
    }
    return true;
}

bool Protocol::readBuffer(char *&buffer, int &count) {
    if (!readCount(count)) {
        return false;
    }
    if (count <= 0) {
        ERROR(tout << "Communication with server: the amount of bytes is unexpectedly non-positive (count = " << count << ") ");
        return false;
    }
    if (!writeConfirmation()) return false;
    buffer = new char[count];
    int bytesRead = m_communicator.read(buffer, count);
    if (bytesRead != count) {
        ERROR(tout << "Communication with server: expected " << count << " bytes, but read " << bytesRead << " bytes");
        delete[] buffer;
        buffer = nullptr;
        return false;
    }
    if (!writeConfirmation()) {
        ERROR(tout << "Communication with server: I've got the message, but could not confirm it.");
        delete[] buffer;
        buffer = nullptr;
        return false;
    }
    return true;
}

bool Protocol::writeBuffer(char *buffer, int count) {
    if (!writeCount(count) || !readConfirmation()) {
        return false;
    }
    int bytesWritten = m_communicator.write(buffer, count);
    if (bytesWritten != count) {
        ERROR(tout << "Communication with server: could not sent the message. Instead sent " << bytesWritten << " bytes");
        return false;
    }
    if (!readConfirmation()) {
        ERROR(tout << "Communication with server: message sent, but no confirmation.");
        return false;
    }
    return true;
}

bool Protocol::handshake() {
    const char *expectedMessage = "Hi!";
    char *message;
    int count;
    if (readBuffer(message, count) && !strcmp(message, expectedMessage)) {
        strcpy(message, "Hi!");
        count = strlen(message);
        if (writeBuffer(message, count)) {
            delete[] message;
            LOG(tout << "Communication with server: handshake success!");
            return true;
        }
        delete[] message;
    }
    ERROR(tout << "Communication with server: handshake failed!");
    return false;
}

#ifndef INSTRUMENTATION
bool Protocol::sendProbes() {
    unsigned bytesCount = ProbesAddresses.size() * sizeof(unsigned long long);
    LOG(tout << "Sending probes..." << std::endl);
    return writeBuffer((char*)ProbesAddresses.data(), bytesCount);
}

bool Protocol::sendMethodBody(const MethodBodyInfo &body) {
    if (!writeBuffer(instrumentationCommandMessage, 1)) return false;
    LOG(tout << "Sending code (token = " << body.token << ")");
    unsigned messageLength = body.codeLength + 6 * sizeof(unsigned) + body.ehsLength + body.assemblyNameLength + body.moduleNameLength + body.signatureTokensLength;
    char *buffer = new char[messageLength];
    char *origBuffer = buffer;
    unsigned size = sizeof(unsigned);
    *(unsigned *)buffer = body.token; buffer += size;
    *(unsigned *)buffer = body.codeLength; buffer += size;
    *(unsigned *)buffer = body.assemblyNameLength; buffer += size;
    *(unsigned *)buffer = body.moduleNameLength; buffer += size;
    *(unsigned *)buffer = body.maxStackSize; buffer += size;
    *(unsigned *)buffer = body.signatureTokensLength;
    buffer += size; size = body.signatureTokensLength;
    memcpy(buffer, body.signatureTokens, size);
    buffer += size; size = body.assemblyNameLength;
    memcpy(buffer, (char*)body.assemblyName, size);
    buffer += size; size = body.moduleNameLength;
    memcpy(buffer, (char*)body.moduleName, size);
    buffer += size; size = body.codeLength;
    memcpy(buffer, body.bytecode, size);
    buffer += size; size = body.ehsLength;
    memcpy(buffer, body.ehs, size);
    bool result = writeBuffer(origBuffer, (int)messageLength);
    delete[] origBuffer;
    return result;
}

bool Protocol::acceptMethodBody(char *&bytecode, int &codeLength, unsigned &maxStackSize, char *&ehs, unsigned &ehsLength)
{
    char *message;
    int messageLength;
    if (!readBuffer(message, messageLength)) {
        ERROR(tout << "Reading instrumented method body failed!");
        return false;
    }
    char *origMessage = message;
    LOG(tout << "Successfully accepted " << messageLength << " bytes of message, parsing it...");
    codeLength = *(int*)message;
    message += sizeof(int);
    maxStackSize = *(unsigned*)message;
    message += sizeof(unsigned);
    bytecode = new char[codeLength];
    memcpy(bytecode, message, codeLength);
    ehsLength = messageLength - sizeof(int) - sizeof(unsigned) - codeLength;
    ehs = new char[ehsLength];
    memcpy(ehs, message + codeLength, ehsLength);
    delete[] origMessage;
    return true;
}
#endif

bool Protocol::connect() {
    LOG(tout << "Connecting to server...");
    return m_communicator.open() && handshake();
}

bool Protocol::shutdown()
{
    return writeCount(-1);
}
