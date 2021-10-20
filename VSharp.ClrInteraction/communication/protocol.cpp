#include "communication/protocol.h"
#include "logging.h"
#include "probes.h"

#include <cstring>
#include <iostream>

using namespace icsharp;

bool Protocol::readConfirmation() {
    char *buffer = new char[1];
    int bytesRead = m_communicator.read(buffer, 1);
    if (bytesRead != 1 || buffer[0] != Confirmation) {
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
    int bytesWritten = m_communicator.write(new char[1] {Confirmation}, 1);
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

bool Protocol::startSession() {
    return connect() && sendProbes();
}

bool Protocol::sendProbes() {
    unsigned bytesCount = ProbesAddresses.size() * sizeof(unsigned long long);
    LOG(tout << "Sending probes..." << std::endl);
    return writeBuffer((char*)ProbesAddresses.data(), bytesCount);
}

bool Protocol::acceptCommand(CommandType &command)
{
    char *message;
    int messageLength;
    if (!readBuffer(message, messageLength)) {
        ERROR(tout << "Reading command failed!");
        return false;
    }
    command = (CommandType) *message;
//    CLOG(command == ReadMethodBody, tout << "Accepted ReadMethodBody command");
//    CLOG(command == ReadString, tout << "Accepted ReadString command");
    delete[] message;
    return true;
}

bool Protocol::acceptString(char *&string) {
    char *message;
    int messageLength;
    if (!readBuffer(message, messageLength)) {
        ERROR(tout << "Reading instrumented method body failed!");
        return false;
    }
    string = new char[messageLength + 2];
    memcpy(string, message, messageLength);
    string[messageLength] = '\0';
    string[messageLength + 1] = '\0';
//    LOG(tout << "Successfully accepted string: " << string);
    delete[] message;
    return true;
}

bool Protocol::sendStringsPoolIndex(const unsigned index) {
    unsigned messageLength = sizeof(unsigned);
    char *buffer = new char[messageLength];
    *(unsigned *)buffer = index;
    bool result = writeBuffer(buffer, (int)messageLength);
    delete[] buffer;
    return result;
}

bool Protocol::acceptMethodBody(char *&bytecode, int &codeLength, unsigned &maxStackSize, char *&ehs, unsigned &ehsLength) {
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

bool Protocol::waitExecResult(char *&message, int &messageLength) {
    if (!readBuffer(message, messageLength)) {
        FAIL_LOUD("Exec responce validation failed!");
    }
    return true;
}

bool Protocol::connect() {
    LOG(tout << "Connecting to server...");
    return m_communicator.open() && handshake();
}

bool Protocol::shutdown()
{
    return writeCount(-1);
}
