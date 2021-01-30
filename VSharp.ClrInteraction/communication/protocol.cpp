#include "communication/protocol.h"
#include "logging.h"

#include <cstring>
#include <iostream>

using namespace icsharp;

char confirmationByte = 0x55;
char *confirmationMessage = new char[1] {confirmationByte};
	
bool protocol::readConfirmation() {
	char *buffer = new char[1];
	int bytesRead = communicator.read(buffer, 1);
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

bool protocol::writeConfirmation() {
	int bytesWritten = communicator.write(confirmationMessage, 1);
	if (bytesWritten != 1) {
		ERROR(tout << "Communication with server: could not send the confirmation message. Instead sent"
				   << bytesWritten << " bytes.");
		return false;
	}
	return true;
}

bool protocol::readCount(int &count) {
    int countCount = communicator.read((char*)(&count), 4);
    if (countCount != 4) {
        ERROR(tout << "Communication with server: could not get the amount of bytes of the next message. Instead read " << countCount << " bytes");
        return false;
    }

    return true;
}

bool protocol::writeCount(int count) {
	int bytesWritten = communicator.write((char*)(&count), 4);
	if (bytesWritten != 4) {
        ERROR(tout << "Communication with server: could not sent the amount of bytes of the next message. Instead sent " << bytesWritten << " bytes");
		return false;
	}
	return true;
}

bool protocol::readBuffer(char *&buffer, int &count) {
	if (!readCount(count)) {
		return false;
	}
	if (count <= 0) {
        ERROR(tout << "Communication with server: the amount of bytes is unexpectedly non-positive (count = " << count << ") ");
        return false;
	}
	if (!writeConfirmation()) return false;
	buffer = new char[count];
	int bytesRead = communicator.read(buffer, count);
	if (bytesRead != count) {
		ERROR(tout << "Communication with server: expected " << count << " bytes, but read " << bytesRead << " bytes");
		delete[] buffer;
		buffer = nullptr;
		return false;
	}
	if (!writeConfirmation()) {
		ERROR(tout << "Communication with server: I've got the message, but could not confirm it.");
		return false;		
	}
	return true;
}

bool protocol::writeBuffer(char *buffer, int count) {
	if (!writeCount(count) || !readConfirmation()) {
		return false;
	}
    int bytesWritten = communicator.write(buffer, count);
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

bool protocol::handshake() {
	const char *expectedMessage = "Hi!";
	char *message;
	int count;
	if (readBuffer(message, count) && !strcmp(message, expectedMessage)) {
		strcpy(message, "Hi!");
		count = strlen(message);
		if (writeBuffer(message, count)) {
			LOG(tout << "Communication with server: handshake success!");
			return true;
		}
	}
	ERROR(tout << "Communication with server: handshake failed!");
	return false;
}

bool protocol::sendMethodBody(char *bytecode, int length) {
	return writeBuffer(bytecode, length);
}

bool protocol::connect() {
	LOG(tout << "Connecting to server...");
	return communicator.open() && handshake();
}
