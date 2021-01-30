#ifndef PROTOCOL_H_
#define PROTOCOL_H_

#include "communication/communicator.h"

namespace icsharp {

class protocol {
private:
	communicator communicator;

	bool readConfirmation();
	bool writeConfirmation();

	bool readCount(int &count);
	bool writeCount(int count);

	bool readBuffer(char *&buffer, int &count);
	bool writeBuffer(char *buffer, int count);

	bool handshake();

public:
	bool connect();
	bool sendMethodBody(char *bytecode, int length);
	bool acceptMethodBody(char *&bytecode, int &length);
};

}

#endif // PROTOCOL_H_