#ifndef PROTOCOL_H_
#define PROTOCOL_H_

#include "communication/communicator.h"

namespace icsharp {

struct MethodBodyInfo {
    unsigned token;
    unsigned codeLength;
    unsigned assemblyNameLength;
    unsigned moduleNameLength;
    unsigned maxStackSize;
    unsigned ehsLength;
    unsigned signatureTokensLength;
    char *signatureTokens;
    char16_t *assemblyName;
    char16_t *moduleName;
    char *bytecode;
    char *ehs;
};

class Protocol {
private:
    Communicator m_communicator;

    bool readConfirmation();
    bool writeConfirmation();

    bool readCount(int &count);
    bool writeCount(int count);

    bool readBuffer(char *&buffer, int &count);
    bool writeBuffer(char *buffer, int count);

    bool handshake();

public:
    bool connect();
    bool sendProbes();
    bool sendMethodBody(const MethodBodyInfo &body);
    bool acceptMethodBody(char *&bytecode, int &codeLength, unsigned &maxStackSize, char *&ehs, unsigned &ehsLength);
    bool shutdown();
};

}

#endif // PROTOCOL_H_
