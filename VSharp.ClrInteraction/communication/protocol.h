#ifndef PROTOCOL_H_
#define PROTOCOL_H_

#include "communicator.h"
#include "../memory/storage.h"
#include "cor.h"
#include <vector>

#ifdef UNIX
#include "pal_mstypes.h"
#include "corhdr.h"
#endif

#ifdef WIN32
#include "../profiler_win.h"
#endif

namespace vsharp {

enum CommandType {
    Confirmation = 0x55,
    InstrumentCommand = 0x56,
    ExecuteCommand = 0x57,
    ReadMethodBody = 0x58,
    ReadString = 0x59,
    ParseTypeInfoFromMethod = 0x60,
    GetTypeTokenFromTypeRef = 0x61,
    GetTypeTokenFromTypeSpec = 0x62,
    ReadHeapBytes = 0x63,
    ReadExecResponse = 0x64,
    Unmarshall = 0x65,
    ReadWholeObject = 0x66,
    ParseFieldRefTypeToken = 0x67,
    ParseFieldDefTypeToken = 0x69,
    ParseArgTypeToken = 0x70,
    ParseLocalTypeToken = 0x71
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
    bool startSession();
    void acceptEntryPoint(char *&entryPointBytes, int &length);
    bool acceptCommand(CommandType &command);
    bool acceptString(char *&string);
    bool acceptWString(WCHAR *&string);
    bool acceptToken(mdToken &token);
    bool acceptInt32(INT32 &value);
    bool acceptTokenAndInt32(mdToken &token, INT32 &value);
    bool accept2Tokens(mdToken &token1, mdToken &token2);
    bool acceptReadObjectParameters(OBJID &objID, bool &isArray, int &refOffsetsLength, int *&refOffsets);
    bool acceptHeapReadingParameters(VirtualAddress &address, INT32 &size, BYTE &isRef);
    bool sendToken(mdToken token);
    bool sendBytes(char *bytes, int size);
    bool sendStringsPoolIndex(unsigned index);
    bool sendTypeInfoFromMethod(const std::vector<mdToken>& types);
    bool acceptMethodBody(char *&bytecode, int &codeLength, unsigned &maxStackSize, char *&ehs, unsigned &ehsLength);
    template<typename T>
    bool sendSerializable(char commandByte, const T &object) {
        if (!writeBuffer(new char[1] {commandByte}, 1)) return false;
        char *bytes;
        unsigned count;
        object.serialize(bytes, count);
        bool result = writeBuffer(bytes, count);
        delete[] bytes;
        return result;
    }
    void acceptExecResult(char *&bytes, int &messageLength);
    bool shutdown();
};

}

#endif // PROTOCOL_H_
