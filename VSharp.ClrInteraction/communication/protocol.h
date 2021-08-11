#ifndef PROTOCOL_H_
#define PROTOCOL_H_

#include "communication/communicator.h"

namespace icsharp {

enum CommandType {ReadMethodBody = 58, ReadString = 59};

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

enum EvalStackArgType {
    OpSymbolic = 1,
    OpI4 = 2,
    OpI8 = 3,
    OpR4 = 4,
    OpR8 = 5,
    OpRef = 6
};

struct EvalStackOperand {
    EvalStackArgType typ;
    long long content; // TODO: struct?
};

struct ExecCommand {
    unsigned offset;
    unsigned isBranch;
    unsigned newCallStackFramesCount;
    unsigned callStackFramesPops;
    unsigned evaluationStackPushesCount;
    unsigned evaluationStackPops;
    unsigned *newCallStackFrames;
    EvalStackOperand *evaluationStackPushes;
    // TODO: 2misha: put here allocated and moved objects
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
    bool acceptCommand(CommandType &command);
    bool acceptString(char *&string);
    bool sendStringsPoolIndex(unsigned index);
    bool sendMethodBody(const MethodBodyInfo &body);
    bool acceptMethodBody(char *&bytecode, int &codeLength, unsigned &maxStackSize, char *&ehs, unsigned &ehsLength);
    bool sendExecCommand(const ExecCommand &command);
    void waitExecResult();
    bool waitExecBranchResult();
    bool shutdown();
};

}

#endif // PROTOCOL_H_
