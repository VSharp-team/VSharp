#ifndef PROTOCOL_H_
#define PROTOCOL_H_

namespace icsharp {

class Protocol {

public:
    bool connect();
    bool startSession();
    bool shutdown();
};

}

#endif // PROTOCOL_H_
