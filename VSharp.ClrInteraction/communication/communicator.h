#ifndef COMMUNICATOR_H_
#define COMMUNICATOR_H_

namespace vsharp {

class Communicator {
public:
    bool open();
    int read(char *buffer, int count);
    int write(char *message, int count);
    bool close();
};

}

#endif // COMMUNICATOR_H_
