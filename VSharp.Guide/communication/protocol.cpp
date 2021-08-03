#include "communication/protocol.h"
#include "logging.h"
#include <iostream>

using namespace icsharp;

bool Protocol::connect() {
    LOG(tout << "Connection to server is not implemented, so ignoring it");
    return true;
}

bool Protocol::startSession(){
    return connect();
}

bool Protocol::shutdown()
{
    return true;
}
