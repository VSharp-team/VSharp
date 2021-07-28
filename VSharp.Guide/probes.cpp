#include <fstream>
#include "probes.h"

using namespace icsharp;

std::map<unsigned, std::pair<unsigned, int>> environment;
unsigned currentEntryNumber = 0;

void icsharp::InitializeProbes() {
    std::ifstream infile("environment.txt");
    int methodToken, value;
    int i = 0;
    while (infile >> methodToken >> value)
    {
        environment[i++] = {methodToken, value};
    }
}

int ConsoleRead() {
    unsigned methodToken;
    int value;
    std::tie(methodToken, value) = environment[currentEntryNumber++];
    assert(methodToken == 6000077);
    return value;
}

int (*icsharp::ConsoleReadAddress)() = &ConsoleRead;
