#ifndef STACK_H_
#define STACK_H_

#include <vector>
#include <stack>

namespace vsharp {

#define OFFSET UINT32

class StackFrame {
private:
    unsigned m_resolvedToken;
    unsigned m_unresolvedToken;
    unsigned m_moduleToken;
    bool m_enteredMarker;
    bool m_spontaneous;

    unsigned m_ip;

public:
    StackFrame(unsigned resolvedToken, unsigned unresolvedToken);
    ~StackFrame();

    unsigned resolvedToken() const;
    unsigned unresolvedToken() const;
    void setResolvedToken(unsigned resolved);
    unsigned ip() const;
    bool hasEntered() const;
    void setEnteredMarker(bool entered);
    bool isSpontaneous() const;
    void setSpontaneous(bool isUnmanaged);

    unsigned moduleToken() const;
    void setModuleToken(unsigned token);
};

class Stack {
private:
    std::deque<StackFrame> m_frames;
    unsigned m_lastSentTop;
    unsigned m_minTopSinceLastSent;

public:

    void pushFrame(unsigned resolvedToken, unsigned unresolvedToken);
    void popFrame();
    void popFrameUntracked();
    StackFrame &topFrame();
    inline const StackFrame &topFrame() const;
    StackFrame &frameAt(unsigned index);
    inline const StackFrame &frameAt(unsigned index) const;

    bool isEmpty() const;
    unsigned framesCount() const;
};

}

#endif // STACK_H_
