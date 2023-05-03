#include "stack.h"
#include "..\logging.h"
#include <cstring>
#include <cassert>
#include <algorithm>

using namespace vsharp;

StackFrame::StackFrame(unsigned resolvedToken, unsigned unresolvedToken)
    : m_resolvedToken(resolvedToken)
    , m_unresolvedToken(unresolvedToken)
    , m_enteredMarker(false)
    , m_spontaneous(false)
    , m_ip(0)
{
}

StackFrame::~StackFrame()
= default;

unsigned StackFrame::resolvedToken() const
{
    return m_resolvedToken;
}

unsigned StackFrame::unresolvedToken() const
{
    return m_unresolvedToken;
}

void StackFrame::setResolvedToken(unsigned resolved)
{
    this->m_resolvedToken = resolved;
}

unsigned StackFrame::ip() const {
    return m_ip;
}

bool StackFrame::hasEntered() const
{
    return m_enteredMarker;
}

void StackFrame::setEnteredMarker(bool entered)
{
    this->m_enteredMarker = entered;
}

bool StackFrame::isSpontaneous() const
{
    return m_spontaneous;
}

void StackFrame::setSpontaneous(bool isUnmanaged)
{
    this->m_spontaneous = isUnmanaged;
}

unsigned StackFrame::moduleToken() const
{
    return m_moduleToken;
}

void StackFrame::setModuleToken(unsigned token)
{
    m_moduleToken = token;
}

void Stack::pushFrame(unsigned resolvedToken, unsigned unresolvedToken)
{
    m_frames.emplace_back(resolvedToken, unresolvedToken);
}


void Stack::popFrame()
{
    popFrameUntracked();
    if (m_frames.size() < m_minTopSinceLastSent) {
        m_minTopSinceLastSent = m_frames.size();
    }
}

void Stack::popFrameUntracked()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Stack is empty! Can't pop frame!");
    }
#endif
    m_frames.pop_back();
}

StackFrame &Stack::topFrame()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!")
    }
#endif
    return m_frames.back();
}

const StackFrame &Stack::topFrame() const
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!")
    }
#endif
    return m_frames.back();
}

StackFrame &Stack::frameAt(unsigned index) {
#ifdef _DEBUG
    if (index >= m_frames.size()) {
        FAIL_LOUD("Requesting too large frame number!")
    }
#endif
    return m_frames[index];
}

const StackFrame &Stack::frameAt(unsigned index) const {
#ifdef _DEBUG
    if (index >= m_frames.size()) {
        FAIL_LOUD("Requesting too large frame number!")
    }
#endif
    return m_frames[index];
}

bool Stack::isEmpty() const
{
    return m_frames.empty();
}

unsigned Stack::framesCount() const
{
    return m_frames.size();
}
