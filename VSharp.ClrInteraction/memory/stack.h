#ifndef STACK_H_
#define STACK_H_

#include <vector>
#include <stack>
#include "storage.h"

namespace vsharp {

#define OFFSET UINT32

// NOTE: every stack cell (evaluation stack cell, local or argument) contains LocalObject class by value
struct StackCell {
    unsigned content;
    LocalObject cell;
};

class StackFrame {
private:
    StackCell *m_concreteness;
    unsigned m_capacity;
    unsigned m_concretenessTop;

    unsigned m_symbolsCount;
    unsigned m_lastSentSymbolsCount;
    unsigned m_minSymbsCountSinceLastSent;

    LocalObject *m_args;
    unsigned m_argsCount;
    LocalObject *m_locals;
    unsigned m_localsCount;
    // NOTE: used to delete from heap all stack cell, which were allocated there
    std::vector<Interval *> allocatedLocals;

    unsigned m_resolvedToken;
    unsigned m_unresolvedToken;
    unsigned m_moduleToken;
    bool m_enteredMarker;
    bool m_spontaneous;

    unsigned m_ip;

    Storage &m_heap;

    std::vector<std::pair<unsigned, unsigned>> m_lastPoppedSymbolics;

public:
    StackFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount, Storage &heap);
    ~StackFrame();

    void configure(unsigned maxStackSize, unsigned localsCount);

    inline bool isEmpty() const;
    inline bool isFull() const;

    bool peekConcreteness(unsigned idx) const;
    bool peek0() const;
    bool peek1() const;
    bool peek2() const;
    LocalObject &peekObject(unsigned idx);
    const LocalObject &peekObject(unsigned idx) const;

    void pop0();
    bool pop1();
    bool pop(unsigned count);
    void pop1Async(); // Does not track the popped symbolics, but tracks the total amount of such pops.

    void push1(const LocalObject &obj);
    void pushPrimitive(bool isConcrete);
    void push1Concrete();

    LocalObject &arg(unsigned index) const;
    LocalObject &loc(unsigned index) const;

    void addAllocatedLocal(LocalObject *local);

    bool dup();

    unsigned count() const;

    unsigned resolvedToken() const;
    unsigned unresolvedToken() const;
    void setResolvedToken(unsigned resolved);
    unsigned ip() const;
    void setIp(unsigned ip);
    bool hasEntered() const;
    void setEnteredMarker(bool entered);
    bool isSpontaneous() const;
    void setSpontaneous(bool isUnmanaged);

    unsigned moduleToken() const;
    void setModuleToken(unsigned token);

    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbolics() const;
    unsigned evaluationStackPops() const;
    unsigned symbolicsCount() const;
    void resetPopsTracking();
};

class Stack {
private:
    std::deque<StackFrame> m_frames;
    unsigned m_lastSentTop;
    unsigned m_minTopSinceLastSent;

    Storage &m_heap;

public:
    struct OperandMem {
    private:
        const StackFrame &m_frame;
        OFFSET m_offset;
        unsigned m_entries_count;
        unsigned m_data_ptr;
        std::vector<char> m_data;
        std::vector<unsigned> m_dataPtrs;

        int m_memSize = 0;

        INT_PTR m_refLikeStructRef;

    public:
        OperandMem(const StackFrame &frame, OFFSET offset);

        const StackFrame &stackFrame() const { return m_frame; }
        OFFSET offset() const { return m_offset; }
        INT8 entriesCount() { return m_entries_count; }

        void mem(char *value, CorElementType t, size_t size, INT8 idx);
        void mem(char *value, CorElementType t, size_t size);
        void update(char *value, size_t size, INT8 idx);

        void mem_i1(INT8 value);
        void mem_i1(INT8 value, INT8 idx);
        void mem_i2(INT16 value);
        void mem_i2(INT16 value, INT8 idx);
        void mem_i4(INT32 value);
        void mem_i4(INT32 value, INT8 idx);
        void mem_i8(INT64 value);
        void mem_i8(INT64 value, INT8 idx);
        void mem_f4(FLOAT value);
        void mem_f4(FLOAT value, INT8 idx);
        void mem_f8(DOUBLE value);
        void mem_f8(DOUBLE value, INT8 idx);
        void mem_p(INT_PTR value);
        void mem_p(INT_PTR value, INT8 idx);
        void mem_refLikeStruct(INT_PTR ref);
        void update_i1(INT8 value, INT8 idx);
        void update_i2(INT16 value, INT8 idx);
        void update_i4(INT32 value, INT8 idx);
        void update_i8(INT64 value, INT8 idx);
        void update_f4(long long value, INT8 idx);
        void update_f8(long long value, INT8 idx);
        void update_p(INT_PTR value, INT8 idx);
        CorElementType unmemType(INT8 idx) const;
        INT8 unmem_i1(INT8 idx) const;
        INT16 unmem_i2(INT8 idx) const;
        INT32 unmem_i4(INT8 idx) const;
        INT64 unmem_i8(INT8 idx) const;
        FLOAT unmem_f4(INT8 idx) const;
        DOUBLE unmem_f8(INT8 idx) const;
        INT_PTR unmem_p(INT8 idx) const;
        INT_PTR unmem_refLikeStruct() const;
    };

private:
    std::deque<OperandMem> m_opmem;

public:
    explicit Stack(Storage &heap);

    void pushFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount);
    void popFrame();
    void popFrameUntracked();
    StackFrame &topFrame();
    inline const StackFrame &topFrame() const;
    StackFrame &frameAt(unsigned index);
    inline const StackFrame &frameAt(unsigned index) const;

    bool isEmpty() const;
    unsigned framesCount() const;
    unsigned tokenAt(unsigned index) const;
    unsigned offsetAt(unsigned index) const;

    unsigned unsentPops() const;
    unsigned minTopSinceLastSent() const;
    void resetMinTop();
    void resetPopsTracking(int framesCount);

    bool opmemIsEmpty() const;
    OperandMem &opmem(OFFSET offset);
    const OperandMem &lastOpmem() const;
    void popOpmem();
};

}

#endif // STACK_H_
