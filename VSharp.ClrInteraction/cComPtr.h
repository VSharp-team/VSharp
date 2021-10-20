#ifndef CCOMPTR_H_
#define CCOMPTR_H_

template<class TInterface>
class CComPtr
{
private:
    TInterface* pointer;
public:
    CComPtr(const CComPtr&) = delete;
    CComPtr& operator= (const CComPtr&) = delete;
    CComPtr(CComPtr&&) = delete;
    CComPtr& operator= (CComPtr&&) = delete;

    void* operator new(std::size_t) = delete;
    void* operator new[](std::size_t) = delete;

    void operator delete(void *ptr) = delete;
    void operator delete[](void *ptr) = delete;

    CComPtr()
    {
        this->pointer = nullptr;
    }

    ~CComPtr()
    {
        if (this->pointer)
        {
            this->pointer->Release();
            this->pointer = nullptr;
        }
    }

    operator TInterface*()
    {
        return this->pointer;
    }

    operator TInterface*() const
    {
        return this->pointer;
    }

    TInterface& operator *()
    {
        return *this->pointer;
    }

    TInterface& operator *() const
    {
        return *this->pointer;
    }

    TInterface** operator&()
    {
        return &this->pointer;
    }

    TInterface** operator&() const
    {
        return &this->pointer;
    }

    TInterface* operator->()
    {
        return this->pointer;
    }

    TInterface* operator->() const
    {
        return this->pointer;
    }

    void Release()
    {
        this->~CComPtr();
    }
};

#endif // CCOMPTR_H_
