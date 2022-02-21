#ifndef INTERVALTREE_H_
#define INTERVALTREE_H_

#include "../logging.h"
#include <cassert>

template<typename Interval, typename Shift, typename Point>
class IntervalTree {
private:
    std::vector<Interval *> objects;
public:
    void add(Interval &node) {
        objects.push_back(&node);
    }

    const Interval *find(const Point &p) const {
        for (const Interval *obj : objects) {
            if (obj->contains(p))
                return obj;
        }
        FAIL_LOUD("Unbound pointer!");
    }

    void moveAndMark(const Interval &interval, const Shift &shift) {
        for (Interval *obj : objects) {
            if (interval.includes(*obj)) {
                obj->move(shift);
                obj->mark();
            } else {
                assert(!interval.intersects(*obj));
            }
        }
    }

    void mark(const Interval &interval) {
        for (Interval *obj : objects) {
            if (interval.includes(*obj))
                obj->mark();
            else
                assert(!interval.intersects(*obj));
        }
    }

    // TODO: copy all marked and clear or remove unmarked one by one?
    std::vector<Interval *> clearUnmarked() {
        std::vector<Interval *> marked;
        std::vector<Interval *> unmarked;
        for (Interval *obj : objects)
            if (obj->isMarked()) {
                obj->unmark();
                marked.push_back(obj);
            } else {
                unmarked.push_back(obj);
                delete obj;
            }
        objects = marked;
        return unmarked;
    }

    std::vector<Interval*> flush() {
        std::vector<Interval*> newAddresses;
        for (Interval *obj : objects)
            if (!obj->isFlushed()) {
                newAddresses.push_back(obj);
                obj->flush();
            }
        return newAddresses;
    }

    std::string dumpObjects() const {
        std::string dump;
        for (const Interval *obj : objects)
            dump += obj->toString() + "\n";
        return dump;
    }
};

#endif // INTERVALTREE_H_
